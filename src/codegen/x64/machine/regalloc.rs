use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    register::*,
};
use super::{builder::*, function::*, instr::*, liveness::*, module::*, spiller::Spiller};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub struct RegisterAllocator {
    queue: VecDeque<VirtReg>,
}

pub struct AllocationOrder<'a> {
    matrix: &'a LiveRegMatrix,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(func);
        }
    }

    pub fn run_on_function(&mut self, cur_func: &mut MachineFunction) {
        let mut matrix = LivenessAnalysis::new().analyze_function(cur_func);

        self.preserve_vreg_uses_across_call(cur_func, &mut matrix);

        fn sort_queue(mut queue: Vec<VirtReg>, matrix: &LiveRegMatrix) -> Vec<VirtReg> {
            queue.sort_by(|x, y| {
                let x = matrix.get_vreg_interval(*x).unwrap().start_point().unwrap();
                let y = matrix.get_vreg_interval(*y).unwrap().start_point().unwrap();
                x.cmp(&y)
            });
            queue
        }

        // TODO: Is this correct?
        self.queue = sort_queue(matrix.collect_vregs(), &matrix)
            .into_iter()
            .collect();

        while let Some(vreg) = self.queue.pop_front() {
            let mut allocated = false;
            let order = AllocationOrder::new(&matrix).get_order(vreg).unwrap();
            for reg in order {
                if matrix.interferes(vreg, reg) {
                    continue;
                }

                matrix.assign_reg(vreg, reg);

                allocated = true;
                break;
            }

            if allocated {
                continue;
            }

            let interfering = matrix.collect_interfering_vregs(vreg);
            let reg_to_spill = matrix
                // TODO: spill weight is coming soon
                .pick_assigned_and_longest_lived_vreg(&interfering)
                .unwrap();
            let phy_reg = matrix.unassign_reg(reg_to_spill).unwrap();
            matrix.assign_reg(vreg, phy_reg);

            let new_regs = Spiller::new(cur_func, &mut matrix).spill(reg_to_spill);
            self.queue.push_back(reg_to_spill);
            for new_reg in new_regs {
                self.queue.push_back(new_reg);
            }

            debug!(
                println!("interfering({:?}): {:?}", vreg, interfering);
                println!(
                    "spill target: {:?}",reg_to_spill
            ));
        }

        // debug!(for (_, x) in &matrix.vreg_interval {
        //     println!("{:?}", x)
        // });

        debug!(println!("{:?}", cur_func));

        self.rewrite_vregs(cur_func, &matrix);
    }

    fn rewrite_vregs(&mut self, cur_func: &mut MachineFunction, matrix: &LiveRegMatrix) {
        for (_, bb) in cur_func.basic_blocks.id_and_block() {
            for instr_id in &*bb.iseq_ref() {
                let instr = &cur_func.instr_arena[*instr_id];
                for def in &instr.def {
                    if def.get_reg().is_some() {
                        continue;
                    }

                    let reg = matrix
                        .get_vreg_interval(def.get_vreg())
                        .unwrap()
                        .reg
                        .unwrap();
                    // if phys reg is not assigned
                    if def.get_reg().is_none() {
                        def.set_phy_reg(reg);
                    }
                }
            }
        }
    }

    fn insert_instr_to_save_reg(
        &mut self,
        cur_func: &mut MachineFunction,
        matrix: &mut LiveRegMatrix,
        occupied: &mut FxHashSet<FrameIndexKind>,
        call_instr_id: MachineInstrId,
    ) {
        // TODO: Refine code. It's hard to understand.
        fn find_unused_slot(
            cur_func: &mut MachineFunction,
            occupied: &mut FxHashSet<FrameIndexKind>,
            r: &MachineRegister,
        ) -> FrameIndexInfo {
            for slot in &*cur_func.local_mgr.locals {
                if occupied.contains(&slot.idx) {
                    continue;
                }
                if Some(r.info_ref().reg_class) == ty2rc(&slot.ty) {
                    occupied.insert(slot.idx);
                    return slot.clone();
                }
            }
            let slot = cur_func.local_mgr.alloc(&rc2ty(r.info_ref().reg_class));
            occupied.insert(slot.idx);
            slot
        }

        let call_instr_pp = matrix.get_program_point(call_instr_id).unwrap();
        let mut regs_to_save = FxHashSet::default();

        // TODO: It's expensive to check all the elements in ``instr_arena``
        for (_, instr) in &cur_func.instr_arena.arena {
            if instr.def.len() == 0 {
                continue;
            }

            if matrix.interferes_with_range(
                instr.get_vreg(),
                LiveRange::new(vec![LiveSegment::new(call_instr_pp, call_instr_pp)]),
            ) {
                regs_to_save.insert(instr.def[0].clone());
            }
        }

        debug!(println!("REG TO SAVE: {:?}", regs_to_save));

        let mut slots_to_save_regs = vec![];
        for r in &regs_to_save {
            slots_to_save_regs.push(find_unused_slot(cur_func, occupied, r));
        }

        debug!(println!("NEW SLOTS: {:?}", slots_to_save_regs));

        let call_instr_parent = cur_func.instr_arena[call_instr_id].parent;

        for (frinfo, reg) in slots_to_save_regs.into_iter().zip(regs_to_save.iter()) {
            let dst = MachineOperand::FrameIndex(frinfo.clone());
            let src = MachineOperand::Register(reg.clone());
            let rbp = MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            );
            let store_instr_id = cur_func.instr_arena.alloc(MachineInstr::new(
                &cur_func.vreg_gen,
                mov_mx(&src).unwrap(),
                vec![rbp, dst, MachineOperand::None, MachineOperand::None, src],
                None,
                call_instr_parent,
            ));
            cur_func.instr_arena[store_instr_id].add_use(store_instr_id);
            cur_func.instr_arena[store_instr_id].add_def(store_instr_id);

            let src = MachineOperand::FrameIndex(frinfo);
            let rbp = MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            );

            let load_instr_id = cur_func.instr_arena.alloc(
                MachineInstr::new_simple(
                    mov_rx(&src).unwrap(),
                    vec![rbp, src, MachineOperand::None, MachineOperand::None],
                    call_instr_parent,
                )
                .with_def(vec![reg.clone()]),
            );
            cur_func.instr_arena[load_instr_id].add_def(load_instr_id); // TODO: is this needed?

            let mut builder = BuilderWithLiveInfoEdit::new(matrix, cur_func);

            builder.set_insert_point_before_instr(call_instr_id);
            builder.insert(store_instr_id);

            builder.set_insert_point_after_instr(call_instr_id);
            builder.insert(load_instr_id);
        }
    }

    fn preserve_vreg_uses_across_call(
        &mut self,
        cur_func: &mut MachineFunction,
        matrix: &mut LiveRegMatrix,
    ) {
        let mut call_instr_id = vec![];

        for (_, bb) in cur_func.basic_blocks.id_and_block() {
            for instr_id in bb.iseq_ref().iter() {
                let instr = &cur_func.instr_arena[*instr_id];
                if instr.opcode == MachineOpcode::Call {
                    call_instr_id.push(*instr_id)
                }
            }
        }

        let occupied = cur_func
            .local_mgr
            .locals
            .iter()
            .map(|l| l.idx)
            .collect::<FxHashSet<_>>();

        for instr_id in call_instr_id {
            self.insert_instr_to_save_reg(cur_func, matrix, &mut occupied.clone(), instr_id);
        }
    }
}

// TODO: Take into consideration the use list of register
impl<'a> AllocationOrder<'a> {
    pub fn new(matrix: &'a LiveRegMatrix) -> Self {
        Self { matrix }
    }

    pub fn get_order(&self, vreg: VirtReg) -> Option<RegisterOrder> {
        Some(
            self.matrix
                .get_entity_by_vreg(vreg)?
                .get_reg_class()
                .get_reg_order(),
        )
    }
}
