use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    register::*,
};
use super::{builder::*, function::*, inst::*, liveness::*, module::*, spiller::Spiller};
use crate::ir::types::Types;
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
            self.run_on_function(&module.types, func);
        }
    }

    pub fn run_on_function(&mut self, tys: &Types, cur_func: &mut MachineFunction) {
        let mut matrix = LivenessAnalysis::new().analyze_function(cur_func);

        self.preserve_vreg_uses_across_call(tys, cur_func, &mut matrix);

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
            // let reg_to_spill = *interfering.last().unwrap();
            let reg_to_spill = matrix
                // TODO: spill weight is coming soon
                .pick_assigned_and_longest_lived_vreg(&interfering)
                .unwrap();
            let phy_reg = matrix.unassign_reg(reg_to_spill).unwrap();
            matrix.assign_reg(vreg, phy_reg);

            let new_regs = Spiller::new(cur_func, &mut matrix).spill(tys, reg_to_spill);
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

        self.rewrite_vregs(cur_func, &matrix);
    }

    fn rewrite_vregs(&mut self, cur_func: &mut MachineFunction, matrix: &LiveRegMatrix) {
        for (_, bb) in cur_func.body.basic_blocks.id_and_block() {
            for inst_id in &*bb.iseq_ref() {
                let inst = &cur_func.body.inst_arena[*inst_id];
                for def in &inst.def {
                    if def.get_reg().is_some() {
                        continue;
                    }

                    let reg = matrix
                        .get_vreg_interval(def.get_vreg())
                        .unwrap()
                        .reg
                        .unwrap();
                    def.set_phy_reg(reg);
                }
            }
        }
    }

    fn insert_inst_to_save_reg(
        &mut self,
        tys: &Types,
        cur_func: &mut MachineFunction,
        matrix: &mut LiveRegMatrix,
        occupied: &mut FxHashSet<FrameIndexKind>,
        call_inst_id: MachineInstId,
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

        let call_inst_pp = matrix.get_program_point(call_inst_id).unwrap();
        let mut regs_to_save = FxHashSet::default();

        // TODO: It's expensive to check all the elements in ``inst_arena``
        // IMPROVED A LITTLE: any better ideas?
        {
            let bb_including_call =
                &cur_func.body.basic_blocks.arena[cur_func.body.inst_arena[call_inst_id].parent];
            let liveness = bb_including_call.liveness_ref();
            let regs_that_may_interfere = &liveness.def | &liveness.live_in;
            for r in &regs_that_may_interfere {
                if matrix.interferes_with_range(
                    r.get_vreg(),
                    LiveRange::new(vec![LiveSegment::new(call_inst_pp, call_inst_pp)]),
                ) {
                    regs_to_save.insert(r.clone());
                }
            }
        }

        // debug!(println!("REG TO SAVE: {:?}", regs_to_save));

        let mut slots_to_save_regs = vec![];
        for r in &regs_to_save {
            slots_to_save_regs.push(find_unused_slot(cur_func, occupied, r));
        }

        // debug!(println!("NEW SLOTS: {:?}", slots_to_save_regs));

        let call_inst_parent = cur_func.body.inst_arena[call_inst_id].parent;

        for (frinfo, reg) in slots_to_save_regs.into_iter().zip(regs_to_save.iter()) {
            let dst = MachineOperand::FrameIndex(frinfo.clone());
            let src = MachineOperand::Register(reg.clone());
            let rbp = MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            );
            let store_inst_id = cur_func.body.inst_arena.alloc(MachineInst::new(
                &cur_func.vreg_gen,
                mov_mx(&src).unwrap(),
                vec![rbp, dst, MachineOperand::None, MachineOperand::None, src],
                None,
                call_inst_parent,
            ));
            cur_func.body.inst_arena[store_inst_id].add_use(store_inst_id);
            cur_func.body.inst_arena[store_inst_id].add_def(store_inst_id);

            let src = MachineOperand::FrameIndex(frinfo);
            let rbp = MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            );

            let load_inst_id = cur_func.body.inst_arena.alloc(
                MachineInst::new_simple(
                    mov_rx(tys, &src).unwrap(),
                    vec![rbp, src, MachineOperand::None, MachineOperand::None],
                    call_inst_parent,
                )
                .with_def(vec![reg.clone()]),
            );
            cur_func.body.inst_arena[load_inst_id].add_def(load_inst_id); // TODO: is this needed?

            let mut builder = BuilderWithLiveInfoEdit::new(matrix, cur_func);

            builder.set_insert_point_before_inst(call_inst_id);
            builder.insert(store_inst_id);

            builder.set_insert_point_after_inst(call_inst_id);
            builder.insert(load_inst_id);
        }
    }

    fn preserve_vreg_uses_across_call(
        &mut self,
        tys: &Types,
        cur_func: &mut MachineFunction,
        matrix: &mut LiveRegMatrix,
    ) {
        let mut call_inst_id = vec![];

        for (_, bb) in cur_func.body.basic_blocks.id_and_block() {
            for inst_id in bb.iseq_ref().iter() {
                let inst = &cur_func.body.inst_arena[*inst_id];
                if inst.opcode == MachineOpcode::Call {
                    call_inst_id.push(*inst_id)
                }
            }
        }

        let occupied = cur_func
            .local_mgr
            .locals
            .iter()
            .map(|l| l.idx)
            .collect::<FxHashSet<_>>();

        for inst_id in call_inst_id {
            self.insert_inst_to_save_reg(tys, cur_func, matrix, &mut occupied.clone(), inst_id);
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
