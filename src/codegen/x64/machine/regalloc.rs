use super::super::frame_object::*;
use super::super::register::*;
use super::{builder::*, function::*, instr::*, liveness::*, module::*, spiller::Spiller};
use crate::ir::types::*;
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub struct RegisterAllocator {
    queue: VecDeque<VirtReg>,
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

        self.queue = matrix.collect_vregs().into_iter().collect();

        while let Some(vreg) = self.queue.pop_front() {
            // TODO: 0..8 ???
            let mut allocated = false;
            for reg in 0..6 {
                let reg = get_general_reg(reg).unwrap();

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
            debug!(
                println!("MachineModule dump:");
                let mut idx = 0;
                for bb_id in &cur_func.basic_blocks {
                    let bb = &cur_func.basic_block_arena[*bb_id];
                    println!("Machine basic block: {:?}", bb);
                    for instr in &*bb.iseq_ref() {
                        println!("{}: {:?}", idx, cur_func.instr_arena[*instr]);
                        idx += 1;
                    }
                    println!()
                }
            );

            // unimplemented!("spilling");
        }

        debug!(for (_, x) in &matrix.vreg_interval {
            println!("{:?}", x)
        });

        self.rewrite_vregs(cur_func, &matrix);
    }

    fn rewrite_vregs(&mut self, cur_func: &mut MachineFunction, matrix: &LiveRegMatrix) {
        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
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
        occupied: &mut FxHashSet<i32>,
        call_instr_id: MachineInstrId,
    ) {
        // TODO: Refine code. It's hard to understand.
        fn find_unused_slot(
            cur_func: &mut MachineFunction,
            occupied: &mut FxHashSet</*idx=*/ i32>,
            r: &MachineRegister,
        ) -> FrameIndexInfo {
            for slot in &*cur_func.local_mgr.locals {
                if occupied.contains(&slot.idx) {
                    continue;
                }
                if r.info_ref().ty == slot.ty {
                    occupied.insert(slot.idx);
                    return slot.clone();
                }
            }
            let slot = cur_func.local_mgr.alloc(&r.info_ref().ty);
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
                LiveRange::new(vec![LiveSegment::new(
                    call_instr_pp,
                    call_instr_pp.next_idx(),
                )]),
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
            let store_instr_id = cur_func.instr_arena.alloc(MachineInstr::new(
                &cur_func.vreg_gen,
                MachineOpcode::Store,
                vec![
                    MachineOperand::FrameIndex(frinfo.clone()),
                    MachineOperand::Register(reg.clone()),
                ],
                &Type::Void,
                call_instr_parent,
            ));
            cur_func.instr_arena[store_instr_id].add_use(store_instr_id);
            cur_func.instr_arena[store_instr_id].add_def(store_instr_id);

            let load_instr_id = cur_func.instr_arena.alloc(
                MachineInstr::new_simple(
                    MachineOpcode::Load,
                    vec![MachineOperand::FrameIndex(frinfo)],
                    call_instr_parent,
                )
                .with_def(vec![reg.clone()]),
            );
            cur_func.instr_arena[load_instr_id].add_def(load_instr_id); // TODO: is this needed?

            let mut builder = BuilderWithLiveInfoEdit::new(matrix, cur_func);

            builder.set_insert_point_before_instr(call_instr_id);
            builder.insert_instr_id(store_instr_id);

            builder.set_insert_point_after_instr(call_instr_id);
            builder.insert_instr_id(load_instr_id);
        }
    }

    fn preserve_vreg_uses_across_call(
        &mut self,
        cur_func: &mut MachineFunction,
        matrix: &mut LiveRegMatrix,
    ) {
        let mut call_instr_id = vec![];

        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
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
