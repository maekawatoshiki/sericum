use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    register::*,
};
use super::reg_coalescer::coalesce_function;
use super::{builder::*, function::*, inst::*, liveness::*, module::*, spiller::Spiller};
use crate::ir::types::Types;
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub struct RegisterAllocator {
    queue: VecDeque<VirtReg>,
}

pub struct AllocationOrder<'a> {
    matrix: &'a LiveRegMatrix,
    func: &'a MachineFunction,
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
        use super::calc_spill_weight::calc_spill_weight;
        calc_spill_weight(cur_func, &mut matrix);

        // debug!(println!("before coalesing {:?}", cur_func));

        coalesce_function(&mut matrix, cur_func);

        // debug!(println!("after coalesing {:?}", cur_func));

        self.preserve_vreg_uses_across_call(tys, cur_func, &mut matrix);

        fn sort_queue(mut queue: Vec<VirtReg>, matrix: &LiveRegMatrix) -> Vec<VirtReg> {
            queue.sort_by(|x, y| {
                let x = matrix
                    .virt_reg_interval
                    .get(x)
                    .unwrap()
                    .start_point()
                    .unwrap();
                let y = matrix
                    .virt_reg_interval
                    .get(y)
                    .unwrap()
                    .start_point()
                    .unwrap();
                x.cmp(&y)
            });
            queue
        }

        // TODO: not efficient
        self.queue = sort_queue(matrix.collect_virt_regs(), &matrix)
            .into_iter()
            .collect();

        while let Some(vreg) = self.queue.pop_front() {
            let mut allocated = false;
            let order = AllocationOrder::new(&matrix, cur_func)
                .get_order(vreg)
                .unwrap();
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

            let new_regs = Spiller::new(cur_func, &mut matrix).spill(tys, reg_to_spill);
            self.queue.push_back(reg_to_spill);
            for new_reg in new_regs {
                self.queue.push_back(new_reg);
            }

            debug!(println!("interfering({:?}): {:?}", vreg, interfering);
                   println!("spill target: {:?}", reg_to_spill));
        }

        self.rewrite_vregs(cur_func, &matrix);

        coalesce_function(&mut matrix, cur_func); // spilling may cause another coalesce needs
    }

    fn rewrite_vregs(&mut self, cur_func: &mut MachineFunction, matrix: &LiveRegMatrix) {
        for (_, bb, iiter) in cur_func.body.mbb_iter() {
            for (_inst_id, inst) in iiter {
                for def in inst.collect_defined_regs() {
                    if let Some(reg) = def.get_reg() {
                        bb.liveness_ref_mut().add_phys_def(reg);
                        continue;
                    }

                    let reg = matrix
                        .virt_reg_interval
                        .get(&def.get_vreg())
                        .unwrap()
                        .reg
                        .unwrap();
                    def.set_phys_reg(reg);
                    bb.liveness_ref_mut().add_phys_def(reg);
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

        // IMPROVED EFFICIENCY A LITTLE: any better ideas?
        {
            let bb_containing_call =
                &cur_func.body.basic_blocks.arena[cur_func.body.inst_arena[call_inst_id].parent];
            let liveness = bb_containing_call.liveness_ref();
            let regs_that_may_interfere = &liveness.def | &liveness.live_in;
            for r in &regs_that_may_interfere {
                if matrix.interferes_with_range(
                    *r,
                    LiveRange::new(vec![LiveSegment::new(call_inst_pp, call_inst_pp)]),
                ) {
                    let r = matrix.get_entity_by_vreg(*r).unwrap().clone();
                    regs_to_save.insert(r);
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

        for (frinfo, reg) in slots_to_save_regs.into_iter().zip(regs_to_save.into_iter()) {
            let dst = MachineOperand::FrameIndex(frinfo.clone());
            let src = MachineOperand::Register(reg.clone());
            let rbp = MachineRegister::phys_reg(GR64::RBP);
            let store_inst_id = cur_func.body.inst_arena.alloc(MachineInst::new(
                &cur_func.vreg_gen,
                mov_mx(&src).unwrap(),
                vec![
                    MachineOperand::Mem(MachineMemOperand::BaseFi(
                        rbp.clone(),
                        *dst.as_frame_index(),
                    )),
                    src,
                ],
                None,
                call_inst_parent,
            ));

            let src = MachineOperand::FrameIndex(frinfo);

            let load_inst_id = cur_func.body.inst_arena.alloc(
                MachineInst::new_simple(
                    mov_rx(tys, &src).unwrap(),
                    vec![MachineOperand::Mem(MachineMemOperand::BaseFi(
                        rbp,
                        *src.as_frame_index(),
                    ))],
                    call_inst_parent,
                )
                .with_def(vec![reg]),
            );

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
                if inst.opcode == MachineOpcode::CALL {
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

impl<'a> AllocationOrder<'a> {
    pub fn new(matrix: &'a LiveRegMatrix, func: &'a MachineFunction) -> Self {
        Self { matrix, func }
    }

    pub fn get_order(&self, vreg: VirtReg) -> Option<RegisterOrder> {
        let reg = self
            .matrix
            .get_entity_by_vreg(vreg)
            .unwrap()
            .info_ref()
            .uses
            .iter()
            .find_map(|&use_| {
                let inst = &self.func.body.inst_arena[use_];
                if inst.opcode.is_copy_like() {
                    return inst.def[0].get_reg();
                }
                None
            });
        let mut order = self
            .matrix
            .get_entity_by_vreg(vreg)?
            .get_reg_class()
            .get_reg_order();

        if let Some(phys) = reg {
            order.add_preferred_reg(phys);
        }

        Some(order)
    }
}
