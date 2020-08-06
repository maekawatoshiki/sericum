use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    machine::register::*,
};
use super::calc_spill_weight::calc_spill_weight;
use super::live_interval_splitter::LiveIntervalSplitter;
use super::reg_coalescer::coalesce_function;
use super::{inst::*, spiller::Spiller};
use crate::analysis::{dom_tree::DominatorTreeConstructor, loops::LoopsConstructor};
use crate::codegen::common::machine::{builder::*, function::*, liveness::*, module::*};
use crate::{ir::types::Types, traits::pass::ModulePassTrait};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub struct RegisterAllocator {
    queue: VecDeque<VirtReg>,
}

pub struct AllocationOrder<'a> {
    matrix: &'a LiveRegMatrix,
    func: &'a MachineFunction,
}

impl ModulePassTrait for RegisterAllocator {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "RegisterAllocator"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
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
        if cur_func.is_internal {
            return;
        }

        let mut matrix = LivenessAnalysis::new().analyze_function(cur_func);
        calc_spill_weight(cur_func, &mut matrix);

        self.preserve_reg_uses_across_call(tys, cur_func, &mut matrix);
        coalesce_function(&mut matrix, cur_func);

        self.queue = matrix.collect_virt_regs().into_iter().collect();
        self.sort_queue(&matrix); // for better allocation. not necessary

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

            let mut interfering = matrix.collect_interfering_assigned_regs(vreg);

            interfering.sort_by(|x, y| {
                let x = matrix.virt_reg_interval.get(x).unwrap().spill_weight;
                let y = matrix.virt_reg_interval.get(y).unwrap().spill_weight;
                x.partial_cmp(&y).unwrap()
            });

            // Spill virtual registers in the order of low spill weight
            let mut allocatable = false;
            for &reg2spill in &interfering {
                if matrix
                    .virt_reg_interval
                    .get(&reg2spill)
                    .unwrap()
                    .is_spillable
                    == false
                {
                    continue;
                }
                let r = matrix.unassign_reg(reg2spill).unwrap();
                let new_regs = Spiller::new(cur_func, &mut matrix).spill(tys, reg2spill);
                for &new_reg in &new_regs {
                    self.queue.push_front(new_reg);
                }
                if !matrix.interferes(vreg, r) {
                    allocatable = true;
                    break;
                }
            }
            assert!(allocatable); // TODO
            self.queue.push_front(vreg);
        }

        self.rewrite_vregs(cur_func, &matrix);

        coalesce_function(&mut matrix, cur_func); // spilling may cause another coalesce needs
    }

    fn sort_queue(&mut self, matrix: &LiveRegMatrix) {
        let mut queue = ::std::mem::replace(&mut self.queue, VecDeque::new())
            .into_iter()
            .collect::<Vec<_>>();
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
        // queue.sort_by(|x, y| {
        //     let x = matrix.virt_reg_interval.get(x).unwrap().spill_weight;
        //     let y = matrix.virt_reg_interval.get(y).unwrap().spill_weight;
        //     y.partial_cmp(&x).unwrap()
        // });
        self.queue = queue.into_iter().collect();
    }

    fn rewrite_vregs(&mut self, cur_func: &mut MachineFunction, matrix: &LiveRegMatrix) {
        let mut arena = cur_func.regs_info.arena_ref_mut();
        for (_id, bb) in cur_func.body.basic_blocks.id_and_block() {
            let mut liveness = bb.liveness_ref_mut();
            for inst_id in &*bb.iseq_ref() {
                let inst = &mut cur_func.body.inst_arena[*inst_id];
                for reg in inst.collect_all_regs_mut() {
                    let r = &mut arena[*reg];
                    if reg.is_phys_reg() {
                        liveness.add_phys_def(reg.as_phys_reg());
                        continue;
                    }

                    let p = matrix
                        .virt_reg_interval
                        .get(&r.virt_reg)
                        .unwrap()
                        .reg
                        .unwrap();
                    r.phys_reg = Some(p);
                    reg.kind = VirtOrPhys::Phys(p);
                    liveness.add_phys_def(p);
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
            r: RegisterId,
        ) -> FrameIndexInfo {
            let rc = cur_func.regs_info.arena_ref()[r].reg_class;
            for slot in &*cur_func.local_mgr.locals {
                if occupied.contains(&slot.idx) {
                    continue;
                }
                if Some(rc) == ty2rc(&slot.ty) {
                    occupied.insert(slot.idx);
                    return slot.clone();
                }
            }
            let slot = cur_func.local_mgr.alloc(&rc2ty(rc));
            occupied.insert(slot.idx);
            slot
        }

        let call_inst_pp = matrix.get_program_point(call_inst_id).unwrap();
        let mut regs_to_save = FxHashSet::default();

        // TODO: So slow. Any better algorithms?
        {
            let bb_containing_call =
                &cur_func.body.basic_blocks.arena[cur_func.body.inst_arena[call_inst_id].parent];
            let liveness = bb_containing_call.liveness_ref();
            let mut regs_that_may_interfere = &liveness.def | &liveness.live_in;
            // remove registers like rbp, rsp, rax...
            for def in cur_func.body.inst_arena[call_inst_id]
                .collect_defined_regs()
                .iter()
                .filter(|def| def.is_phys_reg())
            {
                regs_that_may_interfere.remove(def);
            }
            let range = LiveRange::new(vec![LiveSegment::new(call_inst_pp, call_inst_pp)]);
            for r in &regs_that_may_interfere {
                if (r.is_virt_reg() && matrix.interferes_with_range(r.as_virt_reg(), &range))
                    || (r.is_phys_reg()
                        && matrix.interferes_phys_with_range(r.as_phys_reg(), &range))
                {
                    regs_to_save.insert(*r);
                }
            }
        }

        let mut slots_to_save_regs = vec![];
        for r in &regs_to_save {
            slots_to_save_regs.push(find_unused_slot(cur_func, occupied, *r));
        }

        let call_inst_parent = cur_func.body.inst_arena[call_inst_id].parent;

        let dom_tree = DominatorTreeConstructor::new(&cur_func.body.basic_blocks).construct();
        let loops = LoopsConstructor::new(&dom_tree, &cur_func.body.basic_blocks).analyze();

        for (frinfo, reg) in slots_to_save_regs.into_iter().zip(regs_to_save.into_iter()) {
            if let Some(_) = LiveIntervalSplitter::new(cur_func, matrix).split(
                tys,
                &dom_tree,
                &loops,
                &reg,
                &frinfo,
                &call_inst_id,
                &call_inst_id,
            ) {
                continue;
            }

            let dst = MachineOperand::FrameIndex(frinfo.clone());
            let src = MachineOperand::Register(reg);
            let rbp = cur_func.regs_info.get_phys_reg(GR64::RBP);
            let store_inst_id = cur_func.alloc_inst(MachineInst::new(
                &cur_func.regs_info,
                mov_mx(&cur_func.regs_info, &src).unwrap(),
                vec![
                    MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *dst.as_frame_index())),
                    src,
                ],
                None,
                call_inst_parent,
            ));

            let src = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, frinfo));
            let load_inst_id = cur_func.alloc_inst(
                MachineInst::new_simple(
                    mov_rx(tys, &cur_func.regs_info, &src).unwrap(),
                    vec![src],
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

    fn preserve_reg_uses_across_call(
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

        for v in matrix.collect_virt_regs() {
            let mut count = 0;
            for &c in &call_inst_id {
                let c = matrix.get_program_point(c).unwrap();
                if matrix.interferes_with_range(v, &LiveRange::new(vec![LiveSegment::new(c, c)])) {
                    count += 1;
                }
            }
            count *= 2/*=load+store*/;
            let spill_makes_less_mem_access = {
                let arena = cur_func.regs_info.arena_ref();
                let rinfo = &arena[*matrix.virt_regs.get(&v).unwrap()];
                (rinfo.defs.len() + rinfo.uses.len()) < count
            };
            if spill_makes_less_mem_access {
                Spiller::new(cur_func, matrix).spill(tys, v);
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
        let reg = self.func.regs_info.arena_ref()[*self.matrix.get_entity_by_vreg(vreg).unwrap()]
            .uses
            .iter()
            .find_map(|&use_| {
                let inst = &self.func.body.inst_arena[use_];
                // println!(" I {:?}", inst);
                if inst.opcode.is_copy_like() {
                    return self.func.regs_info.arena_ref()[inst.def[0]].phys_reg;
                }
                None
            });
        let mut order = self.func.regs_info.arena_ref()[*self.matrix.get_entity_by_vreg(vreg)?]
            .reg_class
            .get_reg_order();

        if let Some(phys) = reg {
            order.add_preferred_reg(phys);
        }

        Some(order)
    }
}
