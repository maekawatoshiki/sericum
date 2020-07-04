use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    machine::register::*,
};
use super::calc_spill_weight::calc_spill_weight;
use super::reg_coalescer::coalesce_function;
use super::{inst::*, spiller::Spiller};
use crate::analysis::loops::{Loops, LoopsConstructor};
use crate::codegen::common::machine::{
    basic_block::*, builder::*, function::*, liveness::*, module::*,
};
use crate::{ir::types::Types, traits::pass::ModulePassTrait};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub struct LiveIntervalSplitter<'a> {
    func: &'a mut MachineFunction,
    matrix: &'a mut LiveRegMatrix,
}

impl<'a> LiveIntervalSplitter<'a> {
    pub fn new(func: &'a mut MachineFunction, matrix: &'a mut LiveRegMatrix) -> Self {
        Self { func, matrix }
    }

    // [a, i1), [i2, b)
    // before i1,
    pub fn split(
        &mut self,
        tys: &Types,
        loops: &Loops<MachineBasicBlock>,
        reg: &RegisterId,
        slot: &FrameIndexInfo,
        after_store: &MachineInstId,
        before_load: &MachineInstId,
    ) -> Option<RegisterId> {
        // virtual registers only
        if reg.is_phys_reg() {
            return None;
        }
        // no multiple defines
        if self.func.regs_info.arena_ref()[*reg].defs.len() > 1 {
            return None;
        }
        // all uses must be in one basic block
        {
            let after_store_parent = self.func.body.inst_arena[*after_store].parent;
            let before_load_parent = self.func.body.inst_arena[*before_load].parent;
            assert!(self.func.regs_info.arena_ref()[*reg].defs.len() == 1);
            let mut last_parent = None;
            for &use_id in &self.func.regs_info.arena_ref()[*reg].uses {
                let parent = self.func.body.inst_arena[use_id].parent;
                if last_parent.is_none() {
                    last_parent = Some(parent);
                    continue;
                }
                if let Some(last_parent) = last_parent {
                    if last_parent != parent {
                        return None;
                    }
                }
            }
        }
        debug!(println!("split {:?}", reg));

        let after_store_pp = self.matrix.get_program_point(*after_store).unwrap();
        let before_load_pp = self.matrix.get_program_point(*before_load).unwrap();

        let parent = self.func.body.inst_arena[*after_store].parent;
        let src = MachineOperand::Register(*reg);
        let rbp = self.func.regs_info.get_phys_reg(GR64::RBP);
        let mem = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *slot));
        let store_id = self.func.alloc_inst(MachineInst::new_simple(
            mov_mx(&self.func.regs_info, &src).unwrap(),
            vec![mem, src],
            parent,
        ));
        let store_pp = self.matrix.program_points.prev_of(after_store_pp);
        self.matrix.id2pp.insert(store_id, store_pp);

        let reg_intvl = self
            .matrix
            .virt_reg_interval
            .get_mut(&reg.as_virt_reg())
            .unwrap();
        assert!(reg_intvl.start_point().unwrap() <= store_pp);
        let new_reg_end_point = reg_intvl.end_point().unwrap();
        reg_intvl.range.remove_segment(&LiveSegment::new(
            before_load_pp,
            reg_intvl.end_point().unwrap(),
        ));
        reg_intvl
            .range
            .add_segment(LiveSegment::new(reg_intvl.start_point().unwrap(), store_pp));

        let mut new_reg_uses = FxHashSet::default();
        self.func.regs_info.arena_ref_mut()[*reg]
            .uses
            .retain(|&id| {
                let x = self.matrix.get_program_point(id).unwrap() <= store_pp;
                if !x {
                    new_reg_uses.insert(id);
                }
                x
            });
        assert!(self.func.regs_info.arena_ref_mut()[*reg].uses.len() > 0);
        let new_reg = self.func.regs_info.new_virt_reg_from(&reg);
        self.matrix.add_vreg_entity(new_reg);
        for &id in &new_reg_uses {
            let inst = &mut self.func.body.inst_arena[id];
            inst.replace_operand_register(&self.func.regs_info, *reg, new_reg);
        }
        self.func.regs_info.arena_ref_mut()[new_reg].uses = new_reg_uses;

        let parent = self.func.body.inst_arena[*before_load].parent;

        // println!("!!");
        // fn f(
        //     reg: &RegisterId,
        //     new_reg: RegisterId,
        //     bb_id: MachineBasicBlockId,
        //     bbs: &MachineBasicBlocks,
        // ) {
        //     println!("{:?}", bb_id);
        //     let bb = &bbs.arena[bb_id];
        //     let liveness = &mut bb.liveness_ref_mut();
        //     if liveness.live_in.remove(reg) {
        //         liveness.add_live_in(new_reg);
        //     }
        //     if liveness.live_out.remove(reg) {
        //         liveness.add_live_out(new_reg);
        //     }
        //     println!("--");
        //     for s in &bb.succ {
        //         f(reg, new_reg, *s, bbs);
        //     }
        // }
        // f(reg, new_reg, parent, &self.func.body.basic_blocks);

        let src = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *slot));
        let load_id = self.func.alloc_inst(
            MachineInst::new_simple(
                mov_rx(tys, &self.func.regs_info, &src).unwrap(),
                vec![src],
                parent,
            )
            .with_def(vec![new_reg]),
        );
        let load_pp = self.matrix.program_points.next_of(before_load_pp);
        self.matrix.id2pp.insert(load_id, load_pp);
        {
            let liveness = &mut *self.func.body.basic_blocks.arena[parent].liveness_ref_mut();
            liveness.add_def(new_reg);
        }
        self.matrix.virt_reg_interval.add(
            new_reg.as_virt_reg(),
            LiveRange::new(vec![LiveSegment::new(load_pp, new_reg_end_point)]),
        );

        // TODO: insert or remove reg&new_reg from basic block livein&liveout

        let mut builder = Builder::new(self.func);

        builder.set_insert_point_before_inst(*after_store);
        builder.insert(store_id);

        builder.set_insert_point_after_inst(*before_load);
        builder.insert(load_id);

        Some(new_reg)
    }
}
