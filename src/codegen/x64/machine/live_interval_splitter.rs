use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    machine::register::*,
};
use super::calc_spill_weight::calc_spill_weight;
use super::reg_coalescer::coalesce_function;
use super::{inst::*, spiller::Spiller};
use crate::codegen::common::machine::{builder::*, function::*, liveness::*, module::*};
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
        reg: &RegisterId,
        slot: &FrameIndexInfo,
        after_store: &MachineInstId,
        before_load: &MachineInstId,
    ) -> Option<RegisterId> {
        if reg.is_phys_reg() || self.func.regs_info.arena_ref()[*reg].defs.len() != 1 {
            return None;
        }

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
        // store
        // i

        let reg_interval = &mut self.matrix.virt_reg_interval.inner_mut()[reg.as_virt_reg()];
        assert!(reg_interval.start_point() <= store_pp);
        reg_interval.end_point_mut().unwrap() = store_pp;

        None
    }
}
