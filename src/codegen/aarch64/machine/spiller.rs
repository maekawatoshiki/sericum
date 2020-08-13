use crate::codegen::arch::{frame_object::FrameIndexInfo, machine::register::*};
pub use crate::codegen::common::machine::regalloc::*;
use crate::codegen::common::machine::{function::MachineFunction, liveness::LiveRegMatrix};
use crate::ir::types::Types;

pub struct Spiller<'a> {
    func: &'a mut MachineFunction,
    matrix: &'a mut LiveRegMatrix,
}

impl<'a> Spiller<'a> {
    pub fn new(func: &'a mut MachineFunction, matrix: &'a mut LiveRegMatrix) -> Self {
        Self { func, matrix }
    }

    pub fn insert_evict(&mut self, reg_id: RegisterId, slot: &FrameIndexInfo) -> Vec<VirtReg> {
        unimplemented!()
    }

    pub fn insert_reload(
        &mut self,
        tys: &Types,
        reg_id: RegisterId,
        slot: &FrameIndexInfo,
    ) -> Vec<VirtReg> {
        unimplemented!()
    }

    pub fn spill(&mut self, tys: &Types, vreg: VirtReg) -> Vec<VirtReg> {
        unimplemented!()
    }
}
