use crate::codegen::arch::{frame_object::FrameIndexInfo, machine::register::*};
pub use crate::codegen::common::machine::regalloc::*;
use crate::codegen::common::machine::{function::MachineFunction, liveness::LiveRegMatrix};

pub struct Spiller<'a> {
    _func: &'a mut MachineFunction,
    _matrix: &'a mut LiveRegMatrix,
}

impl<'a> Spiller<'a> {
    pub fn new(_func: &'a mut MachineFunction, _matrix: &'a mut LiveRegMatrix) -> Self {
        Self { _func, _matrix }
    }

    pub fn insert_evict(&mut self, _reg_id: RegisterId, _slot: &FrameIndexInfo) -> Vec<VirtReg> {
        unimplemented!()
    }

    pub fn insert_reload(&mut self, _reg_id: RegisterId, _slot: &FrameIndexInfo) -> Vec<VirtReg> {
        unimplemented!()
    }

    pub fn spill(&mut self, _vreg: VirtReg) -> Vec<VirtReg> {
        unimplemented!()
    }
}
