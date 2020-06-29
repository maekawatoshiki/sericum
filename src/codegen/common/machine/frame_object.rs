use crate::codegen::arch::exec::roundup;
use crate::codegen::arch::machine::inst::MachineOpcode;
use crate::codegen::common::machine::function::MachineFunction;
use crate::ir::types::*;
use rustc_hash::FxHashMap;
use std::cmp;
use std::fmt;

pub const ALIGN: i32 = 16;

#[derive(Debug, Clone)]
pub struct LocalVariables {
    pub locals: Vec<FrameIndexInfo>,
    pub cur_idx: usize,
}

#[derive(Debug)]
pub struct FrameObjectsInfo {
    pub offset_map: FxHashMap<FrameIndexKind, i32>, // frame index -> offset
    pub total_size: i32,
    pub callee_saved_regs_byte: usize, // unaligned
}

#[derive(Clone, PartialEq, Copy)]
pub struct FrameIndexInfo {
    pub ty: Type,
    pub idx: FrameIndexKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum FrameIndexKind {
    Arg(usize),
    Local(usize),
}

impl LocalVariables {
    pub fn new() -> Self {
        Self {
            locals: vec![],
            cur_idx: 0,
        }
    }

    pub fn alloc(&mut self, ty: &Type) -> FrameIndexInfo {
        let info = FrameIndexInfo::new(*ty, FrameIndexKind::Local(self.cur_idx));
        self.cur_idx += 1;
        self.locals.push(info.clone());
        info
    }
}

impl FrameObjectsInfo {
    pub fn aligned_callee_saved_regs_byte(&self) -> i32 {
        roundup(self.callee_saved_regs_byte as i32, ALIGN)
    }

    pub fn calc_max_adjust_stack_down(f: &MachineFunction) -> i32 {
        let mut down = 0i32;

        for (_, _, iiter) in f.body.mbb_iter() {
            for (_, inst) in iiter {
                match inst.opcode {
                    MachineOpcode::AdjStackDown => {
                        let d = inst.operand[0].as_constant().as_i32();
                        down = cmp::max(d, down);
                    }
                    MachineOpcode::AdjStackUp => {}
                    _ => continue,
                }
            }
        }

        down
    }
}

impl FrameIndexKind {
    pub fn new_arg(idx: usize) -> Self {
        FrameIndexKind::Arg(idx)
    }

    pub fn new_local(idx: usize) -> Self {
        FrameIndexKind::Local(idx)
    }
}

impl FrameIndexInfo {
    pub fn new(ty: Type, idx: FrameIndexKind) -> Self {
        Self { ty, idx }
    }
}

impl fmt::Debug for FrameIndexInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FI<{:?}, {:?}>", self.ty, self.idx)
    }
}
