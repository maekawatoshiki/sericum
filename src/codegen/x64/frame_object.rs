use super::machine::register::ty2rc;
use crate::codegen::arch::machine::inst::MachineOpcode;
use crate::codegen::common::machine::function::MachineFunction;
use crate::codegen::x64::exec::roundup;
use crate::ir::types::*;
use rustc_hash::FxHashMap;
use std::cmp;
use std::fmt;

#[derive(Debug, Clone)]
pub struct LocalVariables {
    pub locals: Vec<FrameIndexInfo>,
    pub cur_idx: usize,
}

#[derive(Debug)]
pub struct FrameObjectsInfo {
    offset_map: FxHashMap<FrameIndexKind, usize>, // frame index -> offset
    pub total_size: usize,
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
    pub fn new(tys: &Types, f: &MachineFunction) -> Self {
        let mut offset_map = FxHashMap::default();
        let mut offset = 0;
        let has_call = f.body.has_call(); // push rbp -> 16 byte aligned

        for (i, param_ty) in tys
            .base
            .borrow()
            .as_function_ty(f.ty)
            .unwrap()
            .params_ty
            .iter()
            .enumerate()
        {
            // TODO: Correct?
            let rc = ty2rc(param_ty).unwrap();
            if rc.get_nth_arg_reg(i).is_none() {
                offset += param_ty.size_in_byte(tys);
                offset_map.insert(FrameIndexKind::Arg(i), offset);
            }
        }

        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            offset += ty.size_in_byte(tys);
            offset_map.insert(*idx, offset);
        }

        let stack_down = Self::calc_max_adjust_stack_down(f);
        offset = roundup(
            offset as i32
                + stack_down as i32
                + if has_call || (offset + stack_down == 0) {
                    0
                } else {
                    8
                },
            16,
        ) as usize;

        Self {
            offset_map,
            total_size: offset,
        }
    }

    pub fn offset(&self, kind: FrameIndexKind) -> Option<i32> {
        self.offset_map.get(&kind).map(|x| *x as i32)
    }

    pub fn total_size(&self) -> i32 {
        self.total_size as i32
    }

    fn calc_max_adjust_stack_down(f: &MachineFunction) -> usize {
        let mut down = 0;
        for (_, _, iiter) in f.body.mbb_iter() {
            for (_, inst) in iiter {
                match inst.opcode {
                    MachineOpcode::AdjStackDown => {
                        let d = inst.operand[0].as_constant().as_i32();
                        down = cmp::max(d as usize, down);
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
