use super::exec::roundup;
use crate::codegen::arch::machine::inst::MachineOpcode;
use crate::codegen::common::machine::function::MachineFunction;
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
    offset_map: FxHashMap<FrameIndexKind, i32>, // frame index -> offset
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
        let saved_regs_sz: usize = f
            .body
            .appeared_phys_regs()
            .containing_callee_saved_regs()
            .to_phys_set()
            .len()
            * 8
            + f.body.has_call() as usize * 8;
        let mut total_size = saved_regs_sz;

        // TODO: Implement
        // for (i, param_ty) in tys
        //     .base
        //     .borrow()
        //     .as_function_ty(f.ty)
        //     .unwrap()
        //     .params_ty
        //     .iter()
        //     .enumerate()
        // {
        // let rc = ty2rc(param_ty).unwrap();
        // if rc.get_nth_arg_reg(i).is_none() {
        //     offset += param_ty.size_in_byte(tys);
        //     offset_map.insert(FrameIndexKind::Arg(i), offset);
        // }
        // }

        for FrameIndexInfo { ty, .. } in &f.local_mgr.locals {
            total_size += ty.size_in_byte(tys);
        }

        total_size = roundup(total_size as i32, 16) as usize;

        let mut sz = 0;
        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            sz += ty.size_in_byte(tys) as i32;
            offset_map.insert(*idx, -(saved_regs_sz as i32) - sz);
        }

        let stack_down = Self::calc_max_adjust_stack_down(f) as i32;
        total_size = roundup(total_size as i32 + stack_down, 16) as usize;

        Self {
            offset_map,
            total_size,
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
