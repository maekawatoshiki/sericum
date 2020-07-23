use super::exec::roundup;
use super::machine::register::ty2rc;
pub use crate::codegen::common::machine::frame_object::*;
use crate::codegen::common::machine::function::MachineFunction;
use crate::ir::types::*;
use rustc_hash::FxHashMap;

impl FrameObjectsInfo {
    pub fn new(tys: &Types, f: &MachineFunction) -> Self {
        let mut offset_map = FxHashMap::default();
        let mut offset = 0i32;
        let callee_saved_regs_byte: usize = f
            .body
            .appeared_phys_regs()
            .containing_callee_saved_regs()
            .to_phys_set()
            .len()
            * 8;
        let has_call = f.body.has_call(); // push rbp -> 16 byte aligned

        let padding = |off, align| -> i32 { (align - off % align) % align };

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
                let size = param_ty.size_in_byte(tys) as i32;
                let align = param_ty.align_in_byte(tys) as i32;
                offset += size + padding(offset, align);
                offset_map.insert(FrameIndexKind::Arg(i), offset);
            }
        }

        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            let size = ty.size_in_byte(tys) as i32;
            let align = ty.align_in_byte(tys) as i32;
            offset += size + padding(offset, align);
            offset_map.insert(*idx, offset);
        }

        let stack_down = Self::calc_max_adjust_stack_down(f);
        offset = roundup(
            offset
                + stack_down as i32
                + if has_call || (offset + stack_down == 0) {
                    0
                } else {
                    8
                },
            16,
        );

        Self {
            offset_map,
            total_size: offset,
            callee_saved_regs_byte,
        }
    }

    pub fn offset(&self, kind: FrameIndexKind) -> Option<i32> {
        self.offset_map.get(&kind).map(|x| *x as i32)
    }

    pub fn total_size(&self) -> i32 {
        self.total_size
    }
}
