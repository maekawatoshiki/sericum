use super::exec::roundup;
use super::machine::register::ty2rc;
use crate::codegen::arch::machine::calling_conv::SystemV;
use crate::codegen::common::machine::function::MachineFunction;
pub use crate::codegen::common::machine::{calling_conv::ArgumentRegisterOrder, frame_object::*};
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

        let base = &tys.base.borrow();
        let f_ty = base.as_function_ty(f.ty).unwrap();

        let mut arg_reg_order = ArgumentRegisterOrder::new(SystemV::new());
        for (i, param_ty) in f_ty.params_ty.iter().enumerate() {
            // TODO: Correct?
            let byval = f_ty.params_attr.get(&i).map_or(false, |attr| attr.byval);
            if byval {
                let param_ty = base.get_element_ty(*param_ty, None).unwrap();
                let size = param_ty.size_in_byte(tys) as i32;
                let align = param_ty.align_in_byte(tys) as i32;
                offset += size + padding(offset, align);
                offset_map.insert(FrameIndexKind::Arg(i), offset);
                continue;
            }

            // TODO: bug
            let rc = ty2rc(param_ty).unwrap();
            if arg_reg_order.next(rc).is_none() {
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
