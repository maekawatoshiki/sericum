use super::exec::roundup;
pub use crate::codegen::common::machine::frame_object::*;
use crate::codegen::common::machine::function::MachineFunction;
use crate::ir::types::*;
use rustc_hash::FxHashMap;

impl FrameObjectsInfo {
    pub fn new(tys: &Types, f: &MachineFunction) -> Self {
        let mut offset_map = FxHashMap::default();
        let callee_saved_regs_byte: usize = ::std::cmp::max(
            f.body
                .appeared_phys_regs()
                .containing_callee_saved_regs()
                .to_phys_set()
                .len()
                * 8,
            16,
        );
        // + f.body.has_call() as usize * 8/*=ra*/;
        let mut total_size = 0i32;

        let padding = |off, align| -> i32 { (align - off % align) % align };

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

        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            let size = ty.size_in_byte(tys) as i32;
            let align = ty.align_in_byte(tys) as i32;
            total_size += size + padding(total_size, align);
        }

        total_size = roundup(total_size, ALIGN);
        let mut sz = 0;
        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            let size = ty.size_in_byte(tys) as i32;
            let align = ty.align_in_byte(tys) as i32;
            sz += size + padding(sz, align);
            offset_map.insert(*idx, total_size - sz);
        }

        let stack_down = Self::calc_max_adjust_stack_down(f) as i32;
        total_size = roundup(total_size + stack_down, ALIGN);

        Self {
            offset_map,
            total_size,
            callee_saved_regs_byte,
        }
    }

    pub fn offset(&self, kind: FrameIndexKind) -> Option<i32> {
        self.offset_map
            .get(&kind)
            .map(|x| *x as i32 + roundup(self.callee_saved_regs_byte as i32, ALIGN))
    }

    pub fn total_size(&self) -> i32 {
        self.total_size as i32 + roundup(self.callee_saved_regs_byte as i32, ALIGN)
    }
}
