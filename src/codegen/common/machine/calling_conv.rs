use crate::{
    codegen::{arch::machine::register::RegisterClassKind, common::machine::register::PhysReg},
    ir::types::StructType,
};
use rustc_hash::FxHashMap;

pub trait CallingConv: Clone {
    fn get_nth_arg_reg(&self, rc: RegisterClassKind, nth: usize) -> Option<PhysReg>;
    fn reg_classes_used_for_passing_byval(s: &StructType) -> Vec<RegisterClassKind>;
}

#[derive(Clone)]
pub struct ArgumentRegisterOrder<'a, ABI: CallingConv> {
    abi: &'a ABI,
    nths: FxHashMap<RegisterClassKind, usize>,
}

impl<'a, ABI: CallingConv> ArgumentRegisterOrder<'a, ABI> {
    pub fn new(abi: &'a ABI) -> Self {
        Self {
            abi,
            nths: FxHashMap::default(),
        }
    }

    pub fn next(&mut self, rc: RegisterClassKind) -> Option<PhysReg> {
        let base = rc.register_file_base_class();
        let nth = self.nths.entry(base).or_insert(0);
        *nth += 1;
        self.abi.get_nth_arg_reg(rc, *nth - 1)
    }

    pub fn regs_available_for(&self, rcs: &[RegisterClassKind]) -> bool {
        let mut arg_regs_order: ArgumentRegisterOrder<'a, ABI> = (*self).clone();
        rcs.iter().all(|&rc| arg_regs_order.next(rc).is_some())
    }
}
