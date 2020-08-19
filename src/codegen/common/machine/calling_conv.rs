use crate::codegen::{
    arch::machine::register::RegisterClassKind, common::machine::register::PhysReg,
};
use rustc_hash::FxHashMap;

pub trait CallingConv {
    fn get_nth_arg_reg(&self, rc: RegisterClassKind, nth: usize) -> Option<PhysReg>;
}

pub struct ArgumentRegisterOrder<ABI: CallingConv> {
    abi: ABI,
    nths: FxHashMap<RegisterClassKind, usize>,
}

impl<ABI: CallingConv> ArgumentRegisterOrder<ABI> {
    pub fn new(abi: ABI) -> Self {
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
}
