use crate::codegen::{arch::machine::register::*, common::machine::calling_conv::CallingConv};

pub struct AAPCS64 {
    gr32: Vec<PhysReg>,
    gr64: Vec<PhysReg>,
}

#[rustfmt::skip]
impl AAPCS64 {
    pub fn new() -> Self {
        Self {
            gr32: to_phys![GR32::W0, GR32::W1, GR32::W2, GR32::W3, GR32::W4, GR32::W5, GR32::W6, GR32::W7],
            gr64: to_phys![GR64::X0, GR64::X1, GR64::X2, GR64::X3, GR64::X4, GR64::X5, GR64::X6, GR64::X7],
        }
    }
}

impl CallingConv for AAPCS64 {
    fn get_nth_arg_reg(&self, rc: RegisterClassKind, nth: usize) -> Option<PhysReg> {
        match rc {
            RegisterClassKind::GR32 => self.gr32.get(nth),
            RegisterClassKind::GR64 => self.gr64.get(nth),
            RegisterClassKind::WSP => None,
            RegisterClassKind::SP => None,
        }
        .map_or(None, |r| Some(*r))
    }
}
