use crate::codegen::{arch::machine::register::*, common::machine::calling_conv::CallingConv};

pub struct SystemV {
    gr8: Vec<PhysReg>,
    gr32: Vec<PhysReg>,
    gr64: Vec<PhysReg>,
    xmm: Vec<PhysReg>,
}

#[rustfmt::skip]
impl SystemV {
    pub fn new() -> Self {
        Self {
            gr8:  to_phys![GR8::DIL,  GR8::SIL,  GR8::DL,   GR8::CL,   GR8::R8B,  GR8::R9B ],
            gr32: to_phys![GR32::EDI, GR32::ESI, GR32::EDX, GR32::ECX, GR32::R8D, GR32::R9D],
            gr64: to_phys![GR64::RDI, GR64::RSI, GR64::RDX, GR64::RCX, GR64::R8,  GR64::R9 ],
            xmm:  to_phys![XMM::XMM0, XMM::XMM1, XMM::XMM2, XMM::XMM3, XMM::XMM4, XMM::XMM5, XMM::XMM6, XMM::XMM7],
        }
    }
}

impl CallingConv for SystemV {
    fn get_nth_arg_reg(&self, rc: RegisterClassKind, nth: usize) -> Option<PhysReg> {
        match rc {
            RegisterClassKind::GR8 => self.gr8.get(nth),
            RegisterClassKind::GR32 => self.gr32.get(nth),
            RegisterClassKind::GR64 => self.gr64.get(nth),
            RegisterClassKind::XMM => self.xmm.get(nth),
        }
        .map_or(None, |r| Some(*r))
    }
}
