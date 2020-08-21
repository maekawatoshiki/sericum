use crate::{
    codegen::{arch::machine::register::*, common::machine::calling_conv::CallingConv},
    ir::types::{StructType, Type},
};

#[derive(Clone)]
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

    fn reg_classes_used_for_passing_byval(struct_ty: &StructType) -> Vec<RegisterClassKind> {
        let sz = struct_ty.size();
        let moves_by_8_bytes = sz / 8;
        let moves_by_4_bytes = (sz - 8 * moves_by_8_bytes) / 4;
        assert!((sz - 8 * moves_by_8_bytes) % 4 == 0);

        let mut regs = vec![];

        if sz <= 16 {
            let mut off = 0;
            for &(count, size, rc) in &[
                (moves_by_8_bytes, 8, RegisterClassKind::GR64),
                (moves_by_4_bytes, 4, RegisterClassKind::GR32),
            ] {
                for _ in 0..count {
                    let float = struct_ty.get_type_at(off) == Some(&Type::f64);
                    regs.push(if float { RegisterClassKind::XMM } else { rc });
                    off += size;
                }
            }
            return regs;
        }

        // the size of struct_ty is over 16, so put it onto stack
        vec![]
    }
}
