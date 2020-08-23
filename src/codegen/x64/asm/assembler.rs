pub use crate::codegen::common::asm::assembler::{Assembler, InstAssembler};
use crate::codegen::x64::machine::inst::*;
use crate::codegen::x64::machine::register::RegisterId;

impl<'a> InstAssembler<'a> {
    pub fn assemble(&mut self) {
        match self.inst.opcode {
            MachineOpcode::PUSH64 => self.gen_push64(),
            MachineOpcode::POP64 => self.gen_pop64(),
            MachineOpcode::MOVri32 => self.gen_movri32(),
            MachineOpcode::MOVrm32 => self.gen_movrm32(),
            MachineOpcode::MOVrr64 => self.gen_movrr64(),
            MachineOpcode::MOVmi32 => self.gen_movmi32(),
            MachineOpcode::RET => self.gen_ret(),
            _ => unimplemented!(),
        }
        // debug!(println!("{:?}", self.inst));
    }

    fn gen_push64(&mut self) {
        let rd = reg_code(&self.inst.operand[0].as_register().id);
        self.stream.push_u8(0x50 + rd);
    }

    fn gen_pop64(&mut self) {
        let rd = reg_code(&self.inst.operand[0].as_register().id);
        self.stream.push_u8(0x58 + rd);
    }

    fn gen_movri32(&mut self) {
        self.stream.push_u8(0xb8);
        self.stream
            .push_u32_le(self.inst.operand[0].as_constant().as_i32() as u32);
    }

    fn gen_movrm32(&mut self) {
        self.stream.push_u8(0x8b); // TODO
        let reg = reg_code(&self.inst.def[0].id);
        let reg = reg << 3; // eax
        match self.inst.operand[0].as_mem() {
            MachineMemOperand::BaseFi(base, fi) => {
                let base = reg_code(&base.id);
                let off = -self
                    .function
                    .frame_objects
                    .as_ref()
                    .unwrap()
                    .offset(fi.idx)
                    .unwrap();
                let rm = base;
                self.stream.push_u8(0b01000000 + reg + rm);
                self.stream.push_u8(off as u32 as u8);
            }
            _ => unimplemented!(),
        }
    }

    fn gen_movrr64(&mut self) {
        self.stream.push_u8(0b01001000); // REX.W
        self.stream.push_u8(0x89);
        let reg = 4 << 3; // rsp
        let rm = 5; // rbp
        self.stream.push_u8(0b11000000 + reg + rm);
    }

    fn gen_movmi32(&mut self) {
        self.stream.push_u8(0xc7);

        match self.inst.operand[0].as_mem() {
            MachineMemOperand::BaseFi(base, fi) => {
                let base = reg_code(&base.id);
                let off = -self
                    .function
                    .frame_objects
                    .as_ref()
                    .unwrap()
                    .offset(fi.idx)
                    .unwrap();
                let reg = 0 << 3;
                let rm = base;
                self.stream.push_u8(0b01000000 + reg + rm);
                self.stream.push_u8(off as u32 as u8);
            }
            _ => unimplemented!(),
        }

        self.stream
            .push_u32_le(self.inst.operand[1].as_constant().as_i32() as u32);
    }

    fn gen_ret(&mut self) {
        // TODO: What's the difference between Near return and Far return?
        self.stream.push_u8(0xc3);
    }
}

fn reg_code(r: &RegisterId) -> u8 {
    let r = r.as_phys_reg();
    (r.retrieve() - r.reg_class() as usize) as u8
}
