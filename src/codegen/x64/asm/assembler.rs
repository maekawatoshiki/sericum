pub use crate::codegen::common::asm::assembler::{Assembler, InstAssembler};
use crate::codegen::x64::machine::inst::*;
use crate::codegen::x64::machine::register::RegisterId;

impl<'a> InstAssembler<'a> {
    pub fn assemble(&mut self) {
        let start = self.stream.data().len();
        match self.inst.opcode {
            MachineOpcode::PUSH64 => self.gen_push64(),
            MachineOpcode::POP64 => self.gen_pop64(),

            MachineOpcode::MOVri32 => self.gen_movri32(),
            MachineOpcode::MOVrm32 => self.gen_movrm32(),
            MachineOpcode::MOVrr32 => self.gen_mov_rr32(),
            MachineOpcode::MOVrr64 => self.gen_movrr64(),
            MachineOpcode::MOVmi32 => self.gen_movmi32(),

            MachineOpcode::ADDri32 => self.gen_add_ri32(),

            MachineOpcode::SUBri32 => self.gen_sub_ri32(),

            MachineOpcode::IMULrri32 => self.gen_imul_rri32(),

            MachineOpcode::RET => self.gen_ret(),
            _ => unimplemented!(),
        };
        let end = self.stream.data().len();
        self.labels.add_offset(end - start);
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
            MachineMemOperand::BaseOff(base, off) => {
                self.stream
                    .push_u8(mod_rm(Mod::BaseDisp8, reg, reg_code(&base.id)));
                self.stream.push_u8(*off as u32 as u8);
            }
            _ => unimplemented!(),
        }
    }

    fn gen_mov_rr32(&mut self) {
        self.stream.push_u8(0x89);
        self.stream.push_u8(mod_rm(
            Mod::Reg,
            reg_code(&self.inst.operand[0].as_register().id),
            reg_code(&self.inst.def[0].id),
        ));
    }

    fn gen_movrr64(&mut self) {
        self.stream.push_u8(0b01001000); // REX.W
        self.stream.push_u8(0x89);
        self.stream.push_u8(mod_rm(
            Mod::Reg,
            reg_code(&self.inst.operand[0].as_register().id),
            reg_code(&self.inst.def[0].id),
        ));
    }

    fn gen_movmi32(&mut self) {
        self.stream.push_u8(0xc7);

        match self.inst.operand[0].as_mem() {
            MachineMemOperand::BaseOff(base, off) => {
                self.stream
                    .push_u8(mod_rm(Mod::BaseDisp8, 0, reg_code(&base.id)));
                self.stream.push_u8(*off as u32 as u8);
            }
            _ => unimplemented!(),
        }

        self.stream
            .push_u32_le(self.inst.operand[1].as_constant().as_i32() as u32);
    }

    fn gen_add_ri32(&mut self) {
        self.stream.push_u8(0x81);
        self.stream
            .push_u8(mod_rm(Mod::Reg, 0, reg_code(&self.inst.def[0].id)));
        self.stream
            .push_u32_le(self.inst.operand[1].as_constant().as_i32() as u32)
    }

    fn gen_sub_ri32(&mut self) {
        self.stream.push_u8(0x81);
        self.stream
            .push_u8(mod_rm(Mod::Reg, 5, reg_code(&self.inst.def[0].id)));
        self.stream
            .push_u32_le(self.inst.operand[1].as_constant().as_i32() as u32)
    }

    fn gen_imul_rri32(&mut self) {
        self.stream.push_u8(0x69);
        self.stream.push_u8(mod_rm(
            Mod::Reg,
            reg_code(&self.inst.def[0].id),
            reg_code(&self.inst.operand[0].as_register().id),
        ));
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

#[allow(dead_code)]
enum Mod {
    Reg,
    Base,
    BaseDisp8,
    BaseDisp32,
}

fn mod_rm(mod_: Mod, reg: u8, rm: u8) -> u8 {
    let mod_ = match mod_ {
        Mod::Reg => 0b11,
        Mod::Base => 0b00,
        Mod::BaseDisp8 => 0b01,
        Mod::BaseDisp32 => 0b10,
    };
    (mod_ << 6) + (reg << 3) + rm
}
