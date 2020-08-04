pub use crate::codegen::common::asm::assembler::{Assembler, InstAssembler};
use crate::codegen::x64::machine::inst::*;

impl<'a> InstAssembler<'a> {
    pub fn assemble(&mut self) {
        match self.inst.opcode {
            MachineOpcode::MOVri32 => self.gen_movri32(),
            MachineOpcode::RET => self.gen_ret(),
            _ => unimplemented!(),
        }
        // debug!(println!("{:?}", self.inst));
    }

    fn gen_movri32(&mut self) {
        self.stream.push_u8(0xb8);
        self.stream
            .push_little_u32(self.inst.operand[0].as_constant().as_i32() as u32);
    }

    fn gen_ret(&mut self) {
        // TODO: What's the difference between Near return and Far return?
        self.stream.push_u8(0xc3);
    }
}
