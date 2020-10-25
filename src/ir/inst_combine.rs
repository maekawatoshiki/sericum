use crate::ir::{
    function::Function,
    module::Module,
    opcode::{Instruction, Opcode},
    value::Value,
};
use std::collections::VecDeque;

pub struct InstructionCombine {}

pub struct InstructionCombineOnFunction<'a> {
    func: &'a mut Function,
}

impl InstructionCombine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }

            InstructionCombineOnFunction { func }.run()
        }
    }
}

impl<'a> InstructionCombineOnFunction<'a> {
    pub fn run(&mut self) {
        let mut worklist = VecDeque::new();

        for (_, block) in &self.func.basic_blocks.arena {
            for &inst_id in &*block.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];
                if self.is_combinable(inst) {
                    worklist.push_back(inst_id);
                }
            }
        }

        while let Some(inst_id) = worklist.pop_front() {
            let inst1 = &self.func.inst_table[inst_id];
            if !self.is_combinable(inst1) {
                continue;
            }
            let inst2_id = *inst1.users.borrow().iter().next().unwrap();
            let op0 = inst1.operand.args()[0].clone();
            let op1 = inst1.operand.args()[1].clone();

            if inst1.has_one_use() {
                self.func.remove_inst(inst_id);
            }

            let inst2 = &mut self.func.inst_table[inst2_id];

            inst2.operand.args_mut()[0] = op0;
            inst2.operand.args_mut()[1] = match inst2.opcode {
                Opcode::Add => op1.const_add(&inst2.operand.args()[1]).unwrap(),
                Opcode::Sub => op1.const_add(&inst2.operand.args()[1]).unwrap(),
                Opcode::Mul => op1.const_mul(&inst2.operand.args()[1]).unwrap(),
                Opcode::Div => op1.const_mul(&inst2.operand.args()[1]).unwrap(),
                Opcode::Rem => op1.const_rem(&inst2.operand.args()[1]).unwrap(),
                _ => unreachable!(),
            };
        }
    }

    pub fn is_combinable(&self, inst: &Instruction) -> bool {
        Self::is_combinable_sub(inst) && {
            let user_id = *inst.users.borrow().iter().next().unwrap();
            let user = &self.func.inst_table[user_id];
            inst.opcode == user.opcode
                && inst.parent == user.parent
                && Self::is_combinable_sub(user)
        }
    }

    pub fn is_combinable_sub(inst: &Instruction) -> bool {
        matches!(
            inst.opcode,
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Rem
        ) && matches!(inst.operand.args()[0], Value::Instruction(_))
            && matches!(inst.operand.args()[1], Value::Immediate(_))
    }
}
