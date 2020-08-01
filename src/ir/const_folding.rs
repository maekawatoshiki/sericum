use crate::ir::{
    function::Function,
    module::Module,
    opcode::{Instruction, Opcode, Operand},
    value::Value,
};
use std::collections::VecDeque;
// use rustc_hash::FxHashMap;

pub struct ConstantFolding {}

struct ConstantFoldingOnFunction<'a> {
    cur_func: &'a mut Function,
}

impl ConstantFolding {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            ConstantFoldingOnFunction::new(func).run()
        }
    }
}

impl<'a> ConstantFoldingOnFunction<'a> {
    pub fn new(cur_func: &'a mut Function) -> Self {
        Self { cur_func }
    }

    pub fn run(&mut self) {
        let mut foldable = VecDeque::new();
        let mut to_shift = VecDeque::new();

        for &id in &self.cur_func.basic_blocks.order {
            let bb = &self.cur_func.basic_blocks.arena[id];
            for val in &*bb.iseq.borrow() {
                let inst_id = val.get_inst_id().unwrap();
                let inst = &self.cur_func.inst_table[inst_id];

                if Self::is_foldable(inst) {
                    foldable.push_back(inst_id);
                }

                if Self::is_mul_power_of_two(inst) {
                    to_shift.push_back(inst_id);
                }
            }
        }

        while let Some(inst_id) = foldable.pop_front() {
            let inst = &self.cur_func.inst_table[inst_id];
            let folded = match inst.fold_const() {
                Some(folded) => folded,
                None => continue,
            };
            let users = inst.users.clone();
            for &user_id in &*users.borrow() {
                let ty = self.cur_func.inst_table[inst_id].ty;
                Instruction::replace_operand(
                    &mut self.cur_func.inst_table,
                    user_id,
                    // TODO: Very inefficient!
                    &Operand::new_inst(self.cur_func.id.unwrap(), inst_id, ty),
                    Operand::Value(folded),
                );
                let user = &self.cur_func.inst_table[user_id];
                if Self::is_foldable(user) {
                    foldable.push_back(user_id)
                }
            }
            self.cur_func.remove_inst(inst_id);
        }

        while let Some(inst_id) = to_shift.pop_front() {
            let inst = &mut self.cur_func.inst_table[inst_id];
            assert!(inst.opcode == Opcode::Mul);
            inst.opcode = Opcode::Shl;
            inst.operands[1] = Operand::Value(Value::new_imm_int8(
                inst.operands[1]
                    .get_value()
                    .unwrap()
                    .get_imm()
                    .unwrap()
                    .is_power_of_two()
                    .unwrap() as i8,
            ))
        }
    }

    fn is_foldable(inst: &Instruction) -> bool {
        matches!(
            inst.opcode,
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div
        ) && inst
            .operands
            .iter()
            .all(|op| matches!(op, Operand::Value(Value::Immediate(_))))
    }

    fn is_mul_power_of_two(inst: &Instruction) -> bool {
        inst.opcode == Opcode::Mul
            && inst.operands[0]
                .get_value()
                .map_or(false, |v| v.get_imm().is_none())
            && inst.operands[1].get_value().map_or(false, |v| {
                v.get_imm().map_or(false, |i| i.is_power_of_two().is_some())
            })
    }
}
