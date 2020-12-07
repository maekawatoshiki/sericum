use crate::ir::{
    builder::IRBuilder,
    function::Function,
    module::Module,
    opcode::{Instruction, Opcode},
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
        let mut foldable_condbr = VecDeque::new();
        // TODO: Had better implement a conversion from Mul/Div to Shl/Shr in instcombine pass
        let mut to_shift = VecDeque::new();

        for &id in &self.cur_func.basic_blocks.order {
            let bb = &self.cur_func.basic_blocks.arena[id];
            for &inst_id in &*bb.iseq.borrow() {
                let inst = &self.cur_func.inst_table[inst_id];

                if Self::is_foldable(inst) {
                    foldable.push_back(inst_id);
                }

                if Self::is_foldable_condbr(inst) {
                    foldable_condbr.push_back(inst_id)
                }

                if Self::is_mul_power_of_two(inst) {
                    to_shift.push_back(inst_id);
                }
            }
        }

        while let Some(inst_id) = foldable_condbr.pop_front() {
            let inst = &self.cur_func.inst_table[inst_id];
            let cur_block = inst.parent;
            let cond = inst.operand.args()[0].as_imm().as_int1();
            let dst = inst.operand.blocks()[1 - cond as usize];
            let not_dst = inst.operand.blocks()[cond as usize];
            let (block, index) = self.cur_func.find_inst_pos(inst_id).unwrap();
            {
                let mut builder = self.cur_func.ir_builder();
                builder.set_insert_point_at(index, block);
                builder.build_br(dst);
            }
            self.cur_func
                .basic_block_ref_mut(cur_block)
                .succ
                .remove(&not_dst);
            self.cur_func
                .basic_block_ref_mut(not_dst)
                .pred
                .remove(&cur_block);
            self.cur_func.remove_inst(inst_id);
        }

        while let Some(inst_id) = foldable.pop_front() {
            let inst = &self.cur_func.inst_table[inst_id];
            let folded = match inst.fold_const() {
                Some(folded) => folded,
                None => continue,
            };
            let users = inst.users.clone();
            for &user_id in &*users.borrow() {
                Instruction::replace_operand_value(
                    &mut self.cur_func.inst_table,
                    user_id,
                    // TODO: Very inefficient!
                    &Value::new_inst(self.cur_func.id.unwrap(), inst_id),
                    folded,
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
            inst.operand.args_mut()[1] = Value::new_imm_int8(
                inst.operand.args()[1]
                    .get_imm()
                    .unwrap()
                    .is_power_of_two()
                    .unwrap() as i8,
            );
        }
    }

    fn is_foldable(inst: &Instruction) -> bool {
        matches!(
            inst.opcode,
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::ICmp | Opcode::Zext
        ) && inst
            .operand
            .args()
            .iter()
            .all(|op| matches!(op, Value::Immediate(_)))
    }

    fn is_foldable_condbr(inst: &Instruction) -> bool {
        matches!(inst.opcode, Opcode::CondBr)
            && inst
                .operand
                .args()
                .iter()
                .all(|op| matches!(op, Value::Immediate(_)))
    }

    fn is_mul_power_of_two(inst: &Instruction) -> bool {
        inst.opcode == Opcode::Mul
            && inst.operand.args()[0].get_imm().is_none()
            && inst.operand.args()[1]
                .get_imm()
                .map_or(false, |i| i.is_power_of_two().is_some())
    }
}
