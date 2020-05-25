use crate::ir::{
    function::Function,
    module::Module,
    opcode::{Instruction, InstructionId, Opcode, Operand},
    value::{InstructionValue, Value},
};
use id_arena::Arena;
use rustc_hash::FxHashMap;

pub struct CommonSubexprElimination {}

struct LocalCommonSubexprEliminationOnFunction<'a> {
    func: &'a mut Function,
}

impl CommonSubexprElimination {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }

            LocalCommonSubexprEliminationOnFunction { func }.run()
        }
    }
}

impl<'a> LocalCommonSubexprEliminationOnFunction<'a> {
    pub fn run(&mut self) {
        let mut removal_list = vec![];

        for id in &self.func.basic_blocks.order {
            let bb = &self.func.basic_blocks.arena[*id];
            let mut commons: FxHashMap<Opcode, FxHashMap<Vec<Operand>, InstructionId>> =
                FxHashMap::default();
            fn find_common<'a>(
                commons: &'a mut FxHashMap<Opcode, FxHashMap<Vec<Operand>, InstructionId>>,
                arena: &mut Arena<Instruction>,
                inst_id: &InstructionId,
            ) -> Option<&'a InstructionId> {
                let inst = &arena[*inst_id];
                let opcode = inst.opcode;
                commons
                    .get(&opcode)
                    .map_or(None, |map| map.get(&inst.operands))
            };

            for inst_id in bb.iseq.borrow().iter().map(|v| v.as_instruction().id) {
                if let Some(common) = find_common(&mut commons, &mut self.func.inst_table, &inst_id)
                {
                    let common_val = Value::Instruction(InstructionValue {
                        id: *common,
                        func_id: self.func.id.unwrap(),
                    });
                    Instruction::replace_all_uses(
                        &mut self.func.inst_table,
                        inst_id,
                        Operand::Value(common_val),
                    );
                    removal_list.push(inst_id);
                    continue;
                }

                let inst = &self.func.inst_table[inst_id];
                if !matches!(
                    inst.opcode,
                    Opcode::GetElementPtr
                        | Opcode::Add
                        | Opcode::Sub
                        | Opcode::Mul
                        | Opcode::Div
                        | Opcode::Rem
                        | Opcode::Phi
                ) {
                    continue;
                }
                commons
                    .entry(inst.opcode)
                    .or_insert(FxHashMap::default())
                    .entry(inst.operands.clone())
                    .or_insert(inst_id);
            }
        }

        debug!(println!("{} insts removed", removal_list.len()));

        for remove in removal_list {
            self.func.remove_inst(remove);
        }
    }
}
