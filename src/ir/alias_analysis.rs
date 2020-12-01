use crate::ir::{
    function::Function,
    module::Module,
    opcode::Opcode,
    value::{InstructionValue, Value},
};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct AliasAnalysis {}

struct AliasAnalysisOnFunction<'a> {
    func: &'a Function,
}

impl AliasAnalysis {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            AliasAnalysisOnFunction { func }.run()
        }
    }
}

impl<'a> AliasAnalysisOnFunction<'a> {
    pub fn run(&mut self) {
        let mut alloca = FxHashSet::default();
        let mut points_to = FxHashMap::default();

        for &block_id in &self.func.basic_blocks.order {
            let block = &self.func.basic_blocks.arena[block_id];
            for &inst_id in &*block.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];

                match inst.opcode {
                    Opcode::Alloca => {
                        alloca.insert(inst_id);
                    }
                    Opcode::Load => {
                        let mem = inst.operand.args()[0];
                        match mem {
                            Value::Instruction(InstructionValue { id, .. })
                                if alloca.contains(&id) =>
                            {
                                points_to.insert(inst_id, id);
                            }
                            Value::Instruction(InstructionValue { id, .. })
                                if points_to.contains_key(&id) =>
                            {
                                points_to.insert(inst_id, id);
                            }
                            _ => {
                                todo!("???")
                            }
                        }
                    }
                    _ => {
                        panic!()
                    } // Opcode
                }
            }
        }
    }
}
