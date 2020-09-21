use crate::ir::{builder::*, function::*, module::*, opcode::*};

pub struct CodegenPrepare {}

pub struct CodegenPrepareOnFunction<'a> {
    func: &'a mut Function,
}

impl CodegenPrepare {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }

            CodegenPrepareOnFunction { func }.run()
        }
    }
}

impl<'a> CodegenPrepareOnFunction<'a> {
    pub fn run(&mut self) {
        let mut geps_to_be_sunk = vec![];

        for (_, block) in &self.func.basic_blocks.arena {
            for inst_id in block.iseq.borrow().iter().map(|i| i.as_instruction().id) {
                let inst = &self.func.inst_table[inst_id];
                if inst.opcode == Opcode::GetElementPtr && inst.has_one_use() {
                    geps_to_be_sunk.push(inst_id);
                }
            }
        }

        debug!(println!(
            "CodegenPrepare: {} GEPs to be sunk",
            geps_to_be_sunk.len()
        ));

        for gep_id in geps_to_be_sunk {
            let gep_val = self.func.remove_inst_from_block(gep_id);
            let gep = &self.func.inst_table[gep_id];
            assert_eq!(gep.users.borrow().len(), 1);
            let user_id = *gep.users.borrow().iter().next().unwrap();
            let user_parent = self.func.inst_table[user_id].parent;
            let gep = &mut self.func.inst_table[gep_id];
            gep.parent = user_parent;
            let mut builder = Builder::new(FunctionEntity(self.func));
            builder.set_insert_point_before_inst(user_id);
            builder.insert(gep_val);
        }
    }
}
