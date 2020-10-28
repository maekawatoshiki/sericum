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
            for &inst_id in &*block.iseq.borrow() {
                let inst = &self.func.inst_table[inst_id];
                if inst.opcode == Opcode::GetElementPtr && self.able_to_be_sunk(inst) {
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
            let user_id = *gep
                .users
                .borrow()
                .iter()
                .min_by(|&&x, &&y| {
                    self.func
                        .find_inst_pos(x)
                        .unwrap()
                        .1
                        .cmp(&self.func.find_inst_pos(y).unwrap().1)
                })
                .unwrap();
            let user_parent = self.func.inst_table[user_id].parent;
            let gep = &mut self.func.inst_table[gep_id];
            gep.parent = user_parent;
            let mut builder = self.func.ir_builder();
            builder.set_insert_point_before_inst(user_id);
            builder.insert(gep_val);
        }
    }

    fn able_to_be_sunk(&self, inst: &Instruction) -> bool {
        inst.has_one_use() || {
            inst.users
                .borrow()
                .windows(2)
                .all(|us| self.func.inst_table[us[0]].parent == self.func.inst_table[us[1]].parent)
        }
    }
}
