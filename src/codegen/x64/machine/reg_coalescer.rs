use super::{function::*, liveness::*, module::*};
use rustc_hash::FxHashMap;
// use crate::ir::types::*;

pub struct RegisterCoalescer {}

impl RegisterCoalescer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        let mut matrix = LivenessAnalysis::new().analyze_function(f);

        let mut removal_list = vec![];
        for (_, bb) in f.body.basic_blocks.id_and_block() {
            'a: for &id in &*bb.iseq_ref() {
                let inst = f.body.inst_arena[id].clone();

                if inst.opcode.is_copy_like()
                    && inst.def[0].is_vreg()
                    && inst.operand[0].is_register()
                    && inst.operand[0].as_register().is_vreg()
                {
                    if inst.operand[0].as_register().info_ref().use_list.len() != 1 {
                        continue;
                    }

                    let a = inst.def[0].info_ref().def_list.clone();
                    let b = inst.def[0].info_ref().use_list.clone();
                    for &def in &a {
                        if inst.parent != f.body.inst_arena[def].parent {
                            continue 'a;
                        }
                    }
                    for &use_ in &b {
                        if inst.parent != f.body.inst_arena[use_].parent {
                            continue 'a;
                        }
                    }
                    for def in a {
                        f.body.inst_arena[def].def[0]
                            .info_ref_mut()
                            .def_list
                            .remove(&def);
                        if def == id {
                            continue;
                        }

                        f.body.inst_arena[def].def[0] = inst.operand[0].as_register().clone();
                        f.body.inst_arena[def].def[0].add_def(def);
                    }
                    for use_ in b {
                        inst.operand[0]
                            .as_register()
                            .info_ref_mut()
                            .use_list
                            .remove(&use_);
                        inst.def[0].info_ref_mut().use_list.remove(&use_);
                        if use_ == id {
                            continue;
                        }

                        f.body.inst_arena[use_]
                            .replace_operand_reg(&inst.def[0], &inst.operand[0].as_register());
                        inst.operand[0].as_register().add_use(use_);
                    }
                    removal_list.push(id);
                    continue;
                }

                if inst.opcode.is_copy_like()
                    && inst.def[0].is_phys_reg()
                    && inst.operand[0].is_register()
                    && inst.operand[0].as_register().is_vreg()
                {
                    {
                        let a = inst.operand[0].as_register().info_ref().def_list.clone();
                        let b = inst.operand[0].as_register().info_ref().use_list.clone();
                        for &def in &a {
                            if inst.parent != f.body.inst_arena[def].parent {
                                continue 'a;
                            }
                        }
                        for &use_ in &b {
                            if inst.parent != f.body.inst_arena[use_].parent {
                                continue 'a;
                            }
                        }
                    }
                    for &def in &inst.operand[0].as_register().info_ref().def_list {
                        if def == id {
                            continue;
                        }

                        f.body.inst_arena[def].def[0] = inst.def[0].clone();
                        f.body.inst_arena[def].def[0].add_def(def);
                    }
                    for &use_ in &inst.operand[0].as_register().info_ref().use_list {
                        if use_ == id {
                            continue;
                        }

                        f.body.inst_arena[use_]
                            .replace_operand_reg(&inst.operand[0].as_register(), &inst.def[0]);
                        inst.def[0].add_use(use_);
                    }
                    removal_list.push(id);
                    continue;
                }
            }
        }

        for id in removal_list {
            f.remove_inst(id)
        }
    }
}
