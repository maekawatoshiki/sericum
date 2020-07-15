use super::inst::*;
use crate::codegen::common::machine::{builder::*, function::*, module::*};
use crate::traits::pass::ModulePassTrait;
use rustc_hash::FxHashMap;
use std::mem;

pub struct TwoAddressConverter {}

impl ModulePassTrait for TwoAddressConverter {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "TwoAddressConverter"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl TwoAddressConverter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            if f.is_internal {
                continue;
            }
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        if f.is_internal {
            return;
        }

        let mut tied = vec![];

        for (_, bb) in f.body.basic_blocks.id_and_block() {
            for inst_id in &*bb.iseq_ref() {
                let inst = &mut f.body.inst_arena[*inst_id];

                // No tied values then skip
                if inst.tie.len() == 0 {
                    continue;
                }

                // Delete the map of tied registers by mem::replace.
                // Now that no registtters are tied.
                for (def, use_) in mem::replace(&mut inst.tie, FxHashMap::default()) {
                    tied.push((*inst_id, def, use_));
                }

                let opcode = inst.opcode;
                for (d, u) in &opcode.inst_def().unwrap().tie {
                    // Replace a tied use virtual register with a tied def virtual register.
                    // e.g.
                    //   before: v1 = add v2, 1 (tied: v1 and v2)
                    //   after:  v1 = add v1, 1
                    inst.replace_nth_operand_with(
                        &f.regs_info,
                        u.as_use(),
                        MachineOperand::Register(inst.def[d.as_def()]),
                    );
                }
            }
        }

        for (inst_id, def, use_) in tied {
            let inst_bb = f.body.inst_arena[inst_id].parent;

            // before: v1 = add v1, 1
            // after:  v1 = copy v2
            //         v1 = add v1, 1

            let copy = MachineInst::new_simple(
                MachineOpcode::Copy,
                vec![MachineOperand::Register(use_)],
                inst_bb,
            )
            .with_def(vec![def]);

            let mut builder = Builder::new(f);
            builder.set_insert_point_before_inst(inst_id).unwrap();
            builder.insert(copy);
        }
    }
}
