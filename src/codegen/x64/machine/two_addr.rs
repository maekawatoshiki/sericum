use super::{builder::*, function::*, instr::*, module::*};
use crate::ir::types::*;
use std::mem;

pub struct TwoAddressConverter {}

impl TwoAddressConverter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        let mut tied = vec![];

        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];

            for instr_id in &*bb.iseq_ref() {
                let instr = &mut f.instr_arena[*instr_id];

                // No tied values then skip
                if instr.tie.len() == 0 {
                    continue;
                }

                for (def, use_) in &mut instr.tie {
                    tied.push((*instr_id, def.clone(), use_.clone()));

                    // Tied registers are assigned to one register (def register).
                    // e.g.
                    //   before: v1 = add v2, 1 (tied: v1 and v2)
                    //   after:  v1 = add v1, 1
                    *instr
                        .operand
                        .iter_mut()
                        .find(|op| op.as_register() == use_)
                        .unwrap() = MachineOperand::Register(def.clone());
                }

                // Delete the map of all the tied registers because they are assigned to one
                // register so that no registers tied.
                instr.tie.clear();
            }
        }

        for (instr_id, def, use_) in tied {
            let instr_bb = f.instr_arena[instr_id].parent;

            // before: v1 = add v2, 1 (tied: v1 and v2)
            // after:  v1 = copy v2
            //         v1 = add v1, 1

            let old_instr = {
                let def_ty = def.info_ref().ty.clone();
                mem::replace(
                    &mut f.instr_arena[instr_id],
                    MachineInstr::new_with_def_reg(
                        MachineOpcode::Copy,
                        vec![MachineOperand::Register(use_)],
                        def_ty,
                        vec![def],
                        instr_bb,
                    ),
                )
            };

            let instr = f.instr_arena.alloc(old_instr);

            let mut builder = Builder::new(f);
            builder.set_insert_point_after_instr(instr_id).unwrap();
            builder.insert_instr_id(instr);
        }
    }
}
