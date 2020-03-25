use super::{builder::*, function::*, instr::*, module::*};
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

        for (_, bb) in f.basic_blocks.id_and_block() {
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

            // before: v1 = add v1, 1
            // after:  v1 = copy v2
            //         v1 = add v1, 1

            let old_instr = mem::replace(
                &mut f.instr_arena[instr_id],
                MachineInstr::new_simple(
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(use_)],
                    instr_bb,
                )
                .with_def(vec![def]),
            );

            let instr = f.instr_arena.alloc(old_instr);

            let mut builder = Builder::new(f);
            builder.set_insert_point_after_instr(instr_id).unwrap();
            builder.insert(instr);
        }
    }
}
