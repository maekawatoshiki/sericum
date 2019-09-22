use super::{builder::*, function::*, instr::*, module::*};
use crate::ir::types::*;

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
        let mut info = vec![];

        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];

            for instr_id in &*bb.iseq_ref() {
                let instr = &mut f.instr_arena[*instr_id];

                if instr.tie.len() == 0 {
                    continue;
                }

                for (def, use_) in &mut instr.tie {
                    info.push((*instr_id, def.clone(), use_.clone()));
                    *use_ = def.clone();
                }
            }
        }

        for (instr_id, def, use_) in info {
            let cur_bb = {
                let instr = &mut f.instr_arena[instr_id];
                *instr
                    .operand
                    .iter_mut()
                    .find(|op| op.as_register() == &use_)
                    .unwrap() = MachineOperand::Register(def.clone());
                instr.parent
            };

            let old_instr = ::std::mem::replace(
                &mut f.instr_arena[instr_id],
                MachineInstr::new_with_def_reg(
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(use_)],
                    Type::Void, // TODO
                    vec![def],
                    cur_bb,
                ),
            );

            let instr = f.instr_arena.alloc(old_instr);

            let mut builder = Builder::new(f);
            builder.set_insert_point_after_instr(instr_id).unwrap();
            builder.insert_instr_id(instr);
        }
    }
}
