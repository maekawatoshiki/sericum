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
        let mut a = vec![];

        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];

            for instr_id in &*bb.iseq_ref() {
                let instr = &mut f.instr_arena[*instr_id];

                if instr.tie.len() == 0 {
                    continue;
                }

                for (def, use_) in &instr.tie {
                    a.push((*instr_id, def.clone(), use_.clone()));
                }

                instr.tie.clear();
            }
        }

        for (instr_id, def, use_) in a {
            let instr = &mut f.instr_arena[instr_id];
            *instr
                .operand
                .iter_mut()
                .find(|op| op.as_register() == &use_)
                .unwrap() = MachineOperand::Register(def.clone());

            let instr_copy = f.instr_arena.alloc(MachineInstr::new_with_def_reg(
                MachineOpcode::CopyToReg,
                vec![MachineOperand::Register(use_)],
                Type::Void, // TODO
                vec![def],
            ));

            let mut builder = Builder::new(f);
            builder.set_insert_point_before_instr(instr_id).unwrap();
            builder.insert_instr_id(instr_copy);
        }
    }
}
