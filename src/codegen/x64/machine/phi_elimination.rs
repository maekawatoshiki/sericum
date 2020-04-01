use super::{builder::*, function::*, instr::*, module::*};

pub struct PhiElimination {}

impl PhiElimination {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        let phi_list: Vec<_> = f
            .basic_blocks
            .id_and_block()
            .map(|(_, bb)| {
                bb.iseq_ref()
                    .iter()
                    .filter(|&&id| f.instr_arena[id].opcode == MachineOpcode::Phi)
                    .map(|&id| (id, f.instr_arena[id].clone()))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect();

        for (phi_id, phi) in phi_list {
            for i in (0..phi.operand.len()).step_by(2) {
                let val = &phi.operand[i + 0];
                let incoming_bb_id = phi.operand[i + 1].as_basic_block();

                let copy = MachineInstr::new_simple(
                    MachineOpcode::Copy,
                    vec![val.clone()],
                    incoming_bb_id,
                )
                .with_def(phi.def.clone());

                let mut builder = Builder::new(f);
                builder.set_insert_point_at_end(incoming_bb_id);
                builder.back_insert_point_while(|i| i.opcode.is_terminator());
                builder.insert(copy);
            }
            f.remove_inst(phi_id);
        }
    }
}
