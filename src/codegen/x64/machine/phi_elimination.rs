use super::super::dag::convert_machine::mov_rx;
use super::{basic_block::*, function::*, instr::*, module::*};
use rustc_hash::FxHashMap;

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
        // TODO: Rewrite with MachineInstr Builder
        let phi_pos = self.collect_phi(f);
        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];
            let (phi, pos) = if let Some(phi_pos) = phi_pos.get(bb_id) {
                (f.instr_arena[bb.iseq_ref()[*phi_pos]].clone(), *phi_pos)
            } else {
                continue;
            };
            let mut i = 0;
            while i < phi.operand.len() {
                let val = &phi.operand[i + 0];
                let bb = match phi.operand[i + 1] {
                    MachineOperand::Branch(bb) => bb,
                    _ => unreachable!(),
                };

                when_debug!(println!("PHI {:?}", val));

                let mut iseq = f.basic_block_arena[bb].iseq_ref_mut();
                assert!(
                    iseq.len() > 0 && f.instr_arena[*iseq.last().unwrap()].opcode.is_terminator()
                );

                let mut copy = MachineInstr::new(
                    &f.vreg_gen,
                    mov_rx(&val).unwrap(),
                    vec![val.clone()],
                    &phi.def[0].info_ref().ty,
                    bb,
                );
                copy.def = phi.def.clone();
                let id = f.instr_arena.alloc(copy);
                let pt = if iseq.len() >= 2 { iseq.len() - 1 } else { 0 };
                iseq.insert(pt, id);

                i += 2;
            }

            bb.iseq_ref_mut().remove(pos);
        }
    }

    fn collect_phi(&mut self, f: &MachineFunction) -> FxHashMap<MachineBasicBlockId, usize> {
        let mut phi_pos = FxHashMap::default();
        for bb_id in &f.basic_blocks {
            let bb = &f.basic_block_arena[*bb_id];
            for (i, instr) in bb.iseq_ref().iter().enumerate() {
                if f.instr_arena[*instr].opcode == MachineOpcode::Phi {
                    phi_pos.insert(*bb_id, i);
                }
            }
        }
        phi_pos
    }
}
