use super::{function::*, instr::*};
// use super::{convert::*, node::*};
// use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct PhysicalRegisterAllocator {}

impl PhysicalRegisterAllocator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_function(&mut self, cur_func: &MachineFunction) {
        self.collect_regs(cur_func);
        self.scan(cur_func);
    }

    fn scan(&mut self, cur_func: &MachineFunction) {
        let mut used = FxHashMap::default();
        for (_, bb) in &cur_func.basic_blocks {
            for instr_id in &bb.iseq {
                self.scan_on_instr(cur_func, &mut used, *instr_id);
            }
        }
    }

    fn scan_on_instr(
        &mut self,
        cur_func: &MachineFunction,
        used: &mut FxHashMap<usize, MachineInstrId>,
        instr_id: MachineInstrId,
    ) {
        let instr = &cur_func.instr_arena[instr_id];
        let num_reg = 4;

        if instr.reg.borrow().last_use.is_none() {
            return;
        }

        let mut found = false;

        for i in 0..num_reg - 1 {
            if used.contains_key(&i) {
                let target_last_use_id = cur_func.instr_arena[*used.get(&i).unwrap()]
                    .reg
                    .borrow()
                    .last_use
                    .unwrap();
                let target_last_use = cur_func.instr_arena[target_last_use_id].reg.borrow().vreg;
                if instr.reg.borrow().vreg < target_last_use {
                    continue;
                }
            }

            instr.set_phy_reg(i, false);
            used.insert(i, instr_id);
            found = true;
            break;
        }

        if found {
            return;
        }

        // TODO: Spill
    }

    fn collect_regs(&mut self, cur_func: &MachineFunction) {
        for (_, bb) in &cur_func.basic_blocks {
            let mut last_instr = None;

            for instr_id in &bb.iseq {
                self.collect_regs_on_instr(cur_func, *instr_id);
                last_instr = Some(*instr_id);
            }

            for out in &bb.liveness.borrow().live_out {
                cur_func.instr_arena[*out].set_last_use(last_instr);
            }
        }
    }

    fn collect_regs_on_instr(&mut self, cur_func: &MachineFunction, instr_id: MachineInstrId) {
        let instr = &cur_func.instr_arena[instr_id];
        for operand in &instr.oprand {
            match_then!(
                MachineOprand::Instr(id),
                operand,
                cur_func.instr_arena[*id].set_last_use(Some(instr_id))
            );
        }
    }
}
