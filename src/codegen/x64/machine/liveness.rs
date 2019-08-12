use super::{basic_block::*, function::*, instr::*, module::*};
use crate::ir::types::*;
// use super::{convert::*, node::*};
// use id_arena::*;

pub struct LivenessAnalyzer<'a> {
    pub module: &'a MachineModule,
}

pub struct LivenessAnalysis<'a> {
    pub module: &'a MachineModule, // TODO: Will be used to get type
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self { module }
    }

    pub fn analyze_module(&mut self) {
        for (_, func) in &self.module.functions {
            self.analyze_function(func);
        }
    }

    pub fn analyze_function(&mut self, cur_func: &MachineFunction) {
        self.set_def(cur_func);
        self.visit(cur_func);
    }

    fn set_def(&mut self, cur_func: &MachineFunction) {
        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            for instr_id in &*bb.iseq_ref() {
                self.set_def_instr(cur_func, bb, *instr_id);
            }
        }
    }

    fn set_def_instr(
        &mut self,
        cur_func: &MachineFunction,
        bb: &MachineBasicBlock,
        instr_id: MachineInstrId,
    ) {
        let instr = &cur_func.instr_arena[instr_id];

        if instr.def.len() > 0 {
            bb.liveness.borrow_mut().def.insert(instr.def[0].clone());
        }
    }

    fn visit(&mut self, cur_func: &MachineFunction) {
        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            for instr_id in &*bb.iseq_ref() {
                self.visit_instr(cur_func, *bb_id, *instr_id);
            }
        }
    }

    fn visit_instr(
        &mut self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        instr_id: MachineInstrId,
    ) {
        let instr = &cur_func.instr_arena[instr_id];
        for operand in &instr.operand {
            match_then!(
                MachineOperand::Register(reg),
                operand,
                self.propagate(cur_func, bb, reg)
            );
        }
    }

    fn propagate(
        &self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        reg: &MachineRegister,
    ) {
        let bb = &cur_func.basic_block_arena[bb];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(reg) {
                return;
            }

            if !bb_liveness.live_in.insert(reg.clone()) {
                // live_in already had the value
                return;
            }
        }

        for pred_id in &bb.pred {
            let pred = &cur_func.basic_block_arena[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(reg.clone()) {
                // live_out didn't have the value instr_id
                self.propagate(cur_func, *pred_id, reg);
            }
        }
    }
}
