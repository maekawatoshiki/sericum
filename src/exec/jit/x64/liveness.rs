use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};
// use std::collections::hash_set::

pub struct LivenessAnalyzer<'a> {
    module: &'a Module,
}

impl<'a> LivenessAnalyzer<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        for (_, f) in &self.module.functions {
            self.set_def(f);
            self.visit(&f);
        }
    }

    pub fn set_def(&mut self, f: &Function) {
        for (_, bb) in &f.basic_blocks {
            let def = &mut bb.liveness.borrow_mut().def;
            for instr_val in &bb.iseq {
                let instr = &f.instr_table[instr_val.get_instr_id().unwrap()];
                let idx = instr.unique_idx;
                match instr.opcode {
                    Opcode::Add(_, _)
                    | Opcode::Alloca(_)
                    | Opcode::ICmp(_, _, _)
                    | Opcode::Load(_)
                    | Opcode::Phi(_)
                    | Opcode::Sub(_, _) => {
                        def.insert(idx);
                    }
                    Opcode::Call(f, _) => {
                        if f.get_type(&self.module).get_function_ty().unwrap().ret_ty != Type::Void
                        {
                            def.insert(idx);
                        }
                    }
                    Opcode::Store(_, dst) => {
                        some_then!(id, dst.get_instr_id(), {
                            def.insert(f.instr_table[id].unique_idx);
                        });
                    }
                    Opcode::Br(_) | Opcode::CondBr(_, _, _) | Opcode::Ret(_) => {}
                }
            }
        }
    }

    pub fn visit(&mut self, f: &Function) {
        for (bb_id, bb) in &f.basic_blocks {
            for instr_val in &bb.iseq {
                let instr = &f.instr_table[instr_val.get_instr_id().unwrap()];
                match &instr.opcode {
                    Opcode::Call(func, args) => {
                        some_then!(id, func.get_instr_id(), self.propagate(f, bb_id, id));
                        for arg in args {
                            some_then!(id, arg.get_instr_id(), self.propagate(f, bb_id, id));
                        }
                    }
                    Opcode::CondBr(v, _, _) | Opcode::Ret(v) | Opcode::Load(v) => {
                        some_then!(id, v.get_instr_id(), self.propagate(f, bb_id, id));
                    }
                    Opcode::Phi(vals) => {
                        for (val, _) in vals {
                            some_then!(id, val.get_instr_id(), self.propagate(f, bb_id, id));
                        }
                    }
                    Opcode::Store(v1, v2)
                    | Opcode::ICmp(_, v1, v2)
                    | Opcode::Add(v1, v2)
                    | Opcode::Sub(v1, v2) => {
                        some_then!(id, v1.get_instr_id(), self.propagate(f, bb_id, id));
                        some_then!(id, v2.get_instr_id(), self.propagate(f, bb_id, id));
                    }
                    _ => {}
                }
            }
        }
    }

    pub fn propagate(&mut self, f: &Function, bb_id: BasicBlockId, instr_id: InstructionId) {
        let bb = &f.basic_blocks[bb_id];
        let idx = f.instr_table[instr_id].unique_idx;

        let mut bb_liveness = bb.liveness.borrow_mut();

        if bb_liveness.def.contains(&idx) {
            return;
        }

        if !bb_liveness.live_in.insert(idx) {
            // live_in already had the value idx
            return;
        }

        for pred_id in &bb.pred {
            let pred = &f.basic_blocks[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(idx) {
                // live_out didn't have the value idx
                self.propagate(f, *pred_id, instr_id);
            }
        }
    }
}
