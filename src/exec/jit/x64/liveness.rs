use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};
use id_arena::*;

pub struct LivenessAnalyzer<'a> {
    pub module: &'a mut Module,
}

impl<'a> LivenessAnalyzer<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        let mut fs = self.module.functions.clone();
        for (_, f) in &mut fs {
            let mut bbs = f.basic_blocks.clone();
            for (bb_id, bb) in &mut bbs {
                self.set_def(&f, bb);
            }
            for (bb_id, bb) in &bbs.clone() {
                for instr in bb.iseq.clone() {
                    self.visit(
                        bb_id,
                        &mut bbs,
                        f.instr_table[instr.get_instr_id().unwrap()].clone(),
                    );
                }
            }
            f.basic_blocks = bbs;
        }
        self.module.functions = fs;
    }

    pub fn set_def(&mut self, f: &Function, bb: &mut BasicBlock) {
        for instr_val in bb.iseq.clone() {
            let instr_id = instr_val.get_instr_id().unwrap();
            let instr = &f.instr_table[instr_id];
            match instr.opcode {
                Opcode::Add(_, _)
                | Opcode::Alloca(_)
                | Opcode::ICmp(_, _, _)
                | Opcode::Load(_)
                | Opcode::Phi(_)
                | Opcode::Sub(_, _) => {
                    bb.def.insert(instr_id);
                }
                Opcode::Call(f, _) => {
                    if f.get_type(&self.module).get_function_ty().unwrap().ret_ty != Type::Void {
                        bb.def.insert(instr_id);
                    }
                }
                Opcode::Store(_, dst) => {
                    if let Some(id) = dst.get_instr_id() {
                        bb.def.insert(id);
                    }
                }
                Opcode::Br(_) | Opcode::CondBr(_, _, _) | Opcode::Ret(_) => {}
            }
        }
    }

    pub fn visit(&mut self, bb_id: BasicBlockId, bb: &mut Arena<BasicBlock>, instr: Instruction) {
        match instr.opcode {
            Opcode::Call(func, args) => {
                if let Some(id) = func.get_instr_id() {
                    self.propagate(bb_id, bb, id);
                }
                for arg in &args {
                    if let Some(id) = arg.get_instr_id() {
                        self.propagate(bb_id, bb, id);
                    }
                }
            }
            Opcode::CondBr(v, _, _) | Opcode::Ret(v) | Opcode::Load(v) => {
                if let Some(id) = v.get_instr_id() {
                    self.propagate(bb_id, bb, id);
                }
            }
            Opcode::Phi(vals) => {
                for (val, _) in &vals {
                    if let Some(id) = val.get_instr_id() {
                        self.propagate(bb_id, bb, id);
                    }
                }
            }
            Opcode::Store(v1, v2)
            | Opcode::ICmp(_, v1, v2)
            | Opcode::Add(v1, v2)
            | Opcode::Sub(v1, v2) => {
                if let Some(id) = v1.get_instr_id() {
                    self.propagate(bb_id, bb, id);
                }
                if let Some(id) = v2.get_instr_id() {
                    self.propagate(bb_id, bb, id);
                }
            }
            _ => {}
        }
    }

    pub fn propagate(
        &mut self,
        bb_id: BasicBlockId,
        bbs: &mut Arena<BasicBlock>,
        instr_id: InstructionId,
    ) {
        let bb = &mut bbs[bb_id];
        if bb.def.contains(&instr_id) {
            return;
        }

        if bb.live_in.contains(&instr_id) {
            return;
        } else {
            bb.live_in.insert(instr_id);
        }

        for pred_id in bb.pred.clone() {
            let pred = &mut bbs[pred_id];
            if !pred.live_out.contains(&instr_id) {
                pred.live_out.insert(instr_id);
                self.propagate(pred_id, bbs, instr_id);
            }
        }
    }
}
