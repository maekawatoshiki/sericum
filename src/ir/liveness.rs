use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};

pub struct IRLivenessAnalyzer<'a> {
    module: &'a Module,
}

impl<'a> IRLivenessAnalyzer<'a> {
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
        for (_, bb) in &f.basic_block_arena {
            let def = &mut bb.liveness.borrow_mut().def;

            for inst_val in &*bb.iseq.borrow() {
                let inst_id = inst_val.get_inst_id().unwrap();
                let inst = &f.inst_table[inst_id];

                // inst.operands[0];

                if inst.opcode == Opcode::Call
                    && self
                        .module
                        .types
                        .as_function_ty(inst.operands[0].as_value().get_type(self.module))
                        .unwrap()
                        .ret_ty
                        != Type::Void
                {
                    def.insert(inst_id);
                    continue;
                }

                if inst.opcode.returns_value() {
                    def.insert(inst_id);
                }
            }
        }
    }

    pub fn visit(&mut self, f: &Function) {
        for (bb_id, bb) in &f.basic_block_arena {
            for inst_val in &*bb.iseq.borrow() {
                let inst = &f.inst_table[inst_val.get_inst_id().unwrap()];

                self.visit_operands(f, bb_id, &inst.operands);
                // match &inst.opcode {
                //     Opcode::Call(func, args) => {
                //         some_then!(id, func.get_inst_id(), self.propagate(f, bb_id, id));
                //         for arg in args {
                //             some_then!(id, arg.get_inst_id(), self.propagate(f, bb_id, id));
                //         }
                //     }
                //     Opcode::CondBr(v, _, _) | Opcode::Ret(v) | Opcode::Load(v) => {
                //         some_then!(id, v.get_inst_id(), self.propagate(f, bb_id, id));
                //     }
                //     Opcode::Phi(vals) => {
                //         for (val, _) in vals {
                //             some_then!(id, val.get_inst_id(), self.propagate(f, bb_id, id));
                //         }
                //     }
                //     Opcode::GetElementPtr(base, idx) => {
                //         some_then!(id, base.get_inst_id(), self.propagate(f, bb_id, id));
                //         for idx in idx {
                //             some_then!(id, idx.get_inst_id(), self.propagate(f, bb_id, id));
                //         }
                //     }
                //     Opcode::Store(v1, v2)
                //     | Opcode::ICmp(_, v1, v2)
                //     | Opcode::Add(v1, v2)
                //     | Opcode::Sub(v1, v2) => {
                //         some_then!(id, v1.get_inst_id(), self.propagate(f, bb_id, id));
                //         some_then!(id, v2.get_inst_id(), self.propagate(f, bb_id, id));
                //     }
                //     _ => {}
                // }
            }
        }
    }

    pub fn visit_operands(&mut self, f: &Function, cur_bb: BasicBlockId, operands: &[Operand]) {
        for operand in operands {
            match operand {
                Operand::BasicBlock(_) => {}
                Operand::Type(_) => {}
                Operand::ICmpKind(_) => {}
                Operand::Value(v) => {
                    some_then!(id, v.get_inst_id(), self.propagate(f, cur_bb, id));
                }
            }
        }
    }

    pub fn propagate(&mut self, f: &Function, bb_id: BasicBlockId, inst_id: InstructionId) {
        let bb = &f.basic_block_arena[bb_id];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(&inst_id) {
                return;
            }

            if !bb_liveness.live_in.insert(inst_id) {
                // live_in already had the value inst_id
                return;
            }
        }

        for pred_id in &bb.pred {
            let pred = &f.basic_block_arena[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(inst_id) {
                // live_out didn't have the value inst_id
                self.propagate(f, *pred_id, inst_id);
            }
        }
    }
}
