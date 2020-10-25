use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};

pub struct IRLivenessAnalyzer<'a> {
    module: &'a Module,
}

pub struct LivenessAnalyzerOnFunction<'a> {
    func: &'a Function,
}

impl<'a> IRLivenessAnalyzer<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        for (_, f) in &self.module.functions {
            if f.is_internal {
                continue;
            }

            LivenessAnalyzerOnFunction::new(f).analyze();
        }
    }
}

impl<'a> LivenessAnalyzerOnFunction<'a> {
    pub fn new(func: &'a Function) -> Self {
        Self { func }
    }

    pub fn analyze(&mut self) {
        self.set_def();
        self.visit();
    }

    pub fn set_def(&mut self) {
        for (_, bb) in &self.func.basic_blocks.arena {
            *bb.liveness.borrow_mut() = LivenessInfo::new();

            let def = &mut bb.liveness.borrow_mut().def;

            for &inst_id in &*bb.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];

                if inst.opcode == Opcode::Call && self.func.get_return_type() != Type::Void {
                    def.insert(inst_id);
                    continue;
                }

                if inst.opcode.returns_value() {
                    def.insert(inst_id);
                }
            }
        }
    }

    pub fn visit(&mut self) {
        for (bb_id, bb) in &self.func.basic_blocks.arena {
            for &inst_id in &*bb.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];
                self.visit_operands(bb_id, &inst.operands, inst.opcode == Opcode::Phi);

                for (i, v) in inst.operand.args().iter().enumerate() {
                    some_then!(
                        id,
                        v.get_inst_id(),
                        self.propagate(
                            bb_id,
                            id,
                            if inst.opcode == Opcode::Phi {
                                Some(inst.operand.blocks()[i])
                            } else {
                                None
                            },
                            inst.opcode == Opcode::Phi
                        )
                    );
                }
            }
        }
    }

    pub fn visit_operands(&mut self, cur_bb: BasicBlockId, operands: &[Operand], is_phi: bool) {
        for i in 0..operands.len() {
            match &operands[i] {
                Operand::BasicBlock(_)
                | Operand::Type(_)
                | Operand::ICmpKind(_)
                | Operand::FCmpKind(_) => {}
                Operand::Value(v) if is_phi => some_then!(
                    id,
                    v.get_inst_id(),
                    self.propagate(cur_bb, id, Some(*operands[i + 1].as_basic_block()), true)
                ),
                Operand::Value(v) => {
                    some_then!(id, v.get_inst_id(), self.propagate(cur_bb, id, None, false));
                }
            }
        }
    }

    pub fn propagate(
        &mut self,
        bb_id: BasicBlockId,
        inst_id: InstructionId,
        phi_incoming: Option<BasicBlockId>,
        phi: bool,
    ) {
        let bb = &self.func.basic_blocks.arena[bb_id];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(&inst_id) {
                return;
            }

            if !bb_liveness.live_in.insert(inst_id) && !phi {
                // live_in already had the value inst_id
                return;
            }
        }

        if let Some(phi_incoming) = phi_incoming {
            self.propagate_if_necessary(phi_incoming, inst_id, phi);
        } else {
            for pred_id in &bb.pred {
                self.propagate_if_necessary(*pred_id, inst_id, phi);
            }
        }
    }

    fn propagate_if_necessary(&mut self, pred_id: BasicBlockId, inst_id: InstructionId, phi: bool) {
        let pred = &self.func.basic_blocks.arena[pred_id];
        if pred.liveness.borrow_mut().live_out.insert(inst_id) {
            // live_out didn't have the value inst_id
            self.propagate(pred_id, inst_id, None, phi);
        }
    }
}
