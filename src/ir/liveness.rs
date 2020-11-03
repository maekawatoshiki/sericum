use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};
use rustc_hash::FxHashMap;

pub struct IRLivenessAnalyzer<'a> {
    module: &'a mut Module,
}

pub struct LivenessAnalyzerOnFunction<'a> {
    func: &'a Function,
    liveness: FxHashMap<BasicBlockId, LivenessInfo>,
}

impl<'a> IRLivenessAnalyzer<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        for (_, f) in &mut self.module.functions {
            if f.is_internal {
                continue;
            }

            let liveness = LivenessAnalyzerOnFunction::new(f).analyze();
            f.basic_blocks.liveness = liveness;
        }
    }
}

impl<'a> LivenessAnalyzerOnFunction<'a> {
    pub fn new(func: &'a Function) -> Self {
        Self {
            func,
            liveness: FxHashMap::default(),
        }
    }

    pub fn analyze(mut self) -> FxHashMap<BasicBlockId, LivenessInfo> {
        self.set_def();
        self.visit();
        self.liveness
    }

    pub fn set_def(&mut self) {
        for (id, block) in &self.func.basic_blocks.arena {
            let mut liveness = LivenessInfo::new();

            for &inst_id in &*block.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];

                if inst.opcode == Opcode::Call && self.func.get_return_type() != Type::Void {
                    liveness.def.insert(inst_id);
                    continue;
                }

                if inst.opcode.returns_value() {
                    liveness.def.insert(inst_id);
                }
            }

            self.liveness.insert(id, liveness);
        }
    }

    pub fn visit(&mut self) {
        for (bb_id, bb) in &self.func.basic_blocks.arena {
            for &inst_id in &*bb.iseq_ref() {
                let inst = &self.func.inst_table[inst_id];
                self.visit_operands(bb_id, &inst.operand, inst.opcode == Opcode::Phi);
            }
        }
    }

    pub fn visit_operands(&mut self, cur_bb: BasicBlockId, operand: &InstOperand, is_phi: bool) {
        for (i, v) in operand.args().iter().enumerate() {
            some_then!(
                id,
                v.get_inst_id(),
                self.propagate(
                    cur_bb,
                    id,
                    if is_phi {
                        Some(operand.blocks()[i])
                    } else {
                        None
                    },
                    is_phi
                )
            );
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
            let liveness = self.liveness.get_mut(&bb_id).unwrap();

            if liveness.def.contains(&inst_id) {
                return;
            }

            if !liveness.live_in.insert(inst_id) && !phi {
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
        if self
            .liveness
            .get_mut(&pred_id)
            .unwrap()
            .live_out
            .insert(inst_id)
        {
            // live_out didn't have the value inst_id
            self.propagate(pred_id, inst_id, None, phi);
        }
    }
}
