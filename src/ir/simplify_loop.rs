use crate::{
    analysis::dom_tree::{DominatorTree, DominatorTreeConstructor},
    ir::{
        basic_block::{BasicBlock, BasicBlockId},
        builder::{IRBuilder, IRBuilderWithFunction},
        function::Function,
        module::Module,
        opcode::{Instruction, Opcode, Operand},
    },
};

// Make sure a natural loop has a single backedge.
// TODO: Should this pass do pre-header insertion?
pub struct SimplifyLoop {}

pub struct SimplifyLoopOnFunction<'a> {
    func: &'a mut Function,
}

struct BackEdges {
    edges: Vec<BasicBlockId>,
    dest: BasicBlockId,
}

impl SimplifyLoop {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal || func.is_empty() {
                continue;
            }
            SimplifyLoopOnFunction::new(func).run();
        }
    }
}

impl<'a> SimplifyLoopOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    pub fn run(&mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();
        let backedges_to_merge = self.collect_backedges_to_merge(&dom_tree);
        for back_edges in backedges_to_merge {
            self.merge_backedges(back_edges);
        }
    }

    fn collect_backedges_to_merge(
        &mut self,
        dom_tree: &DominatorTree<BasicBlock>,
    ) -> Vec<BackEdges> {
        let post_order = dom_tree.post_ordered_blocks(None);
        let mut backedges_to_merge = vec![];

        for node in &post_order {
            let mut back_edges = vec![];
            let header = *node;

            let preds = &self.func.basic_blocks.arena[header].pred;
            for back_edge in preds {
                if dom_tree.dominate_bb(header, *back_edge)
                /* && back_edge is reachable from entry*/
                {
                    back_edges.push(*back_edge);
                }
            }

            if back_edges.len() > 1 {
                // multiple backedges
                backedges_to_merge.push(BackEdges {
                    edges: back_edges,
                    dest: header,
                });
            }
        }

        backedges_to_merge
    }

    fn merge_backedges(&mut self, back_edges: BackEdges) {
        let merge = self.func.append_basic_block_before(back_edges.edges[0]);

        for &edge in &back_edges.edges {
            let edge_ = &mut self.func.basic_blocks.arena[edge];
            edge_.succ.remove(&back_edges.dest);
            edge_.succ.insert(merge);

            for &id in edge_.iseq_ref().iter().rev() {
                if self.func.inst_table[id].opcode.is_terminator() {
                    Instruction::replace_operand(
                        &mut self.func.inst_table,
                        id,
                        &Operand::BasicBlock(back_edges.dest),
                        Operand::BasicBlock(merge),
                    );
                }
            }

            let merge_ = &mut self.func.basic_blocks.arena[merge];
            merge_.pred.insert(edge);
            let dest_ = &mut self.func.basic_blocks.arena[back_edges.dest];
            dest_.pred.remove(&edge);
        }

        let mut builder = IRBuilderWithFunction::new(self.func);
        builder.set_insert_point(merge);
        builder.build_br(back_edges.dest);

        self.move_phi(&back_edges, merge)
    }

    fn move_phi(&mut self, back_edges: &BackEdges, merge: BasicBlockId) {
        let dest_ = &mut self.func.basic_blocks.arena[back_edges.dest];
        let mut new_phi_incomings = vec![];

        for &id in &*dest_.iseq_ref() {
            if self.func.inst_table[id].opcode != Opcode::Phi {
                continue;
            }
            let inst = &mut self.func.inst_table[id];
            let mut phi_incomings = vec![];
            for i in (0..inst.operands.len()).step_by(2) {
                let val = inst.operands[i];
                let block = inst.operands[i + 1];
                if back_edges.edges.contains(block.as_basic_block()) {
                    phi_incomings.push((*val.as_value(), *block.as_basic_block()));
                }
            }
            for (_val, block) in &phi_incomings {
                let i = inst
                    .operands
                    .iter()
                    .position(|op| {
                        matches!(op, Operand::BasicBlock(_)) && op.as_basic_block() == block
                    })
                    .unwrap();
                inst.operands.remove(i); // remove block
                inst.operands.remove(i - 1); // remove val
            }
            if phi_incomings.len() > 0 {
                new_phi_incomings.push((id, phi_incomings));
            }
        }

        let mut builder = IRBuilderWithFunction::new(self.func);
        builder.set_insert_point_at(0, merge);

        for (phi, incomings) in new_phi_incomings {
            let new_phi = builder.build_phi(incomings);
            let phi = &mut builder.func_ref_mut().inst_table[phi];
            phi.operands.push(Operand::Value(new_phi));
            phi.operands.push(Operand::BasicBlock(merge));
        }
    }
}
