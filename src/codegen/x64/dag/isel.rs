use super::{function::DAGFunction, module::DAGModule, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
use rustc_hash::FxHashMap;

pub struct MISelector {
    selected: FxHashMap<Raw<DAGNode>, Raw<DAGNode>>,
}

impl MISelector {
    pub fn new() -> Self {
        Self {
            selected: FxHashMap::default(),
        }
    }

    pub fn run_on_module(&mut self, module: &mut DAGModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(func)
        }
    }

    fn run_on_function(&mut self, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            self.run_on_node(&mut func.dag_heap, bb.entry.unwrap());
        }
    }

    fn run_on_node(
        &mut self,
        heap: &mut RawAllocator<DAGNode>,
        mut node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        // macro_rules! is_const {
        //     ($node:expr) => {
        //         $node.is_constant()
        //     };
        //     ($node:expr, $ty:expr) => {
        //         is_const!($node) && $node.ty == $ty
        //     };
        // }
        macro_rules! is_maybe_reg {
            ($node:expr) => {
                $node.is_maybe_register()
            };
            ($node:expr, $ty:expr) => {
                is_maybe_reg!($node) && $node.ty == $ty
            };
        }

        if !node.is_operation() {
            return node;
        }

        if let Some(node) = self.selected.get(&node) {
            return *node;
        }

        let mut selected = match node.kind {
            NodeKind::IR(IRNodeKind::Add) => {
                let op0 = self.run_on_node(heap, node.operand[0]);
                let op1 = self.run_on_node(heap, node.operand[1]);

                let kind = if is_maybe_reg!(node.operand[0], Type::Int32)
                    && is_maybe_reg!(node.operand[1], Type::Int32)
                {
                    // (Add $a:GR32 $b:GR32) => (ADDrr32 $a $b)
                    NodeKind::MI(MINodeKind::ADDrr32)
                } else {
                    NodeKind::IR(IRNodeKind::Add)
                };

                heap.alloc(DAGNode::new(kind, vec![op0, op1], node.ty.clone()))
            }
            _ => {
                node.operand = node
                    .operand
                    .iter()
                    .map(|op| self.run_on_node(heap, *op))
                    .collect();
                node
            }
        };

        self.selected.insert(node, selected);

        if let Some(next) = node.next {
            selected.next = Some(self.run_on_node(heap, next));
        }

        selected
    }
}
