use super::{function::DAGFunction, module::DAGModule, node::*};
use crate::ir::types::*;
use crate::util::allocator::*;
use rustc_hash::FxHashMap;

pub struct MISelector {}

impl MISelector {
    pub fn new() -> Self {
        Self {}
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
        node: Raw<DAGNode>,
    ) -> Raw<DAGNode> {
        let kind = match node.kind {
            NodeKind::MI(_) | NodeKind::Operand(_) | NodeKind::None => return node,
            NodeKind::IR(ref ir) => ir,
        };

        // match kind {
        //     IRNodeKind::Add => {}
        // }

        node
    }
}
