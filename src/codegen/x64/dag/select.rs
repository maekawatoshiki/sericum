use super::node::*;
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct SelectInstruction<'a> {
    pub module: &'a Module,
    // pub dag_arena: Arena<DAGNode>,
    // pub instr_id_to_dag_node_id: FxHashMap<InstructionId, DAGNodeId>,
}

impl<'a> SelectInstruction<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }
}
