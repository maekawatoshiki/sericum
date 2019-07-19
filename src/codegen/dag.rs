use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;

pub type DAGNodeId = Id<DAGNode>;

#[derive(Debug, Clone, PartialEq)]
pub struct DAGNode {
    pub kind: DAGNodeKind,
    pub ty: Type,
    pub next: Option<DAGNodeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DAGNodeKind {
    Load(DAGNodeId),
    Store(DAGNodeId, DAGNodeId),     // dst, src
    CopyToReg(DAGNodeId, DAGNodeId), // reg, val
    Register(RegisterKind),
    Constant(ConstantKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegisterKind {
    VReg(VirtualRegister),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantKind {
    Int32(i32),
}

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    // func_id: FunctionId,
    // cur_bb: Option<BasicBlockId>,
    // insert_point: usize,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn construct_dag(&self, func_id: FunctionId) {
        let func = self.module.function_ref(func_id);

        for (_, bb) in &func.basic_blocks {
            self.construct_dag_from_basic_block(bb);
        }
    }

    pub fn construct_dag_from_basic_block(&self, bb: &BasicBlock) -> DAGNode {
        DAGNode {
            kind: DAGNodeKind::Constant(ConstantKind::Int32(1)),
            ty: Type::Int32,
            next: None,
        }
    }
}
