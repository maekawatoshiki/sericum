use super::{basic_block::*, node::*};
use crate::ir::{function::*, types::*};
use id_arena::*;

pub type DAGFunctionId = Id<DAGFunction>;

#[derive(Debug, Clone)]
pub struct DAGFunction {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// DAG Basic blocks
    pub dag_basic_blocks: Arena<DAGBasicBlock>,

    /// DAG node arena
    pub dag_arena: Arena<DAGNode>,

    /// True if internal function
    pub internal: bool,

    pub locals_ty: Vec<Type>,
}

impl DAGFunction {
    pub fn new(
        func: &Function,
        dag_arena: Arena<DAGNode>,
        dag_basic_blocks: Arena<DAGBasicBlock>,
        locals_ty: Vec<Type>,
    ) -> Self {
        Self {
            name: func.name.clone(),
            ty: func.ty.clone(),
            dag_basic_blocks,
            dag_arena,
            internal: func.internal,
            locals_ty,
        }
    }
}
