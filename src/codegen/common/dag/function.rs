use crate::codegen::arch::{frame_object::*, machine::register::*};
use crate::codegen::common::dag::{basic_block::*, node::*};
use crate::ir::{function::*, types::*};
use id_arena::*;
use std::fmt;

pub type DAGFunctionId = Id<DAGFunction>;

pub struct DAGFunction {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// DAG Basic block arena
    pub dag_basic_block_arena: Arena<DAGBasicBlock>,

    /// DAG Basic blocks list
    pub dag_basic_blocks: Vec<DAGBasicBlockId>,

    /// DAG node arena
    pub node_arena: Arena<Node>,

    pub local_vars: LocalVariables,

    pub regs: RegistersInfo,

    pub is_internal: bool,

    pub types: Types,
}

impl DAGFunction {
    pub fn new(
        func: &Function,
        node_arena: Arena<Node>,
        dag_basic_block_arena: Arena<DAGBasicBlock>,
        dag_basic_blocks: Vec<DAGBasicBlockId>,
        local_vars: LocalVariables,
        regs: RegistersInfo,
    ) -> Self {
        Self {
            is_internal: func.is_internal,
            name: func.name.clone(),
            ty: func.ty.clone(),
            dag_basic_block_arena,
            dag_basic_blocks,
            node_arena,
            local_vars,
            regs,
            types: func.types.clone(),
        }
    }

    pub fn debug(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "DAGFunc(name: {}, ty: {}):",
            self.name,
            self.types.to_string(self.ty)
        )?;

        for bb_id in &self.dag_basic_blocks {
            let bb = &self.dag_basic_block_arena[*bb_id];
            bb.debug(f, &self.node_arena, &self.types, bb_id.index())?;
        }

        fmt::Result::Ok(())
    }
}
