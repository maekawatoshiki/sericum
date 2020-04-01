use super::super::register::*;
use super::{basic_block::*, frame_object::*, node::*};
use crate::ir::{function::*, types::*};
use crate::util::allocator::*;
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
    pub dag_heap: RawAllocator<DAGNode>,

    // /// True if internal function
    // pub internal: bool,
    pub local_mgr: LocalVariables,

    /// Virtual register generator
    pub vreg_gen: VirtRegGen,
}

impl DAGFunction {
    pub fn new(
        func: &Function,
        dag_heap: RawAllocator<DAGNode>,
        dag_basic_block_arena: Arena<DAGBasicBlock>,
        dag_basic_blocks: Vec<DAGBasicBlockId>,
        local_mgr: LocalVariables,
        vreg_gen: VirtRegGen,
    ) -> Self {
        Self {
            name: func.name.clone(),
            ty: func.ty.clone(),
            dag_basic_block_arena,
            dag_basic_blocks,
            dag_heap,
            local_mgr,
            vreg_gen,
        }
    }
}

impl fmt::Debug for DAGFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DAGFunc(name: {}, ty: {:?}):", self.name, self.ty)?;

        for bb_id in &self.dag_basic_blocks {
            let bb = &self.dag_basic_block_arena[*bb_id];
            bb.debug(f, bb_id.index())?;
        }

        fmt::Result::Ok(())
    }
}
