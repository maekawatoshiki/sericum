use crate::codegen::arch::{dag::node::*, frame_object::*, machine::register::*};
use crate::codegen::common::dag::basic_block::*;
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
    pub dag_heap: DAGHeap,

    pub local_mgr: LocalVariables,

    pub regs_info: RegistersInfo,

    pub is_internal: bool,

    pub types: Types,
}

pub struct DAGHeap {
    // TODO: -> NodeHeap
    heap: RawAllocator<DAGNode>,
    node_none: Raw<DAGNode>,
    node_regs: [Option<Raw<DAGNode>>; PHYS_REGISTERS_NUM],
}

impl DAGFunction {
    pub fn new(
        func: &Function,
        dag_heap: DAGHeap,
        dag_basic_block_arena: Arena<DAGBasicBlock>,
        dag_basic_blocks: Vec<DAGBasicBlockId>,
        local_mgr: LocalVariables,
        regs_info: RegistersInfo,
    ) -> Self {
        Self {
            is_internal: func.is_internal,
            name: func.name.clone(),
            ty: func.ty.clone(),
            dag_basic_block_arena,
            dag_basic_blocks,
            dag_heap,
            local_mgr,
            regs_info,
            types: func.types.clone(),
        }
    }

    pub fn debug(&self, f: &mut fmt::Formatter, tys: &Types) -> fmt::Result {
        writeln!(
            f,
            "DAGFunc(name: {}, ty: {}):",
            self.name,
            tys.to_string(self.ty)
        )?;

        for bb_id in &self.dag_basic_blocks {
            let bb = &self.dag_basic_block_arena[*bb_id];
            bb.debug(f, tys, bb_id.index())?;
        }

        fmt::Result::Ok(())
    }
}

impl DAGHeap {
    pub fn new() -> Self {
        let mut heap = RawAllocator::new();
        Self {
            node_none: heap.alloc(DAGNode::new_none()),
            node_regs: [None; PHYS_REGISTERS_NUM],
            heap,
        }
    }

    pub fn alloc(&mut self, node: DAGNode) -> Raw<DAGNode> {
        self.heap.alloc(node)
    }

    pub fn alloc_none(&self) -> Raw<DAGNode> {
        self.node_none
    }

    pub fn alloc_phys_reg<T: TargetRegisterTrait>(
        &mut self,
        regs_info: &RegistersInfo,
        r: T,
    ) -> Raw<DAGNode> {
        let rn = r.as_phys_reg().retrieve();
        match self.node_regs[rn] {
            Some(node) => node,
            None => {
                let r = self.alloc(DAGNode::new_phys_reg(regs_info, r));
                self.node_regs[rn] = Some(r);
                r
            }
        }
    }
}
