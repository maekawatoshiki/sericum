use super::super::dag::function::*;
use super::{basic_block::*, instr::*};
use crate::ir::types::*;
use id_arena::*;

pub type MachineFunctionId = Id<MachineFunction>;

#[derive(Debug, Clone)]
pub struct MachineFunction {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// Machine Basic blocks
    pub basic_blocks: Arena<MachineBasicBlock>,

    /// Instruction arena
    pub instr_arena: Arena<MachineInstr>,

    /// True if internal function
    pub internal: bool,

    /// Objects on stack
    pub locals_ty: Vec<Type>,
}

impl MachineFunction {
    pub fn new(
        f: &DAGFunction,
        basic_blocks: Arena<MachineBasicBlock>,
        instr_arena: Arena<MachineInstr>,
    ) -> Self {
        Self {
            name: f.name.clone(),
            ty: f.ty.clone(),
            instr_arena,
            basic_blocks,
            internal: f.internal,
            locals_ty: f.locals_ty.clone(),
        }
    }
}
