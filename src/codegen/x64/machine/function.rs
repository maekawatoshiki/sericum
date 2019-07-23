use super::{basic_block::*, instr::*};
use crate::ir::{function::*, types::*};
use id_arena::*;

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
}

impl MachineFunction {
    pub fn new(
        f: &Function,
        basic_blocks: Arena<MachineBasicBlock>,
        instr_arena: Arena<MachineInstr>,
    ) -> Self {
        Self {
            name: f.name.clone(),
            ty: f.ty.clone(),
            instr_arena,
            internal: f.internal,
            basic_blocks,
        }
    }
}
