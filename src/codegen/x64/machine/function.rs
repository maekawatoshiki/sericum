use super::super::dag::function::*;
use super::{basic_block::*, frame_object::*, instr::*};
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

    /// Local variables types
    // pub locals_ty: Vec<Type>,
    pub local_mgr: LocalVariableManager,
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
            // locals_ty: f.locals_ty.clone(),
            local_mgr: f.local_mgr.clone(),
        }
    }

    pub fn find_instr_pos(&self, instr_id: MachineInstrId) -> Option<(MachineBasicBlockId, usize)> {
        for (bb_id, bb) in &self.basic_blocks {
            if let Some(pos) = bb.find_instr_pos(instr_id) {
                return Some((bb_id, pos));
            }
        }
        None
    }

    // pub fn add_
}
