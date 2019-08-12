use super::super::dag::function::*;
use super::super::register::*;
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

    /// Machine Basic blocks list
    pub basic_blocks: Vec<MachineBasicBlockId>,

    /// Machine Basic block arena
    pub basic_block_arena: Arena<MachineBasicBlock>,

    /// Instruction arena
    pub instr_arena: Arena<MachineInstr>,

    /// True if internal function
    pub internal: bool,

    /// Local variables info
    pub local_mgr: LocalVariableManager,

    /// Virtual register generator
    pub vreg_gen: VirtRegGen,
}

impl MachineFunction {
    pub fn new(
        f: DAGFunction,
        basic_block_arena: Arena<MachineBasicBlock>,
        basic_blocks: Vec<MachineBasicBlockId>,
        instr_arena: Arena<MachineInstr>,
    ) -> Self {
        Self {
            name: f.name,
            ty: f.ty,
            instr_arena,
            basic_block_arena,
            basic_blocks,
            internal: f.internal,
            local_mgr: f.local_mgr,
            vreg_gen: f.vreg_gen,
        }
    }

    pub fn find_instr_pos(&self, instr_id: MachineInstrId) -> Option<(MachineBasicBlockId, usize)> {
        for bb_id in &self.basic_blocks {
            let bb = &self.basic_block_arena[*bb_id];
            if let Some(pos) = bb.find_instr_pos(instr_id) {
                return Some((*bb_id, pos));
            }
        }
        None
    }

    // pub fn add_
}
