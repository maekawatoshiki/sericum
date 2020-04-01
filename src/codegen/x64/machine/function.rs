use super::super::dag::function::*;
use super::super::register::*;
use super::{basic_block::*, frame_object::*, instr::*};
use crate::{codegen::is_internal_function, ir::types::*};
use id_arena::*;
use std::fmt;
use std::ops::{Index, IndexMut};

pub type MachineFunctionId = Id<MachineFunction>;

// TODO: Documents in detail
#[derive(Clone)]
pub struct MachineFunction {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// Machine Basic Blocks
    pub basic_blocks: MachineBasicBlocks,

    /// Instruction arena
    pub instr_arena: InstructionArena,

    /// True if internal function
    pub internal: bool,

    /// Local variables info
    pub local_mgr: LocalVariableManager,

    /// Virtual register generator
    pub vreg_gen: VirtRegGen,
}

#[derive(Debug, Clone)]
pub struct InstructionArena {
    pub arena: Arena<MachineInstr>,
}

impl MachineFunction {
    pub fn new(
        f: DAGFunction,
        basic_blocks: MachineBasicBlocks,
        instr_arena: InstructionArena,
    ) -> Self {
        Self {
            internal: is_internal_function(&f.name),
            name: f.name,
            ty: f.ty,
            instr_arena,
            basic_blocks,
            local_mgr: f.local_mgr,
            vreg_gen: f.vreg_gen,
        }
    }

    pub fn find_instr_pos(&self, inst_id: MachineInstrId) -> Option<(MachineBasicBlockId, usize)> {
        let parent = self.instr_arena[inst_id].parent;
        if let Some(pos) = self.basic_blocks.arena[parent].find_instr_pos(inst_id) {
            return Some((parent, pos));
        }
        None
    }

    pub fn remove_inst(&self, inst_id: MachineInstrId) {
        let (bb_id, pos) = self.find_instr_pos(inst_id).unwrap();
        self.basic_blocks.arena[bb_id].iseq_ref_mut().remove(pos);
    }

    pub fn get_entry_bb(&self) -> Option<&MachineBasicBlockId> {
        self.basic_blocks.order.get(0)
    }
}

impl InstructionArena {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn alloc(&mut self, mi: MachineInstr) -> Id<MachineInstr> {
        let mi_id = self.arena.alloc(mi);
        let mi = &mut self.arena[mi_id];
        mi.add_use(mi_id);
        mi.add_def(mi_id);
        mi_id
    }
}

impl Index<MachineInstrId> for InstructionArena {
    type Output = MachineInstr;

    fn index(&self, idx: MachineInstrId) -> &Self::Output {
        &self.arena[idx]
    }
}

impl IndexMut<MachineInstrId> for InstructionArena {
    fn index_mut(&mut self, idx: MachineInstrId) -> &mut Self::Output {
        &mut self.arena[idx]
    }
}

impl fmt::Debug for MachineFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "MachineFunction(name: {}, ty: {:?}):",
            self.name, self.ty
        )?;

        let mut idx = 0;
        for (id, bb) in self.basic_blocks.id_and_block() {
            writeln!(f, "MachineBasicBlock #{} ({:?})", id.index(), bb)?;
            for inst in &*bb.iseq_ref() {
                writeln!(f, "{: ^4}: {:?}", idx, self.instr_arena[*inst])?;
                idx += 1;
            }
        }

        fmt::Result::Ok(())
    }
}
