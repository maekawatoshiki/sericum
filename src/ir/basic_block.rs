use super::{function::Function, module::Module, opcode::*, types::Type};
use crate::traits::basic_block::*;
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Ref, RefCell, RefMut};

/// BasicBlockId that indicates a BasicBlock uniquely is given by [id_arena::Arena](https://docs.rs/id-arena).
pub type BasicBlockId = Id<BasicBlock>;

/// All information for `BasicBlock`s.
#[derive(Debug, Clone)]
pub struct BasicBlocks {
    /// Arena for Basic Block
    pub arena: Arena<BasicBlock>,

    /// Ordering of Basic Block
    pub order: Vec<BasicBlockId>,

    /// Liveness information
    pub liveness: FxHashMap<BasicBlockId, LivenessInfo>,
}

/// The representation of BasicBlock.
/// A BasicBlock has a set of Predecessors, Successors and a sequence of [Value](../value/enum.Value.html).
#[derive(Clone, Debug)]
pub struct BasicBlock {
    /// Predecessors
    pub pred: FxHashSet<BasicBlockId>,

    /// Successors
    pub succ: FxHashSet<BasicBlockId>,

    /// Instruction list
    pub iseq: RefCell<Vec<InstructionId>>,
}

/// Liveness analysis information for a basic block
/// See also [liveness analysis](../liveness/struct.IRLivenessAnalyzer.html#method.analyze) to know how it's used.
#[derive(Clone, Debug)]
pub struct LivenessInfo {
    /// Set of instructions executed in a basic block
    pub def: FxHashSet<InstructionId>,
    /// Set of live-in instructions
    pub live_in: FxHashSet<InstructionId>,
    /// Set of live-out instructions
    pub live_out: FxHashSet<InstructionId>,
}

impl BasicBlocks {
    /// Creates an empty BasicBlocks.
    ///
    /// # Examples
    ///
    /// ```
    /// use sericum::ir::basic_block::BasicBlocks;
    /// let bbs = BasicBlocks::new();
    ///
    /// assert!(bbs.arena.len() == 0);
    /// assert!(bbs.order.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            order: vec![],
            liveness: FxHashMap::default(),
        }
    }

    /// Makes an edge from `from` to `to`
    pub fn make_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        self.arena[from].succ.insert(to);
        self.arena[to].pred.insert(from);
    }

    /// Deletes an edge from `from` to `to`
    pub fn delete_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        self.arena[from].succ.remove(&to);
        self.arena[to].pred.remove(&from);
    }

    /// Removes a block
    pub fn remove_block(&mut self, block: BasicBlockId) {
        self.order.retain(|id| id != &block);
        for (_, block_) in &mut self.arena {
            block_.pred.remove(&block);
            block_.succ.remove(&block);
        }
    }

    /// Removes blocks
    pub fn remove_blocks(&mut self, blocks: &FxHashSet<BasicBlockId>) {
        self.order.retain(|id| !blocks.contains(id));
        for (_, block_) in &mut self.arena {
            for block in blocks {
                block_.pred.remove(block);
                block_.succ.remove(block);
            }
        }
    }
}

impl BasicBlock {
    /// Creates an empty BasicBlock
    ///
    /// # Examples
    ///
    /// ```
    /// use sericum::ir::basic_block::BasicBlock;
    ///
    /// let bb = BasicBlock::new();
    /// assert!(bb.iseq.borrow().is_empty());
    /// assert!(bb.pred.is_empty());
    /// assert!(bb.succ.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            iseq: RefCell::new(Vec::new()),
            pred: FxHashSet::default(),
            succ: FxHashSet::default(),
            // liveness: RefCell::new(LivenessInfo::new()),
        }
    }

    /// Get a reference to basicblock's instructions.
    pub fn iseq_ref<'b>(&'b self) -> Ref<Vec<InstructionId>> {
        self.iseq.borrow()
    }

    /// Get a mutable reference to basicblock's instructions.
    pub fn iseq_ref_mut(&self) -> RefMut<Vec<InstructionId>> {
        self.iseq.borrow_mut()
    }

    /// Try to find a position of given instruction in the vector of instructions that receiver has.
    pub fn find_inst_pos(&self, id2find: InstructionId) -> Option<usize> {
        self.iseq_ref().iter().position(|&id| id == id2find)
    }
}

impl BasicBlockTrait for BasicBlock {
    fn get_preds(&self) -> &FxHashSet<Id<Self>> {
        &self.pred
    }

    fn get_succs(&self) -> &FxHashSet<Id<Self>> {
        &self.succ
    }
}

impl BasicBlocksTrait for BasicBlocks {
    type BB = BasicBlock;

    fn get_arena(&self) -> &Arena<Self::BB> {
        &self.arena
    }

    fn get_order(&self) -> &Vec<Id<Self::BB>> {
        &self.order
    }
}

impl BasicBlock {
    pub fn dump2(&self, module: &Module, f: &Function) -> String {
        self.iseq_ref()
            .iter()
            .fold("".to_string(), |s, id| {
                let inst = &f.inst_table[*id];
                if inst.ty == Type::Void {
                    format!("{}    {}\n", s, inst.to_string(module))
                } else {
                    format!("{}    %{} = {}\n", s, id.index(), inst.to_string(module))
                }
            })
            .trim_end()
            .to_string()
    }
}

impl LivenessInfo {
    /// Creates an empty LivenessInfo
    ///
    /// # Examples
    ///
    /// ```
    /// use sericum::ir::basic_block::LivenessInfo;
    ///
    /// let info = LivenessInfo::new();
    /// assert!(info.def.is_empty());
    /// assert!(info.live_in.is_empty());
    /// assert!(info.live_out.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            def: FxHashSet::default(),
            live_in: FxHashSet::default(),
            live_out: FxHashSet::default(),
        }
    }

    /// Merge src into self
    pub fn merge(&mut self, src: LivenessInfo) {
        self.def = &self.def | &src.def;
        self.live_in = &self.live_in | &src.live_in;
        self.live_out = &self.live_out | &src.live_out;
    }
}
