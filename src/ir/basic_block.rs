use super::{function::Function, module::Module, opcode::*, types::Type};
use crate::traits::basic_block::*;
use id_arena::*;
use rustc_hash::FxHashSet;
use std::cell::{Ref, RefCell, RefMut};

/// BasicBlockId that indicates a BasicBlock uniquely is given by [id_arena::Arena](https://docs.rs/id-arena).
pub type BasicBlockId = Id<BasicBlock>;

/// An allocator of BasicBlock and a sequence of BasicBlock.
/// See also [Builder](../builder/struct.Builder.html) to know how it's used.
#[derive(Debug, Clone)]
pub struct BasicBlocks {
    pub arena: Arena<BasicBlock>,
    pub order: Vec<BasicBlockId>,
}

/// The representation of BasicBlock.
/// A BasicBlock has a set of Predecessors, Successors and a sequence of [Value](../value/enum.Value.html).
#[derive(Clone, Debug)]
pub struct BasicBlock {
    /// Information for liveness analysis
    // TODO: Will be removed
    pub liveness: RefCell<LivenessInfo>,

    /// Predecessors
    pub pred: FxHashSet<BasicBlockId>,

    /// Successors
    pub succ: FxHashSet<BasicBlockId>,

    /// Instruction list
    pub iseq: RefCell<Vec<InstructionId>>,
}

/// The information of Liveness Analysis.
/// See also [liveness analysis](../liveness/struct.IRLivenessAnalyzer.html#method.analyze) to know how it's used.
#[derive(Clone, Debug)]
pub struct LivenessInfo {
    /// The set of variables that are used in an instruction.
    pub def: FxHashSet<InstructionId>,
    /// The set of "in" information that is living at before an instruction executing.
    pub live_in: FxHashSet<InstructionId>,
    /// The set of "out" information that is living at after executed an instruction.
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
        }
    }

    /// Let src-basicblock to absorb into dst basicblock.
    pub fn merge(&mut self, dst: &BasicBlockId, src: &BasicBlockId) {
        let src_block = ::std::mem::replace(&mut self.arena[*src], BasicBlock::new());
        let BasicBlock {
            liveness: src_liveness,
            succ: src_succ,
            iseq: src_iseq,
            ..
        } = src_block;
        for &succ in &src_succ {
            self.arena[succ].pred.remove(src);
            self.arena[succ].pred.insert(*dst);
        }
        let dst_block = &mut self.arena[*dst];
        dst_block.succ = src_succ;
        dst_block
            .iseq
            .borrow_mut()
            .append(&mut src_iseq.borrow_mut());
        dst_block
            .liveness
            .borrow_mut()
            .merge(src_liveness.into_inner());
        self.order.retain(|bb| bb != src);
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
            liveness: RefCell::new(LivenessInfo::new()),
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
