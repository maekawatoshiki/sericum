use super::dom_tree::DominatorTree;
use crate::traits::basic_block::{BasicBlockTrait, BasicBlocksTrait};
use id_arena::{Arena, Id};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

pub struct LoopsConstructor<'a, BBS: BasicBlocksTrait> {
    dom_tree: &'a DominatorTree<BBS::BB>,
    loops: Loops<BBS::BB>,
    basic_blocks: &'a BBS,
}

#[derive(Debug)]
pub struct Loops<BB: BasicBlockTrait> {
    pub arena: Arena<Loop<BB>>,
    pub bb_to_loop: FxHashMap<Id<BB>, Id<Loop<BB>>>,
    pub top_level_loops: Vec<Id<Loop<BB>>>,
}

#[derive(Debug)]
pub struct Loop<BB: BasicBlockTrait> {
    parent: Option<Id<Loop<BB>>>,
    header: Id<BB>,
    sub_loops: Vec<Id<Loop<BB>>>,
    set: FxHashSet<Id<BB>>,
}

impl<'a, BBS: BasicBlocksTrait> LoopsConstructor<'a, BBS> {
    pub fn new(dom_tree: &'a DominatorTree<BBS::BB>, basic_blocks: &'a BBS) -> Self {
        Self {
            dom_tree,
            basic_blocks,
            loops: Loops::new(),
        }
    }

    pub fn analyze(mut self) -> Loops<BBS::BB> {
        let post_order = self.get_post_ordered_blocks(self.dom_tree.root.unwrap());

        for node in &post_order {
            let mut back_edges = vec![];
            let header = *node;

            let preds = self.basic_blocks.get_arena()[header].get_preds();
            for back_edge in preds {
                if self.dom_tree.dominate_bb(header, *back_edge)
                /* && back_edge is reachable from entry*/
                {
                    back_edges.push(*back_edge);
                }
            }

            if back_edges.len() > 0 {
                let loop_id = self.loops.new_loop(header);
                self.discover_and_map_sub_loop(loop_id, &back_edges);
            }
        }

        self.insert_blocks_into_loops(&post_order);

        self.loops
    }

    fn discover_and_map_sub_loop(
        &mut self,
        loop_id: Id<Loop<BBS::BB>>,
        back_edges: &[Id<BBS::BB>],
    ) {
        let mut worklist: VecDeque<Id<BBS::BB>> = VecDeque::new();
        worklist.extend(back_edges.iter());

        while let Some(pred) = worklist.pop_front() {
            let sub_loop = self.loops.get_loop_for(pred);

            if let Some(sub_loop) = sub_loop {
                let mut sub_loop = sub_loop;
                while let Some(parent) = self.loops.arena[sub_loop].parent {
                    sub_loop = parent;
                }

                if sub_loop == loop_id {
                    continue;
                }

                self.loops.arena[sub_loop].parent = Some(loop_id);
                let pred = self.loops.arena[sub_loop].header;
                for pred in self.basic_blocks.get_arena()[pred].get_preds() {
                    if self.loops.get_loop_for(*pred) != Some(sub_loop) {
                        worklist.push_back(*pred)
                    }
                }
            } else {
                self.loops.set_loop_for(pred, loop_id);
                if pred == self.loops.arena[loop_id].header {
                    continue;
                }
                worklist.extend(self.basic_blocks.get_arena()[pred].get_preds().iter());
            }
        }
    }

    fn insert_blocks_into_loops(&mut self, post_order: &Vec<Id<BBS::BB>>) {
        for node in post_order {
            self.insert_block_into_loop(*node)
        }
    }

    fn insert_block_into_loop(&mut self, bb: Id<BBS::BB>) {
        let sub_loop = match self.loops.get_loop_for(bb) {
            Some(sub_loop) => sub_loop,
            None => return,
        };

        let sub_loop_ = &self.loops.arena[sub_loop];
        if bb == sub_loop_.header {
            if let Some(parent) = sub_loop_.parent {
                self.loops.arena[parent].sub_loops.push(sub_loop);
            } else {
                self.loops.top_level_loops.push(sub_loop);
                return;
            }
        }

        let mut sub_loop = sub_loop;
        while let Some(parent) = self.loops.arena[sub_loop].parent {
            self.loops.arena[parent].set.insert(bb);
            sub_loop = parent;
        }
    }

    fn get_post_ordered_blocks(&self, root: Id<BBS::BB>) -> Vec<Id<BBS::BB>> {
        let mut blocks = vec![];
        for &child in self
            .dom_tree
            .tree
            .get(&root)
            .unwrap_or(&FxHashSet::default())
        {
            blocks.append(&mut self.get_post_ordered_blocks(child));
        }
        blocks.push(root);
        blocks
    }
}

impl<BB: BasicBlockTrait> Loops<BB> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            bb_to_loop: FxHashMap::default(),
            top_level_loops: Vec::new(),
        }
    }

    pub fn new_loop(&mut self, header: Id<BB>) -> Id<Loop<BB>> {
        self.arena.alloc(Loop {
            parent: None,
            header,
            sub_loops: vec![],
            set: FxHashSet::default(),
        })
    }

    pub fn get_loop_for(&self, bb: Id<BB>) -> Option<Id<Loop<BB>>> {
        self.bb_to_loop.get(&bb).map(|x| *x)
    }

    pub fn set_loop_for(&mut self, bb: Id<BB>, loop_id: Id<Loop<BB>>) {
        self.bb_to_loop.insert(bb, loop_id);
    }
}
