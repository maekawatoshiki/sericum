// TODO: refactoring!!!

use super::{basic_block::BasicBlockId, function::Function};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct DominatorTree {
    pub tree: FxHashMap<BasicBlockId, FxHashSet<BasicBlockId>>,
}

pub struct DominatorTreeConstructor<'a> {
    func: &'a Function,
    tree: DominatorTree,
    dom: FxHashMap<BasicBlockId, FxHashSet<BasicBlockId>>, // <a, bs>, b dominates a
}

impl DominatorTree {
    pub fn new() -> Self {
        Self {
            tree: FxHashMap::default(),
        }
    }

    pub fn dominate_bb(&self, bb0: BasicBlockId, bb1: BasicBlockId) -> bool {
        if let Some(set) = self.tree.get(&bb0) {
            if set.contains(&bb1) {
                return true;
            }
            for &s in set {
                if self.dominate_bb(s, bb1) {
                    return true;
                }
            }
        }
        false
    }
}

impl<'a> DominatorTreeConstructor<'a> {
    pub fn new(func: &'a Function) -> Self {
        Self {
            func,
            tree: DominatorTree::new(),
            dom: FxHashMap::default(),
        }
    }

    pub fn construct(mut self) -> DominatorTree {
        self.inspect_dominators();

        let mut strictly_dominated = FxHashMap::default();
        let mut idom = FxHashMap::default();

        for (a, bs) in &self.dom {
            for b in bs {
                if a != b {
                    strictly_dominated
                        .entry(*a)
                        .or_insert(FxHashSet::default())
                        .insert(*b);
                }
            }
        }

        for (a, bs) in &strictly_dominated {
            for b in bs {
                if bs.iter().any(|bb| {
                    if bb == b {
                        return false;
                    }
                    strictly_dominated.get(bb).map_or(false, |s| s.contains(b))
                }) {
                    idom.insert(*a, *b);
                }
            }
        }

        for (b, a) in idom {
            self.tree
                .tree
                .entry(a)
                .or_insert(FxHashSet::default())
                .insert(b);
        }

        self.tree
    }

    fn inspect_dominators(&mut self) {
        let all = self
            .func
            .basic_blocks
            .iter()
            .map(|&x| x)
            .collect::<FxHashSet<_>>();
        let entry = self.func.basic_blocks[0];
        self.dom
            .entry(entry)
            .or_insert(FxHashSet::default())
            .insert(entry);
        for cur in &self.func.basic_blocks[1..] {
            self.dom.insert(*cur, all.clone());
        }
        loop {
            let mut changed = false;
            for cur in &self.func.basic_blocks[1..] {
                changed |= self.inspect_dominators_of(*cur);
            }
            if !changed {
                break;
            }
        }
    }

    fn inspect_dominators_of(&mut self, cur_bb: BasicBlockId) -> bool {
        let mut set = None;
        for &pred in &self.func.basic_block_arena[cur_bb].pred {
            let dominators_of_pred = self.dom.get(&pred).unwrap().clone();
            if let Some(ref mut set) = &mut set {
                *set = &*set & &dominators_of_pred;
            } else {
                set = Some(dominators_of_pred);
            }
        }
        let mut set = set.unwrap_or(FxHashSet::default());
        set.insert(cur_bb);
        let changed = self.dom.get(&cur_bb).unwrap() != &set;
        self.dom.insert(cur_bb, set);
        changed
    }
}
