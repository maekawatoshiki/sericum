// TODO: refactoring!!!

use super::{
    basic_block::{BasicBlockId, BasicBlockTrait, BasicBlocks, BasicBlocksTrait},
    function::FunctionTrait,
};
use id_arena::Id;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct DominatorTree<T: BasicBlockTrait> {
    pub tree: FxHashMap<Id<T>, FxHashSet<Id<T>>>,
    pub level: FxHashMap<Id<T>, usize>,
}

pub struct DominatorTreeConstructor<'a, F: FunctionTrait> {
    func: &'a F,
    tree: DominatorTree<<<F as FunctionTrait>::BasicBlocksTy as BasicBlocksTrait>::BasicBlockTy>,
    dom: FxHashMap<
        Id<<<F as FunctionTrait>::BasicBlocksTy as BasicBlocksTrait>::BasicBlockTy>,
        FxHashSet<Id<<<F as FunctionTrait>::BasicBlocksTy as BasicBlocksTrait>::BasicBlockTy>>,
    >, // <a, bs>, b dominates a
}

impl<T: BasicBlockTrait> DominatorTree<T> {
    pub fn new() -> Self {
        Self {
            tree: FxHashMap::default(),
            level: FxHashMap::default(),
        }
    }

    pub fn dominate_bb(&self, bb0: Id<T>, bb1: Id<T>) -> bool {
        if bb0 == bb1 {
            return true;
        }
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

    pub fn get_level_of(&self, bb: Id<T>) -> usize {
        *self.level.get(&bb).unwrap()
    }
}

impl<'a, F: FunctionTrait> DominatorTreeConstructor<'a, F> {
    pub fn new(func: &'a F) -> Self {
        Self {
            func,
            tree: DominatorTree::new(),
            dom: FxHashMap::default(),
        }
    }

    pub fn construct(
        mut self,
    ) -> DominatorTree<<<F as FunctionTrait>::BasicBlocksTy as BasicBlocksTrait>::BasicBlockTy>
    {
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
                if !bs.iter().any(|bb| {
                    if bb == b {
                        return false;
                    }
                    strictly_dominated.get(bb).map_or(false, |s| s.contains(b))
                }) {
                    idom.insert(*a, *b);
                }
            }
        }

        for (&b, &a) in &idom {
            self.tree
                .tree
                .entry(a)
                .or_insert(FxHashSet::default())
                .insert(b);
        }

        fn leveling<B: BasicBlockTrait>(
            level: &mut FxHashMap<Id<B>, usize>,
            dom: &FxHashMap<Id<B>, FxHashSet<Id<B>>>,
            cur: Id<B>,
            cur_level: usize,
        ) {
            level.insert(cur, cur_level);
            if dom.get(&cur).is_none() {
                return;
            }
            for &child in dom.get(&cur).unwrap() {
                leveling(level, dom, child, cur_level + 1);
            }
        }

        let entry = self.func.get_basic_blocks().get_order()[0];
        leveling(&mut self.tree.level, &self.tree.tree, entry, 0);

        self.tree
    }

    fn inspect_dominators(&mut self) {
        let all = self
            .func
            .get_basic_blocks()
            .get_order()
            .iter()
            .map(|&x| x)
            .collect::<FxHashSet<_>>();
        let entry = self.func.get_basic_blocks().get_order()[0];
        self.dom
            .entry(entry)
            .or_insert(FxHashSet::default())
            .insert(entry);
        for cur in &self.func.get_basic_blocks().get_order()[1..] {
            self.dom.insert(*cur, all.clone());
        }
        loop {
            let mut changed = false;
            for cur in &self.func.get_basic_blocks().get_order()[1..] {
                changed |= self.inspect_dominators_of(*cur);
            }
            if !changed {
                break;
            }
        }
    }

    fn inspect_dominators_of(
        &mut self,
        cur_bb: Id<<<F as FunctionTrait>::BasicBlocksTy as BasicBlocksTrait>::BasicBlockTy>,
    ) -> bool {
        let mut set = None;
        for &pred in self.func.get_basic_blocks().get_arena()[cur_bb].get_preds() {
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
