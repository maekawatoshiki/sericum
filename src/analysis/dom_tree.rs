// TODO: refactoring!!!

use crate::traits::{
    basic_block::{BasicBlockTrait, BasicBlocksTrait},
    function::FunctionTrait,
};
use id_arena::Id;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub struct DominatorTree<T: BasicBlockTrait> {
    pub root: Option<Id<T>>,
    pub tree: FxHashMap<Id<T>, FxHashSet<Id<T>>>,
    pub level: FxHashMap<Id<T>, usize>,
}

type BB<T> = <<T as FunctionTrait>::BBS as BasicBlocksTrait>::BB;

pub struct DominatorTreeConstructor<'a, F: FunctionTrait> {
    func: &'a F,
    tree: DominatorTree<BB<F>>,
    dfnum: FxHashMap<Id<BB<F>>, usize>,
    semi: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
    ancestor: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
    idom: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
    samedom: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
    vertex: FxHashMap<usize, Id<BB<F>>>,
    parent: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
    best: FxHashMap<Id<BB<F>>, Id<BB<F>>>,
}

impl<T: BasicBlockTrait> DominatorTree<T> {
    pub fn new() -> Self {
        Self {
            root: None,
            tree: FxHashMap::default(),
            level: FxHashMap::default(),
        }
    }

    pub fn dominate_bb(&self, bb0: Id<T>, bb1: Id<T>) -> bool {
        bb0 == bb1
            || self.tree.get(&bb0).map_or(false, |children| {
                children.contains(&bb1)
                    || children.iter().any(|&child| self.dominate_bb(child, bb1))
            })
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
            dfnum: FxHashMap::default(),
            semi: FxHashMap::default(),
            ancestor: FxHashMap::default(),
            idom: FxHashMap::default(),
            samedom: FxHashMap::default(),
            vertex: FxHashMap::default(),
            parent: FxHashMap::default(),
            best: FxHashMap::default(),
        }
    }

    pub fn construct_dom(&mut self) {
        let mut num = 0;
        let entry = self.func.get_basic_blocks().get_order()[0];
        let mut bucket: FxHashMap<Id<BB<F>>, FxHashSet<Id<BB<F>>>> = FxHashMap::default();
        self.dfs(None, entry, &mut num);
        for i in (1..num).rev() {
            let node = *self.vertex.get(&i).unwrap();
            let pred = *self.parent.get(&node).unwrap();
            let mut s = pred;
            for v in self.func.get_basic_blocks().get_arena()[node].get_preds() {
                let s_ = if self.dfnum.get(v).unwrap() <= self.dfnum.get(&node).unwrap() {
                    *v
                } else {
                    let n = self.ancestor_with_lowest_semi(*v);
                    *self.semi.get(&n).unwrap()
                };
                if self.dfnum.get(&s_).unwrap() < self.dfnum.get(&s).unwrap() {
                    s = s_;
                }
            }
            self.semi.insert(node, s);
            bucket.entry(s).or_insert(FxHashSet::default()).insert(node);
            self.link(pred, node);
            if let Some(set) = bucket.get_mut(&pred) {
                for v in &*set {
                    let y = self.ancestor_with_lowest_semi(*v);
                    if self.semi.get(&y).unwrap() == self.semi.get(&v).unwrap() {
                        self.idom.insert(*v, pred);
                    } else {
                        self.samedom.insert(*v, y);
                    }
                }
                set.clear();
            }
        }
        for i in 1..num {
            let n = *self.vertex.get(&i).unwrap();
            if let Some(s) = self.samedom.get(&n) {
                self.idom.insert(n, *s);
            }
        }
    }

    fn dfs(&mut self, pred: Option<Id<BB<F>>>, node: Id<BB<F>>, num: &mut usize) {
        if !self.dfnum.contains_key(&node)
            || (self.dfnum.contains_key(&node) && *self.dfnum.get(&node).unwrap() == 0)
        {
            self.dfnum.insert(node, *num);
            self.vertex.insert(*num, node);
            if let Some(pred) = pred {
                self.parent.insert(node, pred);
            }
            *num += 1;
            for succ in self.func.get_basic_blocks().get_arena()[node].get_succs() {
                self.dfs(Some(node), *succ, num);
            }
        }
    }

    fn ancestor_with_lowest_semi(&mut self, node: Id<BB<F>>) -> Id<BB<F>> {
        let a = *self.ancestor.get(&node).unwrap();
        if self.ancestor.contains_key(&a) {
            let b = self.ancestor_with_lowest_semi(a);
            let aa = *self.ancestor.get(&a).unwrap();
            self.ancestor.insert(node, aa);
            if self.dfnum.get(self.semi.get(&b).unwrap()).unwrap()
                < self
                    .dfnum
                    .get(self.semi.get(&self.best.get(&node).unwrap()).unwrap())
                    .unwrap()
            {
                self.best.insert(node, b);
            }
        }
        *self.best.get(&node).unwrap()
    }

    fn link(&mut self, pred: Id<BB<F>>, node: Id<BB<F>>) {
        self.ancestor.insert(node, pred);
        self.best.insert(node, node);
    }

    pub fn construct(mut self) -> DominatorTree<BB<F>> {
        self.construct_dom();

        for (&b, &a) in &self.idom {
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

        self.tree.root = Some(entry);

        self.tree
    }
}
