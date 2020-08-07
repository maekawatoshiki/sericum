// TODO: refactoring!!!

use crate::traits::basic_block::{BasicBlockTrait, BasicBlocksTrait};
use id_arena::Id;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub struct DominatorTree<T: BasicBlockTrait> {
    pub root: Option<Id<T>>,
    pub tree: FxHashMap<Id<T>, FxHashSet<Id<T>>>,
    pub frontier: FxHashMap<Id<T>, FxHashSet<Id<T>>>,
    pub level: FxHashMap<Id<T>, usize>,
}

type Map<T> = FxHashMap<T, T>;

pub struct DominatorTreeConstructor<'a, BBS: BasicBlocksTrait> {
    basic_blocks: &'a BBS,
    tree: DominatorTree<BBS::BB>,
    dfnum: FxHashMap<Id<BBS::BB>, usize>,
    vertex: Vec<Id<BBS::BB>>,
    semi: Map<Id<BBS::BB>>,
    ancestor: Map<Id<BBS::BB>>,
    idom: Map<Id<BBS::BB>>,
    samedom: Map<Id<BBS::BB>>,
    parent: Map<Id<BBS::BB>>,
    best: Map<Id<BBS::BB>>,
}

impl<T: BasicBlockTrait> DominatorTree<T> {
    pub fn new() -> Self {
        Self {
            root: None,
            tree: FxHashMap::default(),
            frontier: FxHashMap::default(),
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

    pub fn children_of(&self, bb: Id<T>) -> Option<&FxHashSet<Id<T>>> {
        self.tree.get(&bb)
    }

    pub fn dominance_frontier_of(&self, bb: Id<T>) -> Option<&FxHashSet<Id<T>>> {
        self.frontier.get(&bb)
    }

    pub fn path_exists(&self, from: Id<T>, to: Id<T>) -> bool {
        self.dominate_bb(from, to)
            || self.dominance_frontier_of(from).map_or(false, |frontiers| {
                frontiers
                    .iter()
                    .any(|&frontier| self.path_exists(frontier, to))
            })
    }

    pub fn get_level_of(&self, bb: Id<T>) -> usize {
        *self.level.get(&bb).unwrap()
    }
}

macro_rules! cmp {
    ($base:expr; ($a:expr) $t:tt ($b:expr)) => {
        $base.get(&$a).unwrap() $t $base.get(&$b).unwrap()
    };
}

impl<'a, BBS: BasicBlocksTrait> DominatorTreeConstructor<'a, BBS> {
    pub fn new(basic_blocks: &'a BBS) -> Self {
        Self {
            basic_blocks,
            tree: DominatorTree::new(),
            dfnum: FxHashMap::default(),
            semi: FxHashMap::default(),
            ancestor: FxHashMap::default(),
            idom: FxHashMap::default(),
            samedom: FxHashMap::default(),
            vertex: Vec::new(),
            parent: FxHashMap::default(),
            best: FxHashMap::default(),
        }
    }

    pub fn construct_dom(&mut self) {
        let entry = self.basic_blocks.get_order()[0];
        let mut bucket = FxHashMap::default();
        let mut num = 0;

        self.number_by_dfs(None, entry, &mut num);

        for i in (1..num).rev() {
            let node = self.vertex[i];
            let pred = *self.parent.get(&node).unwrap();
            let mut s = pred;

            for v in self.basic_blocks.get_arena()[node].get_preds() {
                let s_ = if self.dfnum[v] <= self.dfnum[&node] {
                    *v
                } else {
                    let n = self.ancestor_with_lowest_semi(*v);
                    *self.semi.get(&n).unwrap()
                };
                if cmp!(self.dfnum; (s_) < (s)) {
                    s = s_;
                }
            }

            self.semi.insert(node, s);
            bucket.entry(s).or_insert(FxHashSet::default()).insert(node);
            self.link(pred, node);

            if let Some(set) = bucket.get_mut(&pred) {
                for v in &*set {
                    let y = self.ancestor_with_lowest_semi(*v);
                    if cmp!(self.semi; (y) == (v)) {
                        self.idom.insert(*v, pred);
                    } else {
                        self.samedom.insert(*v, y);
                    }
                }
                set.clear();
            }
        }

        for &n in &self.vertex[1..] {
            if let Some(s) = self.samedom.get(&n) {
                self.idom.insert(n, *s);
            }
        }
    }

    fn number_by_dfs(&mut self, pred: Option<Id<BBS::BB>>, node: Id<BBS::BB>, num: &mut usize) {
        if self.dfnum.contains_key(&node) {
            return;
        }

        self.dfnum.insert(node, *num);
        self.vertex.insert(*num, node);
        if let Some(pred) = pred {
            self.parent.insert(node, pred);
        }
        *num += 1;

        for succ in self.basic_blocks.get_arena()[node].get_succs() {
            self.number_by_dfs(Some(node), *succ, num);
        }
    }

    fn ancestor_with_lowest_semi(&mut self, node: Id<BBS::BB>) -> Id<BBS::BB> {
        let a = *self.ancestor.get(&node).unwrap();
        if self.ancestor.contains_key(&a) {
            let b = self.ancestor_with_lowest_semi(a);
            let aa = *self.ancestor.get(&a).unwrap();
            self.ancestor.insert(node, aa);
            if cmp!(self.dfnum; (self.semi.get(&b).unwrap()) <
                 (self.semi.get(&self.best.get(&node).unwrap()).unwrap()))
            {
                self.best.insert(node, b);
            }
        }
        *self.best.get(&node).unwrap()
    }

    fn link(&mut self, pred: Id<BBS::BB>, node: Id<BBS::BB>) {
        self.ancestor.insert(node, pred);
        self.best.insert(node, node);
    }

    fn calc_dom_frontier_of(
        &self,
        x: Id<BBS::BB>,
        frontier: &mut FxHashMap<Id<BBS::BB>, FxHashSet<Id<BBS::BB>>>,
    ) {
        if frontier.contains_key(&x) {
            // dominance frontier for x is already calcuated
            return;
        }

        frontier.insert(x, FxHashSet::default());

        for succ in self.basic_blocks.get_arena()[x].get_succs() {
            if self.idom.get(succ).map_or(true, |&x_| x != x_) {
                frontier.get_mut(&x).unwrap().insert(*succ);
            }
            for child in self.tree.children_of(x).unwrap_or(&FxHashSet::default()) {
                self.calc_dom_frontier_of(*child, frontier);
                for y in frontier.get(child).unwrap().clone() {
                    if self.idom.get(&y).map_or(true, |&x_| x_ != x) {
                        frontier.get_mut(&x).unwrap().insert(y);
                    }
                }
            }
        }
    }

    fn calc_dom_frontier(
        &self,
        start: Id<BBS::BB>,
    ) -> FxHashMap<Id<BBS::BB>, FxHashSet<Id<BBS::BB>>> {
        let mut frontier = FxHashMap::default();
        for &child in self.tree.tree.get(&start).unwrap_or(&FxHashSet::default()) {
            self.calc_dom_frontier_of(child, &mut frontier);
        }
        self.calc_dom_frontier_of(start, &mut frontier);
        frontier
    }

    pub fn construct(mut self) -> DominatorTree<BBS::BB> {
        self.construct_dom();

        // a dominates b
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

        let entry = self.basic_blocks.get_order()[0];
        self.tree.frontier = self.calc_dom_frontier(entry);
        leveling(&mut self.tree.level, &self.tree.tree, entry, 0);

        self.tree.root = Some(entry);

        self.tree
    }
}
