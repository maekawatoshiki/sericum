use super::{function::*, module::*, node::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct Combine {}

impl Combine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn combine_module(&mut self, module: &mut DAGModule) {
        for (_, func) in &mut module.functions {
            self.combine_function(func)
        }
    }

    fn combine_function(&mut self, func: &mut DAGFunction) {
        for bb_id in &func.dag_basic_blocks {
            let bb = &func.dag_basic_block_arena[*bb_id];
            some_then!(entry, bb.entry, {
                self.combine_node(&mut FxHashMap::default(), &mut func.dag_arena, entry);
            })
        }
    }

    fn combine_node(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        node_id: DAGNodeId,
    ) -> DAGNodeId {
        if !arena[node_id].is_operation() {
            return node_id;
        }

        if let Some(replaced) = replace.get(&node_id) {
            return *replaced;
        }

        let new_operands = self.combine_operands(replace, arena, arena[node_id].operand.clone());
        arena[node_id].operand = new_operands;

        // TODO: Macro for pattern matching?
        let replaced_id = match &arena[node_id].kind {
            DAGNodeKind::Load => self.combine_node_load(arena, node_id),
            DAGNodeKind::Store => self.combine_node_store(arena, node_id),
            DAGNodeKind::Add => self.combine_node_add(replace, arena, node_id),
            DAGNodeKind::BrCond => self.combine_node_brcond(arena, node_id),
            _ => node_id,
        };
        replace.insert(node_id, replaced_id);

        arena[replaced_id].next = match arena[node_id].next {
            Some(next) => Some(self.combine_node(replace, arena, next)),
            None => return replaced_id,
        };

        replaced_id
    }

    fn combine_node_add(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        node_id: DAGNodeId,
    ) -> DAGNodeId {
        #[rustfmt::skip]
        macro_rules! node {
            () => { arena[node_id] };
            ($id:expr) => { arena[$id] };
        }
        #[rustfmt::skip]
        macro_rules! node_op {
            ($i:expr) => { node!(node!().operand[$i])};
            ($id:expr, $i:expr) => { node!(node!($id).operand[$i])};
        }

        if node!().next.is_some() {
            return node_id;
        };

        // (C + any) -> (any + C)
        if node_op!(0).is_constant() && !node_op!(1).is_constant() {
            node!().operand.swap(0, 1);
        }

        // (~fi + fi) -> (fi + ~fi)
        if !node_op!(0).is_frame_index() && node_op!(1).is_frame_index() {
            node!().operand.swap(0, 1);
        }

        // println!(">>>> {:?}", node_op!(1));
        if node_op!(1).is_constant() && node_op!(1).as_constant().is_null() {
            return node!().operand[0];
        }

        // ((node + C1) + C2) -> (node + (C1 + C2))
        if node_op!(0).is_operation()
            && node!(node!().operand[0]).kind == DAGNodeKind::Add
            && !node_op!(node!().operand[0], 0).is_constant()
            && node_op!(node!().operand[0], 1).is_constant()
            && node_op!(1).is_constant()
        {
            let op0 = self.combine_node(replace, arena, node!(node!().operand[0]).operand[0]);
            let const_folded = node_op!(node!().operand[0], 1)
                .as_constant()
                .add(node_op!(1).as_constant());
            let c = arena.alloc(DAGNode::new(
                DAGNodeKind::Constant(const_folded),
                vec![],
                Some(const_folded.get_type()),
            ));
            return arena.alloc(DAGNode::new(
                DAGNodeKind::Add,
                vec![op0, c],
                node!().ty.clone(),
            ));
        }

        node_id
    }

    fn combine_node_load(&mut self, arena: &mut Arena<DAGNode>, node_id: DAGNodeId) -> DAGNodeId {
        #[rustfmt::skip]
        macro_rules! node {
            () => { arena[node_id] };
            ($id:expr) => { arena[$id] };
        }
        #[rustfmt::skip]
        macro_rules! node_op {
            ($i:expr) => { node!(node!().operand[$i])};
            ($id:expr, $i:expr) => { node!(node!($id).operand[$i])};
        }

        if node_op!(0).is_operation() && node_op!(0).kind == DAGNodeKind::Add {
            let add = node!(node!().operand[0]).clone();
            let op0 = add.operand[0];
            let op1 = add.operand[1];

            if node!(op0).is_frame_index() && node!(op1).is_constant() {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::LoadFiConstOff,
                    vec![op0, op1],
                    node!().ty.clone(),
                ));
            }

            if node!(op0).is_frame_index()
                && node!(op1).kind == DAGNodeKind::Mul
                && node_op!(op1, 1).is_constant()
            {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::LoadFiOff,
                    vec![op0, node!(op1).operand[0], node!(op1).operand[1]],
                    node!().ty.clone(),
                ));
            }

            if node!(op0).is_operation()
                && node!(op1).kind == DAGNodeKind::Mul
                && node_op!(op1, 1).is_constant()
            {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::LoadRegOff,
                    vec![op0, node!(op1).operand[0], node!(op1).operand[1]],
                    node!().ty.clone(),
                ));
            }
        }

        node_id
    }

    fn combine_node_store(&mut self, arena: &mut Arena<DAGNode>, node_id: DAGNodeId) -> DAGNodeId {
        #[rustfmt::skip]
        macro_rules! node {
            () => { arena[node_id] };
            ($id:expr) => { arena[$id] };
        }
        #[rustfmt::skip]
        macro_rules! node_op {
            ($i:expr) => { node!(node!().operand[$i])};
            ($id:expr, $i:expr) => { node!(node!($id).operand[$i])};
        }

        if node_op!(0).kind == DAGNodeKind::Add {
            let add = arena[node!().operand[0]].clone();
            let op0 = add.operand[0];
            let op1 = add.operand[1];
            let new_src = node!().operand[1];

            if node!(op0).is_frame_index() && node!(op1).is_constant() {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::StoreFiConstOff,
                    vec![op0, op1, new_src],
                    node!().ty.clone(),
                ));
            }

            if node!(op0).is_frame_index()
                && node!(op1).kind == DAGNodeKind::Mul
                && node_op!(op1, 1).is_constant()
            {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::StoreFiOff,
                    vec![op0, node!(op1).operand[0], node!(op1).operand[1], new_src],
                    node!().ty.clone(),
                ));
            }

            if node!(op0).is_operation()
                && node!(op1).kind == DAGNodeKind::Mul
                && node_op!(op1, 1).is_constant()
            {
                return arena.alloc(DAGNode::new(
                    DAGNodeKind::StoreRegOff,
                    vec![op0, node!(op1).operand[0], node!(op1).operand[1], new_src],
                    node!().ty.clone(),
                ));
            }
        }

        node_id
    }

    fn combine_node_brcond(&mut self, arena: &mut Arena<DAGNode>, node_id: DAGNodeId) -> DAGNodeId {
        let node = &arena[node_id];
        let cond_id = node.operand[0];
        let br = node.operand[1];
        match arena[cond_id].kind {
            DAGNodeKind::Setcc => arena.alloc(DAGNode::new(
                DAGNodeKind::Brcc,
                vec![
                    arena[cond_id].operand[0],
                    arena[cond_id].operand[1],
                    arena[cond_id].operand[2],
                    br,
                ],
                None,
            )),
            _ => node_id,
        }
    }

    fn combine_operands(
        &mut self,
        replace: &mut FxHashMap<DAGNodeId, DAGNodeId>,
        arena: &mut Arena<DAGNode>,
        operands: Vec<DAGNodeId>,
    ) -> Vec<DAGNodeId> {
        operands
            .into_iter()
            .map(|op| self.combine_node(replace, arena, op))
            .collect()
    }
}
