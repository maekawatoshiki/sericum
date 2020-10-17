use crate::{
    analysis::dom_tree::{DominatorTree, DominatorTreeConstructor},
    ir::{
        basic_block::{BasicBlock, BasicBlockId},
        const_folding::ConstantFolding,
        function::Function,
        module::Module,
        opcode::{Instruction, InstructionId, Opcode, Operand},
        value::{InstructionValue, Value},
    },
    traits::function::FunctionTrait,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;
use std::collections::BinaryHeap;

pub struct Mem2Reg {}

struct Mem2RegOnFunction<'a> {
    cur_func: &'a mut Function,
    inst_indexes: InstructionIndexes,
    dom_tree: DominatorTree<BasicBlock>,
    phi_block_to_allocas: FxHashMap<BasicBlockId, Vec<InstructionId>>,
}

pub type InstructionIndex = usize;

struct InstructionIndexes {
    inst2index: FxHashMap<InstructionId, InstructionIndex>,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct BBWithLevel(usize, BasicBlockId);

impl Mem2Reg {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal || func.is_empty() {
                continue;
            }

            Mem2RegOnFunction {
                dom_tree: DominatorTreeConstructor::new(func.get_basic_blocks()).construct(),
                cur_func: func,
                inst_indexes: InstructionIndexes::new(),
                phi_block_to_allocas: FxHashMap::default(),
            }
            .run();
        }

        ConstantFolding::new().run_on_module(module)
    }
}

impl<'a> Mem2RegOnFunction<'a> {
    fn run(&mut self) {
        let mut single_store_allocas = vec![];
        let mut single_block_allocas = vec![];
        let mut multi_block_allocas = vec![];

        for &id in &self.cur_func.basic_blocks.order {
            let bb = &self.cur_func.basic_blocks.arena[id];
            for val in &*bb.iseq.borrow() {
                let inst_id = val.get_inst_id().unwrap();
                let inst = &self.cur_func.inst_table[inst_id];
                if !matches!(inst.opcode, Opcode::Alloca) {
                    continue;
                }
                let alloca = inst;

                let is_promotable = self.is_alloca_promotable(alloca);
                debug!(println!("promotable? {:?}", is_promotable));

                if !is_promotable {
                    continue;
                }

                let is_stored_only_once = self.is_alloca_stored_only_once(alloca);
                debug!(println!("single store? {:?}", is_stored_only_once));

                let is_only_used_in_single_block = self.is_alloca_only_used_in_single_block(alloca);
                debug!(println!("single block? {:?}", is_only_used_in_single_block));

                if is_stored_only_once {
                    single_store_allocas.push(inst_id);
                    continue;
                }

                if is_only_used_in_single_block {
                    single_block_allocas.push(inst_id);
                    continue;
                }

                // stores and loads in multiple basic blocks
                multi_block_allocas.push(inst_id);
            }
        }

        for alloca in single_store_allocas {
            self.promote_single_store_alloca(alloca);
        }

        for alloca in single_block_allocas {
            self.promote_single_block_alloca(alloca);
        }

        for &alloca in &multi_block_allocas {
            self.promote_multi_block_alloca(alloca);
        }

        self.rename(multi_block_allocas);
    }

    fn promote_single_store_alloca(&mut self, alloca_id: InstructionId) {
        let alloca = &self.cur_func.inst_table[alloca_id];
        let mut src = None;
        let mut store_to_remove = None;
        let mut loads_to_remove = vec![];

        for &use_id in &*alloca.users.borrow() {
            let inst = &self.cur_func.inst_table[use_id];
            match inst.opcode {
                Opcode::Store => {
                    src = Some(inst.operands[0]);
                    store_to_remove = Some(use_id);
                }
                Opcode::Load => loads_to_remove.push((use_id, inst.users.borrow().clone())),
                _ => unreachable!(),
            }
        }

        let src = src.unwrap();
        let store_to_remove = store_to_remove.unwrap();
        let store_idx = self.inst_indexes.get_index(&self.cur_func, store_to_remove);

        // can't handle loads before store so ignore them
        let mut all_loads_removable = true;
        loads_to_remove.retain(|&(id, _)| {
            let load_parent = self.cur_func.inst_table[id].parent;
            let store_parent = self.cur_func.inst_table[store_to_remove].parent;
            let valid = if load_parent == store_parent {
                let load_idx = self.inst_indexes.get_index(&self.cur_func, id);
                store_idx < load_idx
            } else {
                self.dom_tree.dominate_bb(store_parent, load_parent)
            };
            all_loads_removable &= valid;
            valid
        });

        if all_loads_removable {
            self.cur_func.remove_inst(store_to_remove);
            self.cur_func.remove_inst(alloca_id);
        }

        // remove loads and replace them with src
        for (load_id, load_users) in loads_to_remove {
            self.cur_func.remove_inst(load_id);
            for u in load_users {
                Instruction::replace_operand_inst(&mut self.cur_func.inst_table, u, load_id, src)
            }
        }
    }

    fn promote_single_block_alloca(&mut self, alloca_id: InstructionId) {
        let alloca = &self.cur_func.inst_table[alloca_id];
        let mut stores_and_indexes = vec![];
        let mut loads = vec![];

        fn find_nearest_store(
            stores_and_indexes: &Vec<(InstructionId, usize)>,
            load_idx: InstructionIndex,
        ) -> Option<InstructionId> {
            let i = stores_and_indexes
                .binary_search_by(|(_, store_idx)| store_idx.cmp(&load_idx))
                .unwrap_or_else(|x| x);
            if i == 0 {
                return None;
            }
            Some(stores_and_indexes[i - 1].0)
        }

        for &use_id in &*alloca.users.borrow() {
            let inst = &self.cur_func.inst_table[use_id];
            match inst.opcode {
                Opcode::Store => stores_and_indexes
                    .push((use_id, self.inst_indexes.get_index(&self.cur_func, use_id))),
                Opcode::Load => loads.push((use_id, inst.users.borrow().clone())),
                _ => unreachable!(),
            }
        }

        stores_and_indexes.sort_by(|(_, idx0), (_, idx1)| idx0.cmp(idx1)); // sort to make it more efficient to find

        let mut all_access_removable = true;
        let mut stores_to_remove = FxHashSet::default();
        for (load_id, load_users) in loads {
            let load_idx = self.inst_indexes.get_index(&self.cur_func, load_id);
            let nearest_store_id = match find_nearest_store(&stores_and_indexes, load_idx) {
                Some(nearest_store_id) => nearest_store_id,
                None => {
                    all_access_removable = false;
                    continue;
                }
            };

            let nearest_store = &self.cur_func.inst_table[nearest_store_id];
            let src = nearest_store.operands[0];

            stores_to_remove.insert(nearest_store_id);
            self.cur_func.remove_inst(load_id);

            // remove loads and replace them with src
            for u in load_users {
                Instruction::replace_operand_inst(&mut self.cur_func.inst_table, u, load_id, src)
            }
        }

        if all_access_removable {
            self.cur_func.remove_inst(alloca_id);
        }

        for s in stores_to_remove {
            self.cur_func.remove_inst(s)
        }
    }

    fn promote_multi_block_alloca(&mut self, alloca_id: InstructionId) {
        let mut def_blocks = vec![];
        let mut using_blocks = vec![];
        let mut livein_blocks = FxHashSet::default();

        for &use_id in &*self.cur_func.inst_table[alloca_id].users.borrow() {
            let inst = &self.cur_func.inst_table[use_id];
            match inst.opcode {
                Opcode::Store => def_blocks.push(inst.parent),
                Opcode::Load => using_blocks.push(inst.parent),
                _ => unreachable!(),
            }
        }

        // compute livein blocks
        let mut worklist = using_blocks.clone();
        while let Some(bb) = worklist.pop() {
            if !livein_blocks.insert(bb) {
                continue;
            }
            for pred in &self.cur_func.basic_blocks.arena[bb].pred {
                if def_blocks.contains(pred) {
                    continue;
                }
                worklist.push(*pred);
            }
        }
        // println!("def: {:?}", def_blocks);
        // println!("livein: {:?}", livein_blocks);
        // println!("dom: {:?}", self.dom_tree.tree);

        let mut queue = def_blocks
            .iter()
            .map(|&def| BBWithLevel(self.dom_tree.get_level_of(def), def))
            .collect::<BinaryHeap<_>>();
        let mut visited_worklist = FxHashSet::default();
        let mut visited_queue = FxHashSet::default();
        while let Some(root) = queue.pop() {
            let root_level = root.0;
            let root_id = root.1;
            let mut worklist = vec![];

            worklist.push(root_id);
            visited_worklist.insert(root_id);

            while let Some(bb_id) = worklist.pop() {
                let bb = &self.cur_func.basic_blocks.arena[bb_id];
                for &succ_id in &bb.succ {
                    let succ_level = self.dom_tree.get_level_of(succ_id);
                    if succ_level > root_level {
                        continue;
                    }
                    if !visited_queue.insert(succ_id) {
                        continue;
                    }
                    if !livein_blocks.contains(&succ_id) {
                        continue;
                    }

                    self.phi_block_to_allocas
                        .entry(succ_id)
                        .or_insert(vec![])
                        .push(alloca_id);

                    if !def_blocks.contains(&succ_id) {
                        queue.push(BBWithLevel(succ_level, succ_id));
                    }
                }

                if let Some(dom_children) = self.dom_tree.tree.get(&bb_id) {
                    for child in dom_children {
                        if visited_worklist.insert(*child) {
                            worklist.push(*child)
                        }
                    }
                }
            }
        }
    }

    fn rename(&mut self, allocas: Vec<InstructionId>) {
        let entry = self.cur_func.basic_blocks.order[0];
        let mut worklist: Vec<(
            BasicBlockId,
            Option<BasicBlockId>,
            FxHashMap<InstructionId, Operand>, // alloca id -> incoming
        )> = vec![(entry, None, FxHashMap::default())];
        let mut visited = FxHashSet::default();
        let mut added_phi: FxHashMap<(BasicBlockId, InstructionId), Operand> = FxHashMap::default(); // Alloca id -> phi

        // TODO: refactoring
        while let Some((mut cur, mut pred, mut incoming)) = worklist.pop() {
            loop {
                for alloca_id in self.phi_block_to_allocas.get(&cur).unwrap_or(&vec![]) {
                    if let Some(val) = added_phi.get(&(cur, *alloca_id)) {
                        let incoming_val = incoming.get_mut(alloca_id).unwrap();

                        // skip if incoming is not updated
                        if *val == *incoming_val {
                            continue;
                        }

                        // append new incoming to phi
                        let phi_id = val.as_value().as_instruction().id;
                        Instruction::add_operand(
                            &mut self.cur_func.inst_table,
                            phi_id,
                            *incoming_val,
                        );
                        Instruction::add_operand(
                            &mut self.cur_func.inst_table,
                            phi_id,
                            Operand::BasicBlock(pred.unwrap()),
                        );

                        let phi = &self.cur_func.inst_table[phi_id];
                        phi.set_users(&self.cur_func.inst_table); // add phi_id to operands' users

                        *incoming_val = *val;
                    } else {
                        if !incoming.contains_key(alloca_id) {
                            let ty = *self.cur_func.inst_table[*alloca_id].operands[0].as_type();
                            incoming.insert(*alloca_id, Operand::Value(Value::null(ty)));
                        }
                        let incoming_val = incoming.get_mut(alloca_id).unwrap();
                        let ty = self
                            .cur_func
                            .types
                            .get_element_ty(self.cur_func.inst_table[*alloca_id].ty, None)
                            .unwrap();
                        let inst = Instruction::new(
                            Opcode::Phi,
                            vec![*incoming_val, Operand::BasicBlock(pred.unwrap())],
                            ty,
                            cur,
                        );
                        let id = self.cur_func.alloc_inst(inst);
                        let val = Value::Instruction(InstructionValue {
                            func_id: self.cur_func.id.unwrap(),
                            id,
                            ty,
                        });
                        self.cur_func.basic_blocks.arena[cur]
                            .iseq_ref_mut()
                            .insert(0, val);
                        added_phi.insert((cur, *alloca_id), Operand::Value(val));
                        *incoming_val = Operand::Value(val);
                    }
                }

                let bb = &self.cur_func.basic_blocks.arena[cur];
                if !visited.insert(cur) {
                    break;
                }

                let mut removal_list = vec![];
                for inst_id in &*bb.iseq_ref() {
                    let inst_id = inst_id.as_instruction().id;
                    let (opcode, op0, alloca_id) = {
                        let inst = &self.cur_func.inst_table[inst_id];
                        let alloca_id = match inst.opcode {
                            Opcode::Store => inst.operands[1],
                            Opcode::Load => inst.operands[0],
                            _ => continue,
                        }
                        .as_value()
                        .as_instruction()
                        .id;
                        if !allocas.contains(&alloca_id) {
                            continue;
                        }
                        (inst.opcode, inst.operands[0], alloca_id)
                    };
                    match opcode {
                        Opcode::Store => {
                            *incoming
                                .entry(alloca_id)
                                .or_insert(Operand::Value(Value::None)) = op0;
                            removal_list.push(inst_id);
                        }
                        Opcode::Load => {
                            if let Some(val) = incoming.get(&alloca_id) {
                                Instruction::replace_all_uses(
                                    &mut self.cur_func.inst_table,
                                    inst_id,
                                    *val,
                                );
                            }
                            removal_list.push(inst_id);
                        }
                        _ => unreachable!(),
                    }
                }

                for remove in removal_list {
                    self.cur_func.remove_inst(remove);
                }

                if bb.succ.len() == 0 {
                    break;
                }

                pred = Some(cur);
                let mut succ_iter = bb.succ.iter();
                cur = *succ_iter.next().unwrap();
                for &succ in succ_iter {
                    worklist.push((succ, pred, incoming.clone()));
                }
            }
        }

        for alloca_id in allocas {
            self.cur_func.remove_inst(alloca_id);
        }
    }

    fn is_alloca_promotable(&self, alloca: &Instruction) -> bool {
        self.cur_func
            .types
            .get_element_ty(alloca.ty, None)
            .unwrap()
            .is_atomic()
            && alloca.users.borrow().iter().all(|&use_id| {
                let inst = &self.cur_func.inst_table[use_id];
                matches!(inst.opcode, Opcode::Load | Opcode::Store)
            })
    }

    fn is_alloca_stored_only_once(&self, alloca: &Instruction) -> bool {
        alloca.users.borrow().iter().fold(0usize, |acc, &use_id| {
            matches!(self.cur_func.inst_table[use_id].opcode, Opcode::Store) as usize + acc
        }) == 1
    }

    fn is_alloca_only_used_in_single_block(&self, alloca: &Instruction) -> bool {
        let mut last_parent: Option<BasicBlockId> = None;
        alloca.users.borrow().iter().all(|&user_id| {
            let user = &self.cur_func.inst_table[user_id];
            let same_parent = last_parent.get_or_insert(user.parent) == &user.parent;
            last_parent = Some(user.parent);
            matches!(user.opcode, Opcode::Load | Opcode::Store) && same_parent
        })
    }
}

impl InstructionIndexes {
    pub fn new() -> Self {
        Self {
            inst2index: FxHashMap::default(),
        }
    }

    pub fn get_index(&mut self, cur_func: &Function, inst_id: InstructionId) -> InstructionIndex {
        if let Some(idx) = self.inst2index.get(&inst_id) {
            return *idx;
        }

        let inst = &cur_func.inst_table[inst_id];
        let mut i = 0;
        let bb = &cur_func.basic_blocks.arena[inst.parent];
        for val in &*bb.iseq_ref() {
            let inst_id = val.as_instruction().id;
            let opcode = cur_func.inst_table[inst_id].opcode;
            if Self::is_interesting_opcode(opcode) {
                self.inst2index.insert(inst_id, i);
            }
            i += 1;
        }

        self.get_index(cur_func, inst_id)
    }

    pub fn is_interesting_opcode(opcode: Opcode) -> bool {
        matches!(opcode, Opcode::Store | Opcode::Load | Opcode::Alloca)
    }
}

impl Ord for BBWithLevel {
    fn cmp(&self, other: &BBWithLevel) -> Ordering {
        self.0.cmp(&other.0)
    }
}
impl PartialOrd for BBWithLevel {
    fn partial_cmp(&self, other: &BBWithLevel) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
