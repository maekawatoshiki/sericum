use crate::ir::{
    basic_block::BasicBlockId,
    dom_tree::{DominatorTree, DominatorTreeConstructor},
    function::Function,
    module::Module,
    opcode::{Instruction, InstructionId, Opcode, Operand},
    types::Types,
};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct Mem2Reg {}

struct Mem2RegOnFunction<'a> {
    cur_func: &'a mut Function,
    inst_indexes: InstructionIndexes,
    dom_tree: DominatorTree,
}

pub type InstructionIndex = usize;

struct InstructionIndexes {
    inst2index: FxHashMap<InstructionId, InstructionIndex>,
}

impl Mem2Reg {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            // ignore internal function TODO
            if func.basic_blocks.len() == 0 {
                continue;
            }

            Mem2RegOnFunction {
                dom_tree: DominatorTreeConstructor::new(func).construct(),
                cur_func: func,
                inst_indexes: InstructionIndexes::new(),
            }
            .run(&module.types);
        }
    }
}

impl<'a> Mem2RegOnFunction<'a> {
    fn run(&mut self, _tys: &Types) {
        let mut single_store_allocas = vec![];
        let mut single_block_allocas = vec![];
        let mut multi_block_allocas = vec![];

        for &id in &self.cur_func.basic_blocks {
            let bb = &self.cur_func.basic_block_arena[id];
            for val in &*bb.iseq.borrow() {
                let inst_id = val.get_inst_id().unwrap();
                let inst = &self.cur_func.inst_table[inst_id];
                if !matches!(inst.opcode, Opcode::Alloca) {
                    continue;
                }
                let alloca = inst;

                let is_promotable = self.is_alloca_promotable(alloca);
                debug!(println!("promotable? {:?}", is_promotable));

                let is_stored_only_once = self.is_alloca_stored_only_once(alloca);
                debug!(println!("single store? {:?}", is_stored_only_once));

                let is_only_used_in_single_block = self.is_alloca_only_used_in_single_block(alloca);
                debug!(println!("single block? {:?}", is_only_used_in_single_block));

                if is_promotable && is_stored_only_once {
                    single_store_allocas.push(inst_id);
                    continue;
                }

                if is_promotable && is_only_used_in_single_block {
                    single_block_allocas.push(inst_id);
                    continue;
                }

                // stores and loads in multiple basic blocks
                if is_promotable {
                    multi_block_allocas.push(inst_id);
                }

                // TODO: support other cases...
            }
        }

        for alloca in single_store_allocas {
            self.promote_single_store_alloca(alloca);
        }

        for alloca in single_block_allocas {
            self.promote_single_block_alloca(alloca);
        }

        for alloca in multi_block_allocas {
            self.promote_multi_block_alloca(alloca);
        }
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
        for (load, load_users) in loads_to_remove {
            self.cur_func.remove_inst(load);

            for u in load_users {
                Instruction::replace_operand(
                    &mut self.cur_func.inst_table,
                    u,
                    &Operand::new_inst(self.cur_func.id.unwrap(), load),
                    src,
                )
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

            self.cur_func.remove_inst(nearest_store_id);
            self.cur_func.remove_inst(load_id);

            // remove loads and replace them with src
            for u in load_users {
                // let inst = &self.cur_func.inst_table[u];
                Instruction::replace_operand(
                    &mut self.cur_func.inst_table,
                    u,
                    &Operand::new_inst(self.cur_func.id.unwrap(), load_id),
                    src,
                )
            }
        }

        if all_access_removable {
            self.cur_func.remove_inst(alloca_id);
        }
    }

    fn promote_multi_block_alloca(&mut self, alloca_id: InstructionId) {
        // println!("dom tree: {:?}", self.dom_tree.tree);

        let mut def_blocks = vec![];
        let mut using_blocks = vec![];
        let mut livein_blocks = FxHashSet::default();
        let mut bb_to_insts: FxHashMap<
            BasicBlockId,
            /*sorted by inst idx=*/ Vec<(InstructionIndex, InstructionId)>,
        > = FxHashMap::default();

        for &use_id in &*self.cur_func.inst_table[alloca_id].users.borrow() {
            let inst = &self.cur_func.inst_table[use_id];
            match inst.opcode {
                Opcode::Store => {
                    def_blocks.push(inst.parent);
                    bb_to_insts
                        .entry(inst.parent)
                        .or_insert(vec![])
                        .push((self.inst_indexes.get_index(&self.cur_func, use_id), use_id));
                }
                Opcode::Load => {
                    using_blocks.push(inst.parent);
                    bb_to_insts
                        .entry(inst.parent)
                        .or_insert(vec![])
                        .push((self.inst_indexes.get_index(&self.cur_func, use_id), use_id));
                }
                _ => unreachable!(),
            }
        }
        for (_, insts) in &mut bb_to_insts {
            insts.sort_by(|(idx1, _), (idx2, _)| idx1.cmp(idx2))
        }

        // compute livein blocks
        let mut worklist = using_blocks.clone();
        while let Some(bb) = worklist.pop() {
            if !livein_blocks.insert(bb) {
                continue;
            }
            for pred in &self.cur_func.basic_block_arena[bb].pred {
                if def_blocks.contains(pred) {
                    continue;
                }
                worklist.push(*pred);
            }
        }
        // println!("def: {:?}", def_blocks);
        // println!("livein: {:?}", livein_blocks);

        let mut phi = vec![];

        for d in &def_blocks {
            for livein in &livein_blocks {
                for pred in &self.cur_func.basic_block_arena[*livein].pred {
                    if self.dom_tree.dominate_bb(*d, *pred)
                        && !(self.dom_tree.dominate_bb(*d, *livein) && *d != *livein)
                    {
                        phi.push(*livein);
                    }
                }
            }
        }

        let mut phi_map = FxHashMap::default();
        for p in phi.clone() {
            let mut worklist = phi.clone();
            while let Some(bb) = worklist.pop() {
                if def_blocks.contains(&bb) {
                    phi_map.entry(p).or_insert(vec![]).push(bb);
                    continue;
                }
                for pred in &self.cur_func.basic_block_arena[bb].pred {
                    worklist.push(*pred);
                }
            }
        }

        println!("PHI PAIR: {:?}", phi_map);

        for (phi_bb, def_bbs) in &phi_map {
            let phi_insts = bb_to_insts.get(phi_bb).unwrap();
            let mut leading_loads = vec![];
            for (_, phi_inst) in phi_insts {
                let inst = &self.cur_func.inst_table[*phi_inst];
                if inst.opcode == Opcode::Store {
                    break;
                }
                leading_loads.push(*phi_inst);
                println!("to be phi: {:?}", inst);
            }

            let mut srcs = vec![];
            let mut stores = vec![];
            for def_bb in def_bbs {
                let def_insts = bb_to_insts.get(def_bb).unwrap();
                for (_, phi_inst) in def_insts.iter().rev() {
                    let inst = &self.cur_func.inst_table[*phi_inst];
                    if inst.opcode != Opcode::Store {
                        continue;
                    }
                    srcs.push((inst.operands[0], *def_bb));
                    stores.push(*phi_inst);
                }
            }

            for load in leading_loads {
                let ty = self.cur_func.inst_table[load].ty;
                let parent = self.cur_func.inst_table[load].parent;
                let mut operands = vec![];
                for (v, bb) in &srcs {
                    operands.push(*v);
                    operands.push(Operand::BasicBlock(*bb));
                }
                let inst = Instruction::new(Opcode::Phi, operands, ty, parent);
                self.cur_func.change_inst(load, inst);
            }

            for store_id in stores {
                let store = &self.cur_func.inst_table[store_id];
                if store.users.borrow().len() == 0 {
                    self.cur_func.remove_inst(store_id);
                }
            }

            if self.cur_func.inst_table[alloca_id].users.borrow().len() == 0 {
                self.cur_func.remove_inst(alloca_id);
            }
        }
    }

    fn is_alloca_promotable(&self, alloca: &Instruction) -> bool {
        let func = &self.cur_func;
        alloca.users.borrow().iter().all(|&use_id| {
            let should_be_load_or_store = &func.inst_table[use_id];
            use super::types::Type;
            (should_be_load_or_store.opcode == Opcode::Load
                && should_be_load_or_store.ty == Type::Int32)
                || matches!(should_be_load_or_store.opcode, Opcode::Load | Opcode::Store)
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
        let bb = &cur_func.basic_block_arena[inst.parent];
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
