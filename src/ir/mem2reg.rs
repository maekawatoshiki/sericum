use crate::ir::{
    function::Function,
    module::Module,
    opcode::{Instruction, InstructionId, Opcode, Operand},
    types::Types,
    value::{InstructionValue, Value},
};

pub struct Mem2Reg {}

struct Mem2RegOnFunction<'a> {
    cur_func: &'a mut Function,
}

impl Mem2Reg {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            Mem2RegOnFunction { cur_func: func }.run_on_function(&module.types);
        }
    }
}

impl<'a> Mem2RegOnFunction<'a> {
    fn run_on_function(&mut self, _tys: &Types) {
        let mut single_store_allocas = vec![];

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
                debug!(println!("promotable? : {:?}", is_promotable));

                let is_stored_only_once = self.is_alloca_stored_only_once(alloca);
                debug!(println!("single store?: {:?}", is_stored_only_once));

                if is_promotable && is_stored_only_once {
                    single_store_allocas.push(inst_id);
                    continue;
                }

                // TODO: support other cases...
            }
        }

        for alloca in single_store_allocas {
            self.promote_single_store_alloca(alloca);
        }
    }

    fn is_alloca_promotable(&self, alloca: &Instruction) -> bool {
        // let mut last_parent: Option<BasicBlockId> = None;
        // alloca.uses.borrow().iter().all(|&use_id| {
        //     let should_be_load_or_store = &func.inst_table[use_id];
        //     let same_parent = if last_parent.is_some() {
        //         let eq = last_parent.unwrap() == should_be_load_or_store.parent;
        //         last_parent = Some(should_be_load_or_store.parent);
        //         eq
        //     } else {
        //         last_parent = Some(should_be_load_or_store.parent);
        //         true
        //     };
        //     matches!(should_be_load_or_store.opcode, Opcode::Load | Opcode::Store) && same_parent
        // })

        let func = &self.cur_func;
        alloca.uses.borrow().iter().all(|&use_id| {
            let should_be_load_or_store = &func.inst_table[use_id];
            matches!(should_be_load_or_store.opcode, Opcode::Load | Opcode::Store)
        })
    }

    fn is_alloca_stored_only_once(&self, alloca: &Instruction) -> bool {
        alloca.uses.borrow().iter().fold(0usize, |acc, &use_id| {
            matches!(self.cur_func.inst_table[use_id].opcode, Opcode::Store) as usize + acc
        }) == 1
    }

    fn promote_single_store_alloca(&mut self, alloca_id: InstructionId) {
        let alloca = &self.cur_func.inst_table[alloca_id];
        let mut src = None;
        let mut stores_to_remove = vec![];
        let mut loads_to_remove = vec![];
        let mut replaceable_to_src = vec![];
        for &use_id in &*alloca.uses.borrow() {
            let load_or_store = &self.cur_func.inst_table[use_id];
            if load_or_store.opcode == Opcode::Store {
                let store = load_or_store;
                let store_id = use_id;
                src = Some(store.operands[0]);
                stores_to_remove.push(store_id);
            }
            if load_or_store.opcode == Opcode::Load {
                let load = load_or_store;
                let load_id = use_id;
                replaceable_to_src.push((load_id, load.uses.borrow().clone()));
                loads_to_remove.push(load_id);
            }
        }

        for store in stores_to_remove {
            self.cur_func.remove_inst(store);
        }

        for load in loads_to_remove {
            self.cur_func.remove_inst(load);
        }

        self.cur_func.remove_inst(alloca_id);

        // replace loads with src
        for (load, uses_load) in replaceable_to_src {
            for u in uses_load {
                let inst = &mut self.cur_func.inst_table[u];
                inst.replace_operand(
                    &Operand::Value(Value::Instruction(InstructionValue {
                        func_id: self.cur_func.id.unwrap(),
                        id: load,
                    })),
                    src.unwrap(),
                )
            }
        }
    }
}
