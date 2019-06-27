use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*};
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};

pub struct RegisterAllocator<'a> {
    pub module: &'a mut Module,
}

impl<'a> RegisterAllocator<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        let fs = self.module.functions.clone();
        for (_, f) in &fs {
            let (a, b) = self.collect_regs(f);
            println!("reg: \n\tregs:{:?}\n\tlast use:{:?}", a, b);
            let r = self.scan(a, b);
            println!("alloced: {:?}", r);
        }
    }

    pub fn scan(
        &mut self,
        regs: FxHashSet<InstructionId>,
        last_use: FxHashMap<InstructionId, InstructionId>,
    ) -> FxHashMap<InstructionId, (usize, bool)> {
        let mut alloc = FxHashMap::default();
        let mut used = FxHashMap::default();
        let num_reg = 2;

        for reg in &regs {
            let mut found = false;
            for i in 0..num_reg {
                if used.contains_key(&i)
                    && reg.index() < last_use.get(used.get(&i).unwrap()).unwrap().index()
                {
                    continue;
                }
                alloc.insert(*reg, (i, false));
                used.insert(i, *reg);
                found = true;
                break;
            }
            if (found) {
                continue;
            }

            used.insert(num_reg - 1, *reg);
            let mut k = 0;
            for i in 1..num_reg {
                if last_use.get(used.get(&k).unwrap()).unwrap().index()
                    < last_use.get(used.get(&i).unwrap()).unwrap().index()
                {
                    k = i;
                }
            }
            alloc.insert(*reg, (k, false));
            *alloc.get_mut(used.get(&k).unwrap()).unwrap() = (num_reg - 1, true);
            *used.get_mut(&k).unwrap() = *reg;
        }

        alloc
    }

    pub fn collect_regs(
        &mut self,
        f: &Function,
    ) -> (
        FxHashSet<InstructionId>,
        FxHashMap<InstructionId, InstructionId>,
    ) {
        let mut regs = FxHashSet::default();
        let mut last_use = FxHashMap::default(); // (reg, instr that used reg the last)

        for (_, bb) in &f.basic_blocks {
            for instr_val in bb.iseq.clone() {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                match instr.opcode {
                    Opcode::Call(ref func, ref args) => {
                        if let Some(id) = func.get_instr_id() {
                            last_use.insert(id, instr_id);
                        }
                        for arg in args {
                            if let Some(id) = arg.get_instr_id() {
                                last_use.insert(id, instr_id);
                            }
                        }
                    }
                    Opcode::CondBr(ref v, _, _) | Opcode::Ret(ref v) | Opcode::Load(ref v) => {
                        if let Some(id) = v.get_instr_id() {
                            last_use.insert(id, instr_id);
                        }
                    }
                    Opcode::Phi(ref vals) => {
                        for (val, _) in vals {
                            if let Some(id) = val.get_instr_id() {
                                last_use.insert(id, instr_id);
                            }
                        }
                    }
                    Opcode::ICmp(_, v1, v2) | Opcode::Add(v1, v2) | Opcode::Sub(v1, v2) => {
                        if let Some(id) = v1.get_instr_id() {
                            last_use.insert(id, instr_id);
                        }
                        if let Some(id) = v2.get_instr_id() {
                            last_use.insert(id, instr_id);
                        }
                    }
                    _ => {}
                }

                match instr.opcode {
                    Opcode::Add(_, _)
                    | Opcode::Alloca(_)
                    | Opcode::ICmp(_, _, _)
                    | Opcode::Load(_)
                    | Opcode::Phi(_)
                    | Opcode::Sub(_, _) => {
                        regs.insert(instr_id);
                    }
                    Opcode::Call(f, _)
                        if f.get_type(&self.module).get_function_ty().unwrap().ret_ty
                            != Type::Void =>
                    {
                        regs.insert(instr_id);
                    }
                    Opcode::Call(_, _)
                    | Opcode::Br(_)
                    | Opcode::CondBr(_, _, _)
                    | Opcode::Ret(_) => {}
                }
            }
        }

        (regs, last_use)
    }
}
