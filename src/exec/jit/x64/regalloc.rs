use crate::ir::{function::*, module::*, opcode::*, types::*};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone)]
pub struct RegisterAllocInfo {
    pub regs: FxHashMap<InstructionId, (usize, bool)>,
    pub last_use: FxHashMap<InstructionId, InstructionId>,
}

pub struct RegisterAllocator<'a> {
    pub module: &'a mut Module,
}

impl<'a> RegisterAllocator<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) -> FxHashMap<FunctionId, RegisterAllocInfo> {
        let fs = self.module.functions.clone();
        let mut v = FxHashMap::default();
        for (id, f) in &fs {
            let (a, b) = self.collect_regs(f);
            println!("reg: \n\tregs:{:?}\n\tlast use:{:?}\n\t", a, b);
            let regs = self.scan(a, b.clone());
            println!("alloced: {:?}", regs);
            v.insert(id, RegisterAllocInfo { regs, last_use: b });
        }
        v
    }

    pub fn scan(
        &mut self,
        regs: FxHashSet<InstructionId>,
        last_use: FxHashMap<InstructionId, InstructionId>,
    ) -> FxHashMap<InstructionId, (usize, bool)> {
        let mut alloc = FxHashMap::default();
        let mut used = FxHashMap::default();
        let num_reg = 3;

        let mut regs: Vec<InstructionId> = regs.iter().map(|x| *x).collect();
        regs.sort();
        for reg in &regs {
            let mut found = false;
            for i in 0..num_reg - 1 {
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
            if found {
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
        let mut linstr = None;

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
                    Opcode::Store(v1, v2)
                    | Opcode::ICmp(_, v1, v2)
                    | Opcode::Add(v1, v2)
                    | Opcode::Sub(v1, v2) => {
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
                    Opcode::Add(_, _) | Opcode::Sub(_, _) => {
                        regs.insert(instr_id);
                        last_use.insert(instr_id, instr_id);
                    }
                    Opcode::Alloca(_)
                    | Opcode::ICmp(_, _, _)
                    | Opcode::Load(_)
                    | Opcode::Phi(_) => {
                        regs.insert(instr_id);
                        last_use.insert(instr_id, instr_id);
                    }
                    Opcode::Call(f, _)
                        if f.get_type(&self.module).get_function_ty().unwrap().ret_ty
                            != Type::Void =>
                    {
                        regs.insert(instr_id);
                        last_use.insert(instr_id, instr_id);
                    }
                    Opcode::Store(_, dst) => {
                        if let Some(id) = dst.get_instr_id() {
                            regs.insert(id);
                            last_use.insert(instr_id, instr_id);
                        }
                    }
                    Opcode::Call(_, _)
                    | Opcode::Br(_)
                    | Opcode::CondBr(_, _, _)
                    | Opcode::Ret(_) => {}
                }

                linstr = Some(instr_id);
            }

            for out in &bb.live_out {
                last_use.insert(*out, linstr.unwrap());
            }
        }

        (regs, last_use)
    }
}
