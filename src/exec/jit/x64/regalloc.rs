use crate::ir::{function::*, module::*, opcode::*, types::*};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone)]
pub struct RegisterAllocInfo {
    pub regs: FxHashMap<UniqueIndex, (usize, bool)>,
    pub last_use: FxHashMap<UniqueIndex, UniqueIndex>, // TODO: Instruction should have a use_list instead of last_use
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
        regs: FxHashSet<UniqueIndex>,
        last_use: FxHashMap<UniqueIndex, UniqueIndex>,
    ) -> FxHashMap<UniqueIndex, (usize, bool)> {
        let mut alloc = FxHashMap::default();
        let mut used = FxHashMap::default();
        let num_reg = 3;

        let mut regs: Vec<UniqueIndex> = regs.iter().map(|x| *x).collect();
        regs.sort();
        for reg in &regs {
            let mut found = false;
            for i in 0..num_reg - 1 {
                if used.contains_key(&i) && reg < last_use.get(used.get(&i).unwrap()).unwrap() {
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
                if last_use.get(used.get(&k).unwrap()).unwrap()
                    < last_use.get(used.get(&i).unwrap()).unwrap()
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
    ) -> (FxHashSet<UniqueIndex>, FxHashMap<UniqueIndex, UniqueIndex>) {
        let mut regs = FxHashSet::default();
        let mut last_use = FxHashMap::default(); // (reg, instr that used reg the last)
        let mut linstr = None;

        for (_, bb) in &f.basic_blocks {
            for instr_val in bb.iseq.clone() {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                let uniqidx = f.instr_id_to_unique_idx(instr_id);
                match instr.opcode {
                    Opcode::Call(ref func, ref args) => {
                        some_then!(id, func.get_instr_id(), {
                            last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                        });
                        for arg in args {
                            some_then!(id, arg.get_instr_id(), {
                                last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                            });
                        }
                        if func
                            .get_type(&self.module)
                            .get_function_ty()
                            .unwrap()
                            .ret_ty
                            != Type::Void
                        {
                            regs.insert(uniqidx);
                            last_use.insert(uniqidx, uniqidx);
                        }
                    }
                    Opcode::CondBr(ref v, _, _) | Opcode::Ret(ref v) | Opcode::Load(ref v) => {
                        some_then!(id, v.get_instr_id(), {
                            last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                        });
                    }
                    Opcode::Phi(ref vals) => {
                        for (val, _) in vals {
                            some_then!(id, val.get_instr_id(), {
                                last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                            });
                        }
                    }
                    Opcode::Store(v1, v2)
                    | Opcode::ICmp(_, v1, v2)
                    | Opcode::Add(v1, v2)
                    | Opcode::Sub(v1, v2) => {
                        some_then!(id, v1.get_instr_id(), {
                            last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                        });
                        some_then!(id, v2.get_instr_id(), {
                            last_use.insert(f.instr_id_to_unique_idx(id), uniqidx);
                        });
                        regs.insert(f.instr_id_to_unique_idx(instr_id));
                        last_use.insert(uniqidx, uniqidx);
                    }
                    Opcode::Alloca(_) => {
                        regs.insert(f.instr_id_to_unique_idx(instr_id));
                        last_use.insert(uniqidx, uniqidx);
                    }
                    _ => {}
                }

                linstr = Some(uniqidx);
            }

            for out in &bb.liveness.borrow().live_out {
                last_use.insert(*out, linstr.unwrap());
            }
        }

        (regs, last_use)
    }
}
