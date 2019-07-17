use crate::ir::{function::*, module::*, opcode::*};
use rustc_hash::FxHashMap;

pub struct RegisterAllocator<'a> {
    pub module: &'a Module,
}

impl<'a> RegisterAllocator<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    pub fn analyze(&mut self) {
        for (_, f) in &self.module.functions {
            f.renumber_vreg();
            self.collect_regs(f);
            self.scan(f);

            when_debug!(for (_, instr) in &f.instr_table {
                println!(
                    "vreg:{}\treg:{:?}\tspill:{:?}",
                    vreg!(instr),
                    instr.reg.borrow().reg,
                    instr.reg.borrow().spill
                );
            });
        }
    }

    pub fn scan(&mut self, f: &Function) {
        let mut used = FxHashMap::default();
        let num_reg = 5;

        for (_, bb) in &f.basic_blocks {
            for instr_val in &*bb.iseq_ref() {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];

                match &instr.opcode {
                    Opcode::Alloca(_) => continue,
                    _ => {}
                }

                if instr.reg.borrow().last_use.is_none() {
                    continue;
                }

                let mut found = false;
                for i in 0..num_reg - 1 {
                    if used.contains_key(&i) {
                        let target_last_use_id = f.instr_table[*used.get(&i).unwrap()]
                            .reg
                            .borrow()
                            .last_use
                            .unwrap();
                        let target_last_use = vreg!(f; target_last_use_id);
                        if vreg!(instr) < target_last_use {
                            continue;
                        }
                    }

                    instr.set_phy_reg(i, false);
                    used.insert(i, instr_id);
                    found = true;
                    break;
                }
                if found {
                    continue;
                }

                used.insert(num_reg - 1, instr_id);

                let mut k = 0;
                for i in 1..num_reg {
                    let l1 = f.instr_table[*used.get(&k).unwrap()]
                        .reg
                        .borrow()
                        .last_use
                        .unwrap();
                    let l2 = f.instr_table[*used.get(&i).unwrap()]
                        .reg
                        .borrow()
                        .last_use
                        .unwrap();
                    if l1 < l2 {
                        k = i;
                    }
                }

                instr.set_phy_reg(k, false);
                f.instr_table[*used.get(&k).unwrap()].set_phy_reg(num_reg - 1, true);

                *used.get_mut(&k).unwrap() = instr_id;
            }
        }
    }

    pub fn collect_regs(&mut self, f: &Function) {
        let mut last_instr = None;

        for (_, bb) in &f.basic_blocks {
            for instr_val in &*bb.iseq_ref() {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];

                match instr.opcode {
                    Opcode::Call(ref func, ref args) => {
                        some_then!(
                            id,
                            func.get_instr_id(),
                            f.instr_table[id].set_last_use(Some(instr_id))
                        );
                        for arg in args {
                            some_then!(
                                id,
                                arg.get_instr_id(),
                                f.instr_table[id].set_last_use(Some(instr_id))
                            );
                        }
                    }
                    Opcode::CondBr(ref v, _, _) | Opcode::Ret(ref v) | Opcode::Load(ref v) => {
                        some_then!(
                            id,
                            v.get_instr_id(),
                            f.instr_table[id].set_last_use(Some(instr_id))
                        );
                    }
                    Opcode::Phi(ref vals) => {
                        for (val, _) in vals {
                            some_then!(
                                id,
                                val.get_instr_id(),
                                f.instr_table[id].set_last_use(Some(instr_id))
                            );
                        }
                    }
                    Opcode::GetElementPtr(ptrval, ref indices) => {
                        some_then!(
                            id,
                            ptrval.get_instr_id(),
                            f.instr_table[id].set_last_use(Some(instr_id))
                        );
                        for idx in indices {
                            some_then!(
                                id,
                                idx.get_instr_id(),
                                f.instr_table[id].set_last_use(Some(instr_id))
                            );
                        }
                    }
                    Opcode::Store(v1, v2)
                    | Opcode::ICmp(_, v1, v2)
                    | Opcode::Add(v1, v2)
                    | Opcode::Sub(v1, v2)
                    | Opcode::Mul(v1, v2)
                    | Opcode::Rem(v1, v2) => {
                        some_then!(
                            id,
                            v1.get_instr_id(),
                            f.instr_table[id].set_last_use(Some(instr_id))
                        );
                        some_then!(
                            id,
                            v2.get_instr_id(),
                            f.instr_table[id].set_last_use(Some(instr_id))
                        );
                        // instr.set_last_use(Some(instr_id));
                    }
                    Opcode::Br(_) | Opcode::Alloca(_) => {
                        // instr.reg.borrow_mut().last_use = Some(cur_vreg);
                    }
                }

                last_instr = Some(instr_id);
            }

            for out in &bb.liveness.borrow().live_out {
                f.instr_table[*out].set_last_use(last_instr);
            }
        }
    }
}
