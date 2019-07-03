use crate::ir::{function::*, module::*, opcode::*, types::*};
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
            self.collect_regs(f);
            // for (_, instr) in &f.instr_table {
            //     println!(
            //         "  vreg:{} - last_use:{:?}",
            //         instr.unique_idx,
            //         instr.reg.borrow().last_use
            //     );
            // }
            self.scan(f);
            // for (_, instr) in &f.instr_table {
            //     println!(
            //         "  vreg:{} - reg:{:?}",
            //         instr.unique_idx,
            //         instr.reg.borrow().reg
            //     );
            // }
        }
    }

    pub fn scan(&mut self, f: &Function) {
        let mut used = FxHashMap::default();
        let num_reg = 3;

        for (_, bb) in &f.basic_blocks {
            for instr_val in &bb.iseq {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];

                if instr.reg.borrow().last_use.is_none() {
                    continue;
                }

                let mut found = false;
                for i in 0..num_reg - 1 {
                    if used.contains_key(&i) {
                        let target_last_use = f.instr_table[*used.get(&i).unwrap()]
                            .reg
                            .borrow()
                            .last_use
                            .unwrap();
                        if instr.unique_idx < target_last_use {
                            continue;
                        }
                    }

                    {
                        let mut reg = instr.reg.borrow_mut();
                        reg.reg = Some(i);
                        reg.spill = false
                    }

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

                {
                    let mut reg = instr.reg.borrow_mut();
                    reg.reg = Some(k);
                    reg.spill = false
                }

                {
                    let mut reg = f.instr_table[*used.get(&k).unwrap()].reg.borrow_mut();
                    reg.reg = Some(num_reg - 1);
                    reg.spill = true;
                }

                *used.get_mut(&k).unwrap() = instr_id;
            }
        }
    }

    pub fn collect_regs(&mut self, f: &Function) {
        let mut last_instr = None;

        for (_, bb) in &f.basic_blocks {
            for instr_val in &bb.iseq {
                let instr_id = instr_val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                let uniqidx = instr.unique_idx;
                match instr.opcode {
                    Opcode::Call(ref func, ref args) => {
                        some_then!(id, func.get_instr_id(), {
                            let mut reg_info = f.instr_table[id].reg.borrow_mut();
                            reg_info.last_use = Some(uniqidx);
                        });
                        for arg in args {
                            some_then!(id, arg.get_instr_id(), {
                                let mut reg_info = f.instr_table[id].reg.borrow_mut();
                                reg_info.last_use = Some(uniqidx);
                            });
                        }
                        if func
                            .get_type(&self.module)
                            .get_function_ty()
                            .unwrap()
                            .ret_ty
                            != Type::Void
                        {
                            instr.reg.borrow_mut().last_use = Some(uniqidx);
                        }
                    }
                    Opcode::CondBr(ref v, _, _) | Opcode::Ret(ref v) | Opcode::Load(ref v) => {
                        some_then!(id, v.get_instr_id(), {
                            let mut reg_info = f.instr_table[id].reg.borrow_mut();
                            reg_info.last_use = Some(uniqidx);
                        });
                    }
                    Opcode::Phi(ref vals) => {
                        for (val, _) in vals {
                            some_then!(id, val.get_instr_id(), {
                                let mut reg_info = f.instr_table[id].reg.borrow_mut();
                                reg_info.last_use = Some(uniqidx);
                            });
                        }
                    }
                    Opcode::Store(v1, v2)
                    | Opcode::ICmp(_, v1, v2)
                    | Opcode::Add(v1, v2)
                    | Opcode::Sub(v1, v2) => {
                        some_then!(id, v1.get_instr_id(), {
                            let mut reg_info = f.instr_table[id].reg.borrow_mut();
                            reg_info.last_use = Some(uniqidx);
                        });
                        some_then!(id, v2.get_instr_id(), {
                            let mut reg_info = f.instr_table[id].reg.borrow_mut();
                            reg_info.last_use = Some(uniqidx);
                        });
                        instr.reg.borrow_mut().last_use = Some(uniqidx);
                    }
                    Opcode::Alloca(_) => {
                        instr.reg.borrow_mut().last_use = Some(uniqidx);
                    }
                    _ => {}
                }

                last_instr = Some(uniqidx);
            }

            for out in &bb.liveness.borrow().live_out {
                f.find_instruction_by_unique_idx(*out)
                    .unwrap()
                    .reg
                    .borrow_mut()
                    .last_use = last_instr;
            }
        }
    }
}
