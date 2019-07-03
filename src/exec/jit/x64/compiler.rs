// TODO: Better assembler than dynasm-rs?

use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use dynasmrt::*;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum GenericValue {
    Int32(i32),
}

#[derive(Debug)]
pub struct JITCompiler<'a> {
    module: &'a Module,
    asm: x64::Assembler,
    function_map: FxHashMap<FunctionId, DynamicLabel>,
}

impl<'a> JITCompiler<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            asm: x64::Assembler::new().unwrap(),
            function_map: FxHashMap::default(),
        }
    }

    pub fn run(&mut self, id: FunctionId, args: Vec<GenericValue>) -> i32 {
        let f_entry = *self.function_map.get(&id).unwrap();
        let entry = self.asm.offset();

        for arg in args.iter().rev() {
            match arg {
                GenericValue::Int32(i) => {
                    dynasm!(self.asm
                        ; mov r11, *i
                        ; push r11);
                }
            }
        }

        dynasm!(self.asm
                ; call =>f_entry
                ; add rsp, 8*(args.len() as i32)
                ; ret);

        self.asm.commit();
        let executor = self.asm.reader();
        let buf = executor.lock();
        let f: extern "C" fn() -> i32 = unsafe { ::std::mem::transmute(buf.ptr(entry)) };
        f()
    }

    pub fn compile(&mut self, id: FunctionId) {
        let f = self.module.function_ref(id);

        // let params_len = f.ty.get_function_ty().unwrap().params_ty.len();
        let f_entry = self.asm.new_dynamic_label();

        self.function_map.insert(id, f_entry);

        dynasm!(self.asm
            ; => f_entry
            ; push rbp
            ; mov rbp, rsp
        );

        let mut bbs: FxHashMap<BasicBlockId, DynamicLabel> = FxHashMap::default();

        for (bb_id, bb) in &f.basic_blocks {
            if bb_id.index() != 0 {
                let label = *bbs
                    .entry(bb_id)
                    .or_insert_with(|| self.asm.new_dynamic_label());
                dynasm!(self.asm; =>label);
            }

            for val in &bb.iseq {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                let uniqidx = instr.unique_idx;
                match &instr.opcode {
                    Opcode::Add(v1, v2) => {
                        let rn = instr.reg.borrow().reg.unwrap() as u8;
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                dynasm!(self.asm; mov Ra(rn), [rbp+8*(2+*index as i32)]; add Ra(rn), *i);
                            }
                            (
                                Value::Instruction(InstructionValue { id: id1, .. }),
                                Value::Argument(ArgumentValue { index, .. }),
                            ) => {
                                let reg1 = f.instr_table[*id1].reg.borrow().reg.unwrap() as u8;
                                dynasm!(self.asm; mov Ra(rn), Ra(reg1); add Ra(rn), [rbp+8*(2+*index as i32)])
                            }
                            (
                                Value::Instruction(InstructionValue { id: id1, .. }),
                                Value::Instruction(InstructionValue { id: id2, .. }),
                            ) => {
                                let reg1 = f.instr_table[*id1].reg.borrow().reg.unwrap() as u8;
                                let reg2 = f.instr_table[*id2].reg.borrow().reg.unwrap() as u8;
                                dynasm!(self.asm; mov Ra(rn), Ra(reg1); add Ra(rn), Ra(reg2))
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Sub(v1, v2) => {
                        let rn = instr.reg.borrow().reg.unwrap() as u8;
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                dynasm!(self.asm; mov Rd(rn), [rbp+8*(2+*index as i32)]; sub Rd(rn), *i)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Call(func, args) => {
                        let returns = func
                            .get_type(&self.module)
                            .get_function_ty()
                            .unwrap()
                            .ret_ty
                            != Type::Void;
                        match func {
                            Value::Function(f_id) => {
                                let mut save_regs = vec![];
                                for (_, instr) in &f.instr_table {
                                    let bgn = instr.unique_idx;
                                    let end = match instr.reg.borrow().last_use {
                                        Some(last_use) => last_use,
                                        None => continue,
                                    };
                                    if bgn < uniqidx && uniqidx < end {
                                        save_regs.push(instr.reg.borrow().reg.unwrap());
                                    }
                                }
                                println!("save: {:?}", save_regs);

                                let l = self.function_map.get(&f_id).unwrap(); // TODO

                                for save_reg in &save_regs {
                                    dynasm!(self.asm; push Ra(*save_reg as u8));
                                }

                                for arg in args {
                                    match arg {
                                        Value::Instruction(InstructionValue { id, .. }) => {
                                            let rn =
                                                f.instr_table[*id].reg.borrow().reg.unwrap() as u8;
                                            dynasm!(self.asm; push Ra(rn))
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                                dynasm!(self.asm; call =>*l);
                                if returns {
                                    let rn = instr.reg.borrow().reg.unwrap() as u8;
                                    dynasm!(self.asm; mov Ra(rn), rax);
                                }
                                dynasm!(self.asm; add rsp, 8*(args.len() as i32));

                                for save_reg in save_regs.iter().rev() {
                                    dynasm!(self.asm; pop Ra(*save_reg as u8));
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::CondBr(cond, b1, b2) => match cond {
                        Value::Instruction(iv) => {
                            let rn = f.instr_table[iv.id].reg.borrow_mut().reg.unwrap() as u8;
                            dynasm!(self.asm ; cmp Rb(rn), 1);
                            let l1 = *bbs
                                .entry(*b1)
                                .or_insert_with(|| self.asm.new_dynamic_label());
                            let l2 = *bbs
                                .entry(*b2)
                                .or_insert_with(|| self.asm.new_dynamic_label());
                            dynasm!(self.asm ; je =>l1 ; jmp =>l2);
                        }
                        _ => unimplemented!(),
                    },
                    Opcode::ICmp(kind, v1, v2) => {
                        let reg_num = instr.reg.borrow().reg.unwrap() as u8;
                        match kind {
                            ICmpKind::Le => match v1 {
                                Value::Argument(arg) => match v2 {
                                    Value::Immediate(n) => {
                                        dynasm!(self.asm
                                            ; cmp [rbp+8*(2+arg.index as i32)], n.as_int32() as i8);
                                        dynasm!(self.asm; setle Rb(reg_num as u8));
                                    }
                                    _ => unimplemented!(),
                                },
                                _ => unimplemented!(),
                            },
                            ICmpKind::Eq => match v1 {
                                Value::Argument(arg) => match v2 {
                                    Value::Immediate(n) => {
                                        dynasm!(self.asm
                                            ; cmp [rbp+8*(2+arg.index as i32)], n.as_int32() as i8);
                                        dynasm!(self.asm; sete Rb(reg_num as u8));
                                    }
                                    _ => unimplemented!(),
                                },
                                _ => unimplemented!(),
                            },
                        }
                    }
                    Opcode::Ret(v) => {
                        match v {
                            Value::Instruction(iv) => {
                                let rn = f.instr_table[iv.id].reg.borrow_mut().reg.unwrap() as u8;
                                dynasm!(self.asm; mov eax, Rd(rn));
                            }
                            Value::Immediate(ImmediateValue::Int32(x)) => {
                                dynasm!(self.asm ; mov eax, *x);
                            }
                            _ => unimplemented!(),
                        }
                        dynasm!(self.asm ; pop rbp; ret);
                    }
                    _ => {}
                }
            }
        }
    }
}
