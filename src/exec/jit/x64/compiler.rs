use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use dynasmrt::*;
use rustc_hash::FxHashMap;

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

    pub fn run(mut self, id: FunctionId) -> i32 {
        let f_entry = *self.function_map.get(&id).unwrap();
        let args = vec![10];
        let aa = self.asm.offset();
        for arg in args.iter().rev() {
            dynasm!(self.asm
                    ; mov r11, *arg
                    ; push r11);
        }
        dynasm!(self.asm
                ; call =>f_entry
                ; add rsp, 8*(args.len() as i32)
                ; ret);
        let buf = self.asm.finalize().unwrap();
        let f: extern "C" fn() -> i32 = unsafe { ::std::mem::transmute(buf.ptr(aa)) };
        f()
    }

    pub fn compile(&mut self, id: FunctionId, regs: &FxHashMap<InstructionId, (usize, bool)>) {
        let f = self.module.function_ref(id);

        let params_len = f.ty.get_function_ty().unwrap().params_ty.len();
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
                dynasm!(self.asm ; =>label);
            }

            for val in &bb.iseq {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                match &instr.opcode {
                    Opcode::Add(v1, v2) => {
                        let reg_num = regs.get(&instr_id).unwrap().0;
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                dynasm!(self.asm; mov Ra(reg_num as u8), [rbp+8*(2+*index as i32)]; add eax, *i)
                            }
                            (
                                Value::Instruction(InstructionValue { id: id1, .. }),
                                Value::Instruction(InstructionValue { id: id2, .. }),
                            ) => {
                                let reg1 = regs.get(&id1).unwrap().0 as u8;
                                let reg2 = regs.get(&id2).unwrap().0 as u8;
                                dynasm!(self.asm; mov Ra(reg_num as u8), Ra(reg1); add Ra(reg_num as u8), Ra(reg2))
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Sub(v1, v2) => {
                        let reg_num = regs.get(&instr_id).unwrap().0;
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                dynasm!(self.asm; mov Rd(reg_num as u8), [rbp+8*(2+*index as i32)]; sub eax, *i)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Call(f, args) => {
                        let returns = f.get_type(&self.module).get_function_ty().unwrap().ret_ty
                            != Type::Void;
                        match f {
                            Value::Function(f_id) => {
                                let l = self.function_map.get(&f_id).unwrap(); // TODO
                                if returns {
                                    dynasm!(self.asm; push rax);
                                }
                                dynasm!(self.asm; call =>*l);
                                if returns {
                                    let reg_num = regs.get(&instr_id).unwrap().0;
                                    dynasm!(self.asm; mov Rd(reg_num as u8), eax; pop rax);
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::CondBr(cond, b1, b2) => match cond {
                        Value::Instruction(iv) => {
                            let reg_num = regs.get(&iv.id).unwrap().0;
                            match reg_num {
                                0 => dynasm!(self.asm ; cmp al, 1),
                                1 => dynasm!(self.asm ; cmp bl, 1),
                                2 => dynasm!(self.asm ; cmp cl, 1),
                                _ => unimplemented!(),
                            }
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
                        let reg_num = regs.get(&instr_id).unwrap().0;
                        match kind {
                            ICmpKind::Le => match v1 {
                                Value::Argument(arg) => match v2 {
                                    Value::Immediate(n) => {
                                        dynasm!(self.asm
                                            ; cmp [rbp+8*(2+arg.index as i32)], n.as_int32() as i8);
                                        match reg_num {
                                            0 => dynasm!(self.asm; setle al),
                                            1 => dynasm!(self.asm; setle bl),
                                            2 => dynasm!(self.asm; setle cl),
                                            _ => unimplemented!(),
                                        }
                                    }
                                    _ => unimplemented!(),
                                },
                                _ => unimplemented!(),
                            },
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Ret(v) => {
                        dynasm!(self.asm ; pop rbp);
                        match v {
                            Value::Instruction(iv) => {
                                let reg_num = regs.get(&iv.id).unwrap().0;
                                match reg_num {
                                    0 => dynasm!(self.asm ; ret),
                                    1 => dynasm!(self.asm ; mov eax, ebx ; ret),
                                    2 => dynasm!(self.asm ; mov eax, ecx ; ret),
                                    _ => unimplemented!(),
                                }
                            }
                            Value::Immediate(ImmediateValue::Int32(x)) => {
                                dynasm!(self.asm ; mov eax, *x ; ret);
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}
