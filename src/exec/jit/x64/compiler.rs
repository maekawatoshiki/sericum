use crate::ir::{function::*, module::*, opcode::*, types::*, value::*};
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

    pub fn compile(&mut self, id: FunctionId) {
        let f = self.module.function_ref(id);

        let params_len = f.ty.get_function_ty().unwrap().params_ty.len();
        let f_entry = self.asm.new_dynamic_label();

        dynasm!(self.asm
            ; => f_entry
            ; push rbp
            ; mov rbp, rsp
        );

        fn get_value(ops: &mut x64::Assembler, val: &Value) {
            match val {
                Value::Argument(ArgumentValue { index, .. }) => {
                    dynasm!(ops
                        ; mov eax, [rbp+8*(2+*index as i32)]
                        ; push rax
                    );
                }
                // Value::Instruction(InstructionValue { id, .. }) => mem.get(&id).unwrap().clone(),
                Value::Immediate(im) => match im {
                    ImmediateValue::Int32(i) => {
                        dynasm!(ops ; push *i);
                    }
                },
                // Value::Function(_id) => unimplemented!(),
                _ => unimplemented!(),
            }
        }

        let (mut _cur_bb_id, mut bb) = f.basic_blocks.iter().next().unwrap();
        let mut _last_bb_id = _cur_bb_id;

        for val in &bb.iseq {
            let instr_id = val.get_instr_id().unwrap();
            let instr = &f.instr_table[instr_id];
            match &instr.opcode {
                Opcode::Ret(v) => {
                    get_value(&mut self.asm, &v);
                    dynasm!(self.asm
                            ; pop rax
                            ; pop rbp
                            ; ret);
                }
                _ => {}
            }
        }

        self.function_map.insert(id, f_entry);
    }
}
