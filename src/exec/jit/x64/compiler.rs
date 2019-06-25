use crate::ir::{function::*, module::*, opcode::*, types::*, value::*};
use dynasmrt::*;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct JITCompiler<'a> {
    module: &'a Module,
    asm: x64::Assembler,
}

impl<'a> JITCompiler<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut asm = x64::Assembler::new().unwrap();
        let offset = asm.offset();
        dynasm!(asm
        ; mov rax, 123
        ; ret
        );

        let buf = asm.finalize().unwrap();
        let f: extern "C" fn() -> i32 = unsafe { ::std::mem::transmute(buf.ptr(offset)) };
        println!("{}", f());

        Self {
            module,
            asm: x64::Assembler::new().unwrap(),
        }
    }

    pub fn compile(&mut self, id: FunctionId) {
        let f = self.module.function_ref(id);
        // let mut mem = FxHashMap::default();
        fn get_value(ops: &mut x64::Assembler, val: &Value) {
            match val {
                // Value::Argument(ArgumentValue { index, .. }) => args[*index].clone(),
                // Value::Instruction(InstructionValue { id, .. }) => mem.get(&id).unwrap().clone(),
                Value::Immediate(im) => match im {
                    ImmediateValue::Int32(i) => {
                        dynasm!(ops ; mov rax, *i);
                    }
                },
                // Value::Function(_id) => unimplemented!(),
                _ => unimplemented!(),
            }
        }

        let (mut cur_bb_id, mut bb) = f.basic_blocks.iter().next().unwrap();
        let mut last_bb_id = cur_bb_id;

        for val in &bb.iseq {
            let instr_id = val.get_instr_id().unwrap();
            let instr = &f.instr_table[instr_id];
            match &instr.opcode {
                Opcode::Ret(v) => {
                    get_value(&mut self.asm, &v);
                    dynasm!(self.asm ; ret);
                }
                _ => {}
            }
        }
    }
}
