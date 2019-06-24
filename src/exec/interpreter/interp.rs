use crate::ir::{function::*, module::*, opcode::*, value::*};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteValue {
    Int1(bool),
    Int32(i32),
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    module: &'a Module,
}

impl<'a> Interpreter<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self { module }
    }

    // TODO: Refactor
    pub fn run_function(&mut self, id: FunctionId, args: Vec<ConcreteValue>) -> ConcreteValue {
        let f = self.module.function_ref(id);
        let mut mem = FxHashMap::default();

        fn get_value(
            val: &Value,
            args: &[ConcreteValue],
            mem: &mut FxHashMap<InstructionId, ConcreteValue>,
        ) -> ConcreteValue {
            match val {
                Value::Argument(n) => args[*n].clone(),
                Value::Instruction(id) => mem.get(&id).unwrap().clone(),
                Value::Immediate(im) => match im {
                    ImmediateValue::Int32(i) => ConcreteValue::Int32(*i),
                },
                Value::None => unimplemented!(),
            }
        }

        let (_, mut bb) = f.basic_blocks.iter().next().unwrap();
        loop {
            for val in &bb.iseq {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                match &instr.opcode {
                    Opcode::Add(v1, v2) => {
                        let val =
                            get_value(&v1, &args, &mut mem).add(get_value(&v2, &args, &mut mem));
                        mem.insert(instr_id, val);
                    }
                    Opcode::Alloca(_ty) => {
                        mem.insert(instr_id, ConcreteValue::Int32(0));
                    }
                    Opcode::ICmp(kind, v1, v2) => {
                        let val = match kind {
                            ICmpKind::Eq => {
                                get_value(&v1, &args, &mut mem).eq(get_value(&v2, &args, &mut mem))
                            }
                        };
                        mem.insert(instr_id, val);
                    }
                    Opcode::Br(id) => {
                        bb = f.basic_block_ref(*id);
                    }
                    Opcode::Load(v) => {
                        let val = mem.get(&v.get_instr_id().unwrap()).unwrap().clone();
                        mem.insert(instr_id, val);
                    }
                    Opcode::Ret(v) => return get_value(&v, &args, &mut mem),
                }
            }
        }
    }
}

impl ConcreteValue {
    pub fn add(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 + i2),
            _ => unimplemented!(),
        }
    }

    pub fn eq(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int1(i1 == i2),
            (ConcreteValue::Int1(i1), ConcreteValue::Int1(i2)) => ConcreteValue::Int1(i1 == i2),
            _ => unimplemented!(),
        }
    }
}
