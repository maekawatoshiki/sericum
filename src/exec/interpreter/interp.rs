use crate::ir::{function::*, module::*, opcode::*, value::*};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub enum ConcreteValue {
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

        for (i, arg) in args.into_iter().enumerate() {
            mem.insert(i, arg);
        }

        fn get_value(val: &Value, mem: &mut FxHashMap<ValueId, ConcreteValue>) -> ConcreteValue {
            match val {
                Value::Id(id, _) => mem.get(&id).unwrap().clone(),
                Value::Immediate(im) => match im {
                    ImmediateValue::Int32(i) => ConcreteValue::Int32(*i),
                },
                Value::None => unimplemented!(),
            }
        }

        let (_, mut bb) = f.basic_blocks.iter().next().unwrap();
        loop {
            for instr in &bb.iseq {
                match &instr.opcode {
                    Opcode::Add(v1, v2) => {
                        let val = get_value(v1, &mut mem).add(get_value(v2, &mut mem));
                        mem.insert(instr.value.get_id().unwrap(), val);
                    }
                    Opcode::Alloca(_ty) => {
                        mem.insert(instr.value.get_id().unwrap(), ConcreteValue::Int32(0));
                    }
                    Opcode::Br(id) => {
                        bb = f.basic_block_ref(*id);
                    }
                    Opcode::Load(v) => {
                        let val = mem.get(&v.get_id().unwrap()).unwrap().clone();
                        mem.insert(instr.value.get_id().unwrap(), val);
                    }
                    Opcode::Ret(v) => return get_value(v, &mut mem),
                }
            }
        }
    }
}

impl ConcreteValue {
    pub fn add(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 + i2),
        }
    }
}
