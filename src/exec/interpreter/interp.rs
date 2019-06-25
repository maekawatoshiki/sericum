use crate::ir::{function::*, module::*, opcode::*, types::*, value::*};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteValue {
    Void,
    Int1(bool),
    Int32(i32),
    Mem(*mut u8),
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
                Value::Argument(ArgumentValue { index, .. }) => args[*index].clone(),
                Value::Instruction(InstructionValue { id, .. }) => mem.get(&id).unwrap().clone(),
                Value::Immediate(im) => match im {
                    ImmediateValue::Int32(i) => ConcreteValue::Int32(*i),
                },
                Value::Function(id) => unimplemented!(),
                Value::None => unimplemented!(),
            }
        }

        let (mut cur_bb_id, mut bb) = f.basic_blocks.iter().next().unwrap();
        let mut last_bb_id = cur_bb_id;

        let ret = 'main: loop {
            for val in &bb.iseq {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                match &instr.opcode {
                    Opcode::Add(v1, v2) => {
                        let val =
                            get_value(&v1, &args, &mut mem).add(get_value(&v2, &args, &mut mem));
                        mem.insert(instr_id, val);
                    }
                    Opcode::Alloca(ty) => {
                        mem.insert(
                            instr_id,
                            ConcreteValue::Mem(match ty {
                                Type::Int1 => Box::into_raw(Box::new(0u8)) as *mut u8,
                                Type::Int32 => Box::into_raw(Box::new(0u32)) as *mut u8,
                                Type::Pointer(_) => unimplemented!(),
                                Type::Void => unreachable!(),
                                Type::Function(_) => unimplemented!(),
                            }),
                        );
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
                        last_bb_id = cur_bb_id;
                        cur_bb_id = *id;
                        bb = f.basic_block_ref(*id);
                        break;
                    }
                    Opcode::CondBr(cond, bb1, bb2) => {
                        let cond = get_value(&cond, &args, &mut mem).i1_as_bool().unwrap();
                        bb = f.basic_block_ref({
                            last_bb_id = cur_bb_id;
                            cur_bb_id = if cond { *bb1 } else { *bb2 };
                            cur_bb_id
                        });
                        break;
                    }
                    Opcode::Phi(pairs) => {
                        let val = get_value(
                            &pairs.iter().find(|&(_, bb)| bb == &last_bb_id).unwrap().0,
                            &args,
                            &mut mem,
                        );
                        mem.insert(instr_id, val);
                    }
                    Opcode::Call(f, f_args) => match f {
                        Value::Function(id) => {
                            let val = self.run_function(
                                *id,
                                f_args
                                    .iter()
                                    .map(|arg| get_value(&arg, &args, &mut mem))
                                    .collect(),
                            );
                            if val != ConcreteValue::Void {
                                mem.insert(instr_id, val);
                            }
                        }
                        _ => unimplemented!(),
                    },
                    Opcode::Load(v) => {
                        let ptr = match get_value(&v, &args, &mut mem) {
                            ConcreteValue::Mem(ptr) => ptr,
                            _ => unreachable!(),
                        };
                        let val = match v.get_type(&self.module).get_element_ty().unwrap() {
                            Type::Int1 => {
                                ConcreteValue::Int1(if unsafe { *(ptr as *mut u8) } == 0 {
                                    false
                                } else {
                                    true
                                })
                            }
                            Type::Int32 => ConcreteValue::Int32(unsafe { *(ptr as *mut i32) }),
                            _ => unimplemented!(),
                        };
                        mem.insert(instr_id, val);
                    }
                    Opcode::Ret(v) => break 'main get_value(&v, &args, &mut mem),
                }
            }
        };

        for (_, cv) in mem {
            if let ConcreteValue::Mem(m) = cv {
                unsafe { Box::from_raw(m) };
            }
        }

        ret
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

    pub fn i1_as_bool(self) -> Option<bool> {
        match self {
            ConcreteValue::Int1(b) => Some(b),
            _ => None,
        }
    }
}
