// TODO: CAUTION: this code is no longer usable

// use crate::ir::{function::*, module::*, opcode::*, types::*, value::*};
use crate::ir::{function::*, module::*, types::*};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteValue {
    Void,
    Int1(bool),
    Int32(i32),
    Mem(*mut u8, Type),
}

pub struct Interpreter<'a> {
    _module: &'a Module,
    _internal_func: FxHashMap<String, fn(&[ConcreteValue]) -> ConcreteValue>,
}

impl<'a> Interpreter<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            _module: module,
            _internal_func: {
                vec![("cilk.println.i32".to_string(), cilk_println_i32 as _)]
                    .into_iter()
                    .collect::<FxHashMap<_, _>>()
            },
        }
    }

    // TODO: Refactor
    pub fn run_function(&mut self, _id: FunctionId, _args: Vec<ConcreteValue>) -> ConcreteValue {
        panic!()
        // let f = self.module.function_ref(id);
        //
        // if let Some(f) = self.internal_func.get(&f.name) {
        //     return f(&args);
        // }
        //
        // let mut mem = FxHashMap::default();
        //
        // fn get_value(
        //     val: &Value,
        //     args: &[ConcreteValue],
        //     mem: &mut FxHashMap<InstructionId, ConcreteValue>,
        // ) -> ConcreteValue {
        //     match val {
        //         Value::Argument(ArgumentValue { index, .. }) => args[*index].clone(),
        //         Value::Instruction(InstructionValue { id, .. }) => mem.get(&id).unwrap().clone(),
        //         Value::Immediate(im) => match im {
        //             ImmediateValue::Int32(i) => ConcreteValue::Int32(*i),
        //             _ => unimplemented!(),
        //         },
        //         Value::Function(_id) => unimplemented!(),
        //         Value::None => ConcreteValue::Void,
        //     }
        // }
        //
        // let (mut cur_bb_id, mut bb) = f.basic_block_arena.iter().next().unwrap();
        // let mut last_bb_id = cur_bb_id;
        //
        // let ret = 'main: loop {
        //     for val in &*bb.iseq.borrow() {
        //         let inst_id = val.get_inst_id().unwrap();
        //         let inst = &f.inst_table[inst_id];
        //         match &inst.opcode {
        //             Opcode::Add(v1, v2) => {
        //                 let val =
        //                     get_value(&v1, &args, &mut mem).add(get_value(&v2, &args, &mut mem));
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Sub(v1, v2) => {
        //                 let val =
        //                     get_value(&v1, &args, &mut mem).sub(get_value(&v2, &args, &mut mem));
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Mul(v1, v2) => {
        //                 let val =
        //                     get_value(&v1, &args, &mut mem).mul(get_value(&v2, &args, &mut mem));
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Rem(v1, v2) => {
        //                 let val =
        //                     get_value(&v1, &args, &mut mem).rem(get_value(&v2, &args, &mut mem));
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Alloca(ty) => {
        //                 mem.insert(
        //                     inst_id,
        //                     ConcreteValue::Mem(
        //                         match ty {
        //                             Type::Int1 => Box::into_raw(Box::new(0u8)) as *mut u8,
        //                             Type::Int32 => Box::into_raw(Box::new(0u32)) as *mut u8,
        //                             Type::Int64 => Box::into_raw(Box::new(0u64)) as *mut u8,
        //                             Type::F64 => unimplemented!(),
        //                             Type::Pointer(_) => unimplemented!(),
        //                             Type::Void => unreachable!(),
        //                             Type::Function(_) => unimplemented!(),
        //                             Type::Array(a) => unsafe {
        //                                 let mut vec = match a.elem_ty {
        //                                     Type::Int32 => {
        //                                         let mut v = Vec::<*mut i32>::with_capacity(a.len);
        //                                         for _ in 0..a.len {
        //                                             v.push(
        //                                                 Box::into_raw(Box::new(0i32)) as *mut i32
        //                                             );
        //                                         }
        //                                         v
        //                                     }
        //                                     _ => unimplemented!(),
        //                                 };
        //                                 vec.set_len(a.len);
        //                                 Box::into_raw(vec.into_boxed_slice()) as *mut u8
        //                             },
        //                             Type::Struct(_) => unimplemented!(),
        //                         },
        //                         ty.get_pointer_ty(),
        //                     ),
        //                 );
        //             }
        //             Opcode::ICmp(kind, v1, v2) => {
        //                 let val =
        //                     match kind {
        //                         ICmpKind::Eq => get_value(&v1, &args, &mut mem)
        //                             .eq(get_value(&v2, &args, &mut mem)),
        //                         ICmpKind::Le => get_value(&v1, &args, &mut mem)
        //                             .le(get_value(&v2, &args, &mut mem)),
        //                         ICmpKind::Lt => get_value(&v1, &args, &mut mem)
        //                             .lt(get_value(&v2, &args, &mut mem)),
        //                     };
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Br(id) => {
        //                 last_bb_id = cur_bb_id;
        //                 cur_bb_id = *id;
        //                 bb = f.basic_block_ref(*id);
        //                 break;
        //             }
        //             Opcode::CondBr(cond, bb1, bb2) => {
        //                 let cond = get_value(&cond, &args, &mut mem).i1_as_bool().unwrap();
        //                 bb = f.basic_block_ref({
        //                     last_bb_id = cur_bb_id;
        //                     cur_bb_id = if cond { *bb1 } else { *bb2 };
        //                     cur_bb_id
        //                 });
        //                 break;
        //             }
        //             Opcode::Phi(pairs) => {
        //                 let val = get_value(
        //                     &pairs.iter().find(|&(_, bb)| bb == &last_bb_id).unwrap().0,
        //                     &args,
        //                     &mut mem,
        //                 );
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Call(f, f_args) => match f {
        //                 Value::Function(FunctionValue { func_id, .. }) => {
        //                     let val = self.run_function(
        //                         *func_id,
        //                         f_args
        //                             .iter()
        //                             .map(|arg| get_value(&arg, &args, &mut mem))
        //                             .collect(),
        //                     );
        //                     if val != ConcreteValue::Void {
        //                         mem.insert(inst_id, val);
        //                     }
        //                 }
        //                 _ => unimplemented!(),
        //             },
        //             Opcode::Load(v) => {
        //                 let ptr = match get_value(&v, &args, &mut mem) {
        //                     ConcreteValue::Mem(ptr, _) => ptr,
        //                     _ => unreachable!(),
        //                 };
        //                 let val = match v.get_type(self.module).get_element_ty(None).unwrap() {
        //                     Type::Int1 => {
        //                         ConcreteValue::Int1(if unsafe { *(ptr as *mut u8) } == 0 {
        //                             false
        //                         } else {
        //                             true
        //                         })
        //                     }
        //                     Type::Int32 => ConcreteValue::Int32(unsafe { *(ptr as *mut i32) }),
        //                     _ => unimplemented!(),
        //                 };
        //                 mem.insert(inst_id, val);
        //             }
        //             Opcode::Store(src, dst) => {
        //                 let dst_ptr = match get_value(&dst, &args, &mut mem) {
        //                     ConcreteValue::Mem(ptr, _) => ptr,
        //                     _ => unreachable!(),
        //                 };
        //                 match get_value(&src, &args, &mut mem) {
        //                     ConcreteValue::Int32(i) => {
        //                         unsafe { *(dst_ptr as *mut i32) = i };
        //                     }
        //                     _ => unimplemented!(),
        //                 }
        //             }
        //             Opcode::GetElementPtr(ptrval, indices) => {
        //                 let ptr = match get_value(&ptrval, &args, &mut mem) {
        //                     ConcreteValue::Mem(ptr, ref ty) => {
        //                         let mut v = ptr;
        //                         let mut t = ty;
        //                         for idx in indices {
        //                             match t {
        //                                 Type::Pointer(_) => {}
        //                                 Type::Array(_a) => unsafe {
        //                                     // TODO: assume _a.elem_ty is Int32 for now
        //                                     v = *((v as *mut *mut i32).add(
        //                                         match get_value(&idx, &args, &mut mem) {
        //                                             ConcreteValue::Int32(i) => i as usize,
        //                                             _ => unimplemented!(),
        //                                         },
        //                                     ))
        //                                         as *mut u8
        //                                 },
        //                                 _ => unimplemented!(),
        //                             }
        //                             t = t.get_element_ty_with_indices(&vec![*idx]).unwrap();
        //                         }
        //                         v
        //                     }
        //                     _ => unreachable!(),
        //                 };
        //                 mem.insert(inst_id, ConcreteValue::Mem(ptr, inst.ty.clone()));
        //             }
        //             Opcode::Ret(v) => break 'main get_value(&v, &args, &mut mem),
        //         }
        //     }
        // };
        //
        // ret
    }
}

impl ConcreteValue {
    pub fn add(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 + i2),
            _ => unimplemented!(),
        }
    }

    pub fn sub(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 - i2),
            _ => unimplemented!(),
        }
    }

    pub fn mul(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 * i2),
            _ => unimplemented!(),
        }
    }

    pub fn rem(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int32(i1 % i2),
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

    pub fn le(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int1(i1 <= i2),
            _ => unimplemented!(),
        }
    }

    pub fn lt(self, v: ConcreteValue) -> Self {
        match (self, v) {
            (ConcreteValue::Int32(i1), ConcreteValue::Int32(i2)) => ConcreteValue::Int1(i1 < i2),
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

fn cilk_println_i32(args: &[ConcreteValue]) -> ConcreteValue {
    match args[0] {
        ConcreteValue::Int32(i) => println!("{}", i),
        _ => unimplemented!(),
    }
    ConcreteValue::Void
}
