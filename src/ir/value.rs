use super::{function::*, module::*, opcode::*, types::*, DumpToString};
use std::hash;

macro_rules! const_op {
    ($name:ident, $op:tt) => {
    pub fn $name(&self, v: &Value) -> Option<Value> {
        use ImmediateValue::*;
        match (self, v) {
            (Value::Immediate(Int32(i1)), Value::Immediate(Int32(i2))) => Some(Value::Immediate(Int32(i1 $op i2))),
            (Value::Immediate(F64(i1)), Value::Immediate(F64(i2))) => Some(Value::Immediate(F64(i1 $op i2))),
            _ => None,
        }
    } };
    (int_only $name:ident, $op:tt) => {
    pub fn $name(&self, v: &Value) -> Option<Value> {
        use ImmediateValue::*;
        match (self, v) {
            (Value::Immediate(Int32(i1)), Value::Immediate(Int32(i2))) => Some(Value::Immediate(Int32(i1 $op i2))),
            _ => None,
        }
    } }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Value {
    Argument(ArgumentValue),
    Immediate(ImmediateValue),
    Instruction(InstructionValue),
    Function(FunctionValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct ArgumentValue {
    pub func_id: FunctionId,
    pub index: usize,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct InstructionValue {
    pub func_id: FunctionId,
    pub id: InstructionId,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct FunctionValue {
    pub func_id: FunctionId,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ImmediateValue {
    Int32(i32),
    Int64(i64),
    F64(f64),
}

impl hash::Hash for ImmediateValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int32(i) => i.hash(state),
            Self::Int64(i) => i.hash(state),
            Self::F64(f) => unsafe { ::std::mem::transmute::<f64, u64>(*f) }.hash(state),
        }
    }
}

impl Eq for ImmediateValue {}

impl Value {
    pub fn new_imm_int32(i: i32) -> Self {
        Self::Immediate(ImmediateValue::Int32(i))
    }

    pub fn new_imm_f64(f: f64) -> Self {
        Self::Immediate(ImmediateValue::F64(f))
    }

    pub fn new_func(f: FunctionValue) -> Self {
        Self::Function(f)
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::Argument(ArgumentValue { ty, .. }) => *ty,
            Value::Instruction(InstructionValue { ty, .. }) => *ty,
            Value::Function(FunctionValue { ty, .. }) => *ty,
            Value::Immediate(ref im) => *im.get_type(),
            Value::None => Type::Void,
        }
    }

    pub fn get_inst_id(&self) -> Option<InstructionId> {
        match self {
            Value::Instruction(InstructionValue { id, .. }) => Some(*id),
            _ => None,
        }
    }

    // Constant folding

    const_op!(const_add, +);
    const_op!(const_sub, -);
    const_op!(const_mul, *);
    const_op!(const_div, /);
    const_op!(int_only const_rem, %);

    // Utils

    pub fn to_string(&self, parent: &Module, inst: bool) -> String {
        match self {
            Value::Argument(ArgumentValue { index, func_id, .. }) => {
                let f = parent.function_ref(*func_id);
                let ty = f.get_param_type(*index).unwrap();
                format!("{} %arg.{}", ty.to_string(), index)
            }
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
                ImmediateValue::Int64(i) => format!("i64 {}", i),
                ImmediateValue::F64(f) => format!("f64 {}", f),
            },
            Value::Instruction(InstructionValue { func_id, id, .. }) if inst => {
                let f = parent.function_ref(*func_id);
                let inst = &f.inst_table[*id];
                if inst.ty == Type::Void {
                    format!("    {}", inst.to_string(parent))
                } else {
                    format!("    %{} = {}", id.index(), inst.to_string(parent))
                }
            }
            Value::Instruction(InstructionValue { func_id, id, .. }) => {
                let f = parent.function_ref(*func_id);
                format!(
                    "{} %{}",
                    parent.types.to_string(f.inst_table[*id].ty),
                    id.index()
                )
            }
            Value::Function(FunctionValue { func_id, .. }) if inst => {
                let f = parent.function_ref(*func_id);
                f.dump(parent)
            }
            Value::Function(FunctionValue { func_id, .. }) => {
                let f = parent.function_ref(*func_id);
                let ret_ty = parent
                    .types
                    .base
                    .borrow()
                    .as_function_ty(f.ty)
                    .unwrap()
                    .ret_ty;
                format!("{} {}", parent.types.to_string(ret_ty), f.name)
            }
            Value::None => "".to_string(),
        }
    }

    pub fn as_imm(&self) -> &ImmediateValue {
        match self {
            Value::Immediate(imm) => imm,
            _ => panic!(),
        }
    }

    pub fn as_instruction(&self) -> &InstructionValue {
        match self {
            Self::Instruction(iv) => iv,
            _ => panic!(),
        }
    }
}

impl ImmediateValue {
    pub fn get_type(&self) -> &Type {
        match self {
            ImmediateValue::Int32(_) => &Type::Int32,
            ImmediateValue::Int64(_) => &Type::Int64,
            ImmediateValue::F64(_) => &Type::F64,
        }
    }

    pub fn as_int32(&self) -> i32 {
        match self {
            ImmediateValue::Int32(i) => *i,
            _ => panic!(),
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            ImmediateValue::F64(f) => *f,
            _ => panic!(),
        }
    }
}
