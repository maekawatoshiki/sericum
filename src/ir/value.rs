use super::{
    function::*, global_val::GlobalVariableId, module::*, opcode::*, types::*, DumpToString,
};
use std::hash;

macro_rules! const_op {
    ($name:ident, $op:tt) => {
    pub fn $name(&self, v: &Value) -> Option<Value> {
        use ImmediateValue::*;
        match (self, v) {
            (Value::Immediate(Int8(x)), Value::Immediate(Int8(y))) => Some(Value::Immediate(Int8(x $op y))),
            (Value::Immediate(Int32(x)), Value::Immediate(Int32(y))) => Some(Value::Immediate(Int32(x $op y))),
            (Value::Immediate(F64(x)), Value::Immediate(F64(y))) => Some(Value::Immediate(F64(x $op y))),
            _ => None,
        }
    } };
    (int_only $name:ident, $op:tt) => {
    pub fn $name(&self, v: &Value) -> Option<Value> {
        use ImmediateValue::*;
        match (self, v) {
            (Value::Immediate(Int8(x)), Value::Immediate(Int8(y))) => Some(Value::Immediate(Int8(x $op y))),
            (Value::Immediate(Int32(x)), Value::Immediate(Int32(y))) => Some(Value::Immediate(Int32(x $op y))),
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
    Global(GlobalValue),
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

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct GlobalValue {
    pub id: GlobalVariableId,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ImmediateValue {
    Int8(i8),
    Int32(i32),
    Int64(i64),
    F64(f64),
}

impl hash::Hash for ImmediateValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int8(i) => i.hash(state),
            Self::Int32(i) => i.hash(state),
            Self::Int64(i) => i.hash(state),
            Self::F64(f) => unsafe { ::std::mem::transmute::<f64, u64>(*f) }.hash(state),
        }
    }
}

impl Eq for ImmediateValue {}

impl Value {
    pub fn new_imm_int8(i: i8) -> Self {
        Self::Immediate(ImmediateValue::Int8(i))
    }

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
            Value::Global(GlobalValue { ty, .. }) => *ty,
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
                ImmediateValue::Int8(i) => format!("i8 {}", i),
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
            Value::Global(GlobalValue { id, ty }) => {
                let g = &parent.global_vars.arena[*id];
                format!("{} @{}", parent.types.to_string(*ty), g.name)
            }
            Value::None => "".to_string(),
        }
    }

    pub fn get_imm(&self) -> Option<&ImmediateValue> {
        match self {
            Self::Immediate(imm) => Some(imm),
            _ => None,
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
    pub fn is_power_of_two(&self) -> Option<u32> {
        match self {
            Self::Int8(x) if (*x as usize).is_power_of_two() => Some(x.trailing_zeros()),
            Self::Int32(x) if (*x as usize).is_power_of_two() => Some(x.trailing_zeros()),
            Self::Int64(x) if (*x as usize).is_power_of_two() => Some(x.trailing_zeros()),
            Self::Int64(_) | Self::Int32(_) | Self::Int8(_) | Self::F64(_) => None,
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            ImmediateValue::Int8(_) => &Type::i8,
            ImmediateValue::Int32(_) => &Type::i32,
            ImmediateValue::Int64(_) => &Type::i64,
            ImmediateValue::F64(_) => &Type::f64,
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
