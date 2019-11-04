use super::{function::*, module::*, opcode::*, types::*};

pub type ValueId = usize;

macro_rules! const_op { ($name:ident, $op:tt) => {
    pub fn $name(&self, v: &Value) -> Option<Value> {
        use ImmediateValue::*;
        match (self, v) {
            (Value::Immediate(imm1), Value::Immediate(imm2)) => match (imm1, imm2) {
                (Int32(i1), Int32(i2)) => Some(Value::Immediate(Int32(i1 $op i2))),
            },
            _ => None,
        }
    }
}}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Argument(ArgumentValue),
    Immediate(ImmediateValue),
    Instruction(InstructionValue),
    Function(FunctionValue),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArgumentValue {
    pub func_id: FunctionId,
    pub index: usize,
    pub parent: ModuleRef,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstructionValue {
    pub func_id: FunctionId,
    pub id: InstructionId,
    pub parent: ModuleRef,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionValue {
    pub func_id: FunctionId,
    pub parent: ModuleRef,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ImmediateValue {
    Int32(i32),
}

impl Value {
    pub fn new_imm_int32(i: i32) -> Self {
        Self::Immediate(ImmediateValue::Int32(i))
    }

    pub fn new_func(f: FunctionValue) -> Self {
        Self::Function(f)
    }

    pub fn get_type(&self) -> &Type {
        match self {
            Value::Argument(ArgumentValue {
                func_id,
                index,
                parent,
            }) => {
                let f = parent.function_ref(*func_id);
                f.get_param_type(*index).unwrap()
            }
            Value::Instruction(InstructionValue {
                func_id,
                id,
                parent,
            }) => {
                let f = parent.function_ref(*func_id);
                &f.instr_table[*id].ty
            }
            Value::Function(FunctionValue { func_id, parent }) => &parent.function_ref(*func_id).ty,
            Value::Immediate(ref im) => im.get_type(),
            Value::None => &Type::Void,
        }
    }

    pub fn get_instr_id(&self) -> Option<InstructionId> {
        match self {
            Value::Instruction(InstructionValue { id, .. }) => Some(*id),
            _ => None,
        }
    }

    // Constant folding

    const_op!(const_add, +);
    const_op!(const_sub, -);
    const_op!(const_mul, *);
    const_op!(const_rem, %);

    // Utils

    pub fn to_string(&self, instr: bool) -> String {
        match self {
            Value::Argument(ArgumentValue {
                index,
                func_id,
                parent,
            }) => {
                let f = parent.function_ref(*func_id);
                let ty = f.get_param_type(*index).unwrap();
                format!("{} %arg.{}", ty.to_string(), index)
            }
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
            },
            Value::Instruction(InstructionValue {
                func_id,
                id,
                parent,
            }) if instr => {
                let f = parent.function_ref(*func_id);
                let instr = &f.instr_table[*id];
                if instr.ty == Type::Void {
                    format!("    {}", instr.to_string())
                } else {
                    format!("    %{} = {}", id.index(), instr.to_string())
                }
            }
            Value::Instruction(InstructionValue {
                func_id,
                id,
                parent,
            }) => {
                let f = parent.function_ref(*func_id);
                format!("{} %{}", f.instr_table[*id].ty.to_string(), id.index())
            }
            Value::Function(FunctionValue { func_id, parent }) if instr => {
                let f = parent.function_ref(*func_id);
                f.to_string()
            }
            Value::Function(FunctionValue { func_id, parent }) => {
                let f = parent.function_ref(*func_id);
                let fty = f.ty.get_function_ty().unwrap();
                format!("{} {}", fty.ret_ty.to_string(), f.name)
            }
            Value::None => "".to_string(),
        }
    }
}

impl ImmediateValue {
    pub fn get_type(&self) -> &Type {
        match self {
            ImmediateValue::Int32(_) => &Type::Int32,
        }
    }

    pub fn as_int32(&self) -> i32 {
        match self {
            ImmediateValue::Int32(i) => *i,
        }
    }
}
