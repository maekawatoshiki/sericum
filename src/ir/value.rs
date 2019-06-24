use super::{function::*, opcode::*, types::*};

pub type ValueId = usize;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Argument(usize),
    Immediate(ImmediateValue),
    Instruction(InstructionId),
    None,
}

#[derive(Debug, Clone, Copy)]
pub enum ImmediateValue {
    Int32(i32),
}

impl Value {
    pub fn get_type<'a>(&'a self, function: &'a Function) -> &'a Type {
        match self {
            Value::Argument(offset) => function.get_param_type(*offset).unwrap(),
            Value::Instruction(id) => &function.instr_table.get(*id).unwrap().ty,
            Value::Immediate(ref im) => im.get_type(),
            Value::None => &Type::Void,
        }
    }

    pub fn get_instr_id(&self) -> Option<InstructionId> {
        match self {
            Value::Instruction(id) => Some(*id),
            _ => None,
        }
    }

    pub fn to_string(&self, f: &Function, instr: bool) -> String {
        match self {
            Value::Argument(n) => {
                let ty = f.get_param_type(*n).unwrap();
                format!("{} %arg.{}", ty.to_string(), n)
            }
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
            },
            Value::Instruction(id) if instr => {
                let instr = &f.instr_table[*id];
                if instr.ty == Type::Void {
                    format!("    {}", instr.to_string(f))
                } else {
                    format!("    %{} = {}", id.index(), instr.to_string(f))
                }
            }
            Value::Instruction(id) => {
                format!("{} %{}", f.instr_table[*id].ty.to_string(), id.index())
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
}
