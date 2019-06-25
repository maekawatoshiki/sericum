use super::{function::*, module::*, opcode::*, types::*};

pub type ValueId = usize;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Argument(ArgumentValue),
    Immediate(ImmediateValue),
    Instruction(InstructionValue),
    Function(FunctionId),
    None,
}

#[derive(Debug, Clone, Copy)]
pub struct ArgumentValue {
    pub func_id: FunctionId,
    pub index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct InstructionValue {
    pub func_id: FunctionId,
    pub id: InstructionId,
}

#[derive(Debug, Clone, Copy)]
pub enum ImmediateValue {
    Int32(i32),
}

impl Value {
    pub fn get_type<'a>(&'a self, module: &'a Module) -> &'a Type {
        match self {
            Value::Argument(ArgumentValue { func_id, index }) => {
                let f = module.function_ref(*func_id);
                f.get_param_type(*index).unwrap()
            }
            Value::Instruction(InstructionValue { func_id, id }) => {
                let f = module.function_ref(*func_id);
                &f.instr_table[*id].ty
            }
            Value::Function(id) => &module.function_ref(*id).ty,
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

    pub fn to_string(&self, module: &Module, instr: bool) -> String {
        match self {
            Value::Argument(ArgumentValue { index, func_id }) => {
                let f = module.function_ref(*func_id);
                let ty = f.get_param_type(*index).unwrap();
                format!("{} %arg.{}", ty.to_string(), index)
            }
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
            },
            Value::Instruction(InstructionValue { func_id, id }) if instr => {
                let f = module.function_ref(*func_id);
                let instr = &f.instr_table[*id];
                if instr.ty == Type::Void {
                    format!("    {}", instr.to_string(module))
                } else {
                    format!("    %{} = {}", id.index(), instr.to_string(module))
                }
            }
            Value::Instruction(InstructionValue { func_id, id }) => {
                let f = module.function_ref(*func_id);
                format!("{} %{}", f.instr_table[*id].ty.to_string(), id.index())
            }
            Value::Function(id) if instr => {
                let f = module.function_ref(*id);
                f.to_string(module)
            }
            Value::Function(id) => {
                let f = module.function_ref(*id);
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
}
