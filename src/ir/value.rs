use super::types::*;

pub type ValueId = usize;

#[derive(Debug, Clone)]
pub enum Value {
    Immediate(ImmediateValue),
    Id(usize, Type),
    None,
}

#[derive(Debug, Clone)]
pub enum ImmediateValue {
    Int32(i32),
}

impl Value {
    pub fn get_type(&self) -> &Type {
        match self {
            Value::Id(_, t) => &t,
            Value::Immediate(ref im) => im.get_type(),
            Value::None => &Type::Void,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
            },
            Value::Id(id, t) => format!("{} %{}", t.to_string(), id),
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
