pub type ValueId = usize;

#[derive(Debug, Clone)]
pub enum Value {
    Immediate(ImmediateValue),
    Id(usize),
    None,
}

#[derive(Debug, Clone)]
pub enum ImmediateValue {
    Int32(i32),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Immediate(iv) => match iv {
                ImmediateValue::Int32(i) => format!("i32 {}", i),
            },
            Value::Id(id) => format!("%{}", id),
            Value::None => "".to_string(),
        }
    }
}
