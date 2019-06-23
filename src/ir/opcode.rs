use super::types::*;
use super::value::*;

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Add(Value, Value),
    Ret(Value),
}
