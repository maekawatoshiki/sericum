use super::types::*;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub ret_ty: Type,
    pub params_ty: Vec<Type>,
}
