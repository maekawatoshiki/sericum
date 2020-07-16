use super::types::Type;
use id_arena::Arena;

#[derive(Clone)]
pub struct GlobalVariables {
    arena: Arena<GlobalVariable>,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    ty: Type,
    linkage: Linkage,
    name: Option<String>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Linkage {
    Common,
    External,
    // TODO ...
}
