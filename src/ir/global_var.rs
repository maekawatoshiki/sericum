use id_arena::{Arena, Id};

#[derive(Clone)]
pub struct GlobalVariables {
    arena: Arena<GlobalVariable>,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {}
