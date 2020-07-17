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

impl GlobalVariables {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn new_global_var_with_name(
        &mut self,
        ty: Type,
        linkage: Linkage,
        name: &str,
    ) -> GlobalVariable {
        GlobalVariable {
            ty,
            linkage,
            name: Some(name.to_string()),
        }
    }

    pub fn new_global_var(&mut self, ty: Type, linkage: Linkage) -> GlobalVariable {
        GlobalVariable {
            ty,
            linkage,
            name: None,
        }
    }
}
