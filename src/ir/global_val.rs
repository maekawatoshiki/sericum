use super::types::{Type, Types};
use id_arena::{Arena, Id};
use std::fmt;

pub type GlobalVariableId = Id<GlobalVariable>;

#[derive(Clone)]
pub struct GlobalVariables {
    pub arena: Arena<GlobalVariable>,
    types: Types,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub ty: Type,
    pub linkage: Linkage,
    pub name: String,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Linkage {
    Common,
    External,
    // TODO ...
}

impl GlobalVariables {
    pub fn new(types: Types) -> Self {
        Self {
            arena: Arena::new(),
            types,
        }
    }

    pub fn new_global_var_with_name(
        &mut self,
        ty: Type,
        linkage: Linkage,
        name: &str,
    ) -> GlobalVariableId {
        let ptr_ty = self.types.new_pointer_ty(ty);
        let id = self.arena.alloc(GlobalVariable {
            ty,
            linkage,
            name: name.to_string(),
        });
        self.types
            .base
            .borrow_mut()
            .gblvar_ptr_types
            .insert(id, ptr_ty);
        id
    }

    pub fn new_global_var(&mut self, ty: Type, linkage: Linkage) -> GlobalVariableId {
        self.arena.alloc(GlobalVariable {
            ty,
            linkage,
            name: "anony".to_string(),
        })
    }
}

impl fmt::Debug for GlobalVariables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (_, g) in &self.arena {
            writeln!(
                f,
                "@{} = {:?} global {}",
                g.name,
                g.linkage,
                self.types.to_string(g.ty)
            )?;
        }
        fmt::Result::Ok(())
    }
}

impl fmt::Debug for Linkage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Common => write!(f, "common"),
            Self::External => write!(f, "external"),
        }
    }
}
