pub mod dom_tree;
pub mod loops;

use dyn_clone::{clone_trait_object, DynClone};
use std::any::Any;

pub trait Analysis: DynClone {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

clone_trait_object!(Analysis);
