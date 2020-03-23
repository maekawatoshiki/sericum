#![feature(proc_macro_hygiene)]
#![feature(stmt_expr_attributes)]
#![feature(drain_filter)]
#![recursion_limit = "128"]

#[macro_use]
pub mod macros;
pub mod codegen;
pub mod exec;
pub mod ir;
pub mod util;

pub use ir::*;

#[macro_use]
extern crate dynasm;
extern crate defs;
extern crate dynasmrt;
extern crate id_arena;
#[macro_use]
extern crate lazy_static;
extern crate rustc_hash;

pub use rustc_hash::{FxHashMap, FxHashSet};
