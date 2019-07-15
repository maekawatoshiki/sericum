#![feature(proc_macro_hygiene)]
#![feature(drain_filter)]

#[macro_use]
pub mod macros;
pub mod exec;
pub mod ir;
pub mod pass;

pub use ir::*;

#[macro_use]
extern crate dynasm;
extern crate dynasmrt;
extern crate id_arena;
extern crate quote;
extern crate rustc_hash;

pub use rustc_hash::{FxHashMap, FxHashSet};
