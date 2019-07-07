#![feature(proc_macro_hygiene)]

#[macro_use]
pub mod macros;
pub mod exec;
pub mod ir;

#[macro_use]
extern crate dynasm;
extern crate dynasmrt;
extern crate id_arena;
extern crate rustc_hash;
