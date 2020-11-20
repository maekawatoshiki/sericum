#![feature(proc_macro_span)]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate proc_macro_error;
extern crate proc_quote;
extern crate rand;
extern crate syn;

use proc_macro_error::*;

mod isel_pat;
mod node_gen;
mod register;

#[proc_macro]
pub fn registers(item: TokenStream) -> TokenStream {
    register::run(item)
}

#[proc_macro_error]
#[proc_macro]
pub fn isel_pat(item: TokenStream) -> TokenStream {
    isel_pat::run(item)
}

#[proc_macro_error]
#[proc_macro]
pub fn node_gen(item: TokenStream) -> TokenStream {
    node_gen::run(item)
}
