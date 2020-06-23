#![feature(proc_macro_span)]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate proc_macro_error;
extern crate proc_quote;
extern crate rand;
extern crate syn;

use proc_macro_error::*;

mod isel_pat;
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
