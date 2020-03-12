#![feature(proc_macro_span)]

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate proc_quote;

mod register;

#[proc_macro]
pub fn define_registers(item: TokenStream) -> TokenStream {
    register::run(item)
}
