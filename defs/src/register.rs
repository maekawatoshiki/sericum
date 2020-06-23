use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Group, Ident, Span},
    *,
};
use proc_quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, token, Error, LitInt, Token};

type TS = proc_macro2::TokenStream;

// class GR32 (64, Int64) : GR64 {
//      EAX,
//      ECX,
//      ...,
//      R15D,
// }

struct RegisterClass {
    name: String,
    bit: i32,
    ty: TS,
    super_rc: String,
    body: RegisterList,
}

struct RegisterList(pub Vec<Ident>);

// gp_order GR32 { EAX, ... }
// arg_order GR32 { EDI, ... }

struct RegisterOrder {
    kind: RegisterOrderKind,
}

enum RegisterOrderKind {
    GeneralPurpose,
    Argument,
}

// RegisterClass GR32 (i32) {
//    EAX, EBX, .... }
pub fn run(_item: TokenStream) -> TokenStream {
    let expanded = quote! {

    #[derive(Debug, Clone, Copy, Hash, PartialEq)]
    pub enum GR32 {
        EAX ,
        ECX ,
        EDX ,
        EBX ,
        ESP ,
        EBP ,
        ESI ,
        EDI ,
        R8D ,
        R9D ,
        R10D,
        R11D,
        R12D,
        R13D,
        R14D,
        R15D
    }

    };

    TokenStream::from(expanded)
}
