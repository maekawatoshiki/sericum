use proc_macro::TokenStream;
use proc_quote::quote;

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
