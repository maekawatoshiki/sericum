use crate::parser::*;
use proc_macro::TokenStream;
use proc_quote::quote;

// RegisterClass GR32 (i32) {
//    EAX, EBX, .... }
pub fn run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());

    while reader.skip_ident("RegisterClass") {
        let _reg_class = reader.get_ident().unwrap();
        // panic!("reg class name: {}", reg_class);
    }

    let expanded = quote! {

    #[derive(Debug, Clone, Copy, Hash, PartialEq)]
    pub enum GR32 {
        EAX,
        EBX,
        ECX,
        EDX,
        ESI,
        EDI,
        EBP,
        ESP,
        R8D,
        R9D,
        R10D,
        R11D,
        R12D,
        R13D,
        R14D,
        R15D,
    }

    };

    TokenStream::from(expanded)
}
