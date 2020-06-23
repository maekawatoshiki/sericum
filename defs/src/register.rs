use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Group, Ident, Span},
    *,
};
use proc_quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, token, Error, LitInt, Token};

type TS = proc_macro2::TokenStream;

struct Registers {
    class: Vec<RegisterClass>,
    order: Vec<RegisterOrder>,
}

enum DefKind {
    Class,
    Order,
}

// class GR32 (64, Int64) : GR64 {
//      EAX,
//      ECX,
//      ...,
//      R15D,
// }

struct RegisterClass {
    name: Ident,
    bit: i32,
    ty: Ident,
    super_rc: Option<Ident>,
    body: RegisterList,
}

struct BitAndType(pub i32, pub Ident);

struct RegisterList(pub Vec<Ident>);

// order gp GR32 { EAX, ... }
// order arg GR32 { EDI, ... }

struct RegisterOrder {
    kind: RegisterOrderKind,
    class: Ident,
    body: RegisterList,
}

enum RegisterOrderKind {
    GeneralPurpose,
    Argument,
}

impl Parse for Registers {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut class = vec![];
        let mut order = vec![];

        while !input.is_empty() {
            let ident = input.parse::<Ident>()?;
            match ident.to_string().as_str() {
                "class" => class.push(input.parse::<RegisterClass>()?),
                "order" => order.push(input.parse::<RegisterOrder>()?),
                _ => {
                    return Err(Error::new(
                        ident.span(),
                        "expected 'class', 'gp_order' or 'arg_order'",
                    ))
                }
            }
        }

        Ok(Self { class, order })
    }
}

impl Parse for RegisterClass {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let name = input.parse::<Ident>()?;
        let BitAndType(bit, ty) = syn::parse2::<BitAndType>(input.parse::<Group>()?.stream())?;
        let super_rc = match input.parse::<Token![:]>() {
            Ok(_) => Some(input.parse::<Ident>()?),
            Err(_) => None,
        };
        let body: RegisterList = syn::parse2(input.parse::<Group>()?.stream())?;
        Ok(Self {
            name,
            bit,
            ty,
            super_rc,
            body,
        })
    }
}

impl Parse for RegisterOrder {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let kind = match input.parse::<Ident>()?.to_string().as_str() {
            "gp" => RegisterOrderKind::GeneralPurpose,
            "arg" => RegisterOrderKind::Argument,
            _ => return Err(Error::new(input.span(), "expected 'gp' or 'arg'")),
        };
        let class = input.parse::<Ident>()?;
        let body: RegisterList = syn::parse2(input.parse::<Group>()?.stream())?;
        Ok(Self { kind, class, body })
    }
}

impl Parse for RegisterList {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut list = vec![];
        loop {
            list.push(input.parse::<Ident>()?);
            if input.parse::<Token![,]>().is_err() {
                break;
            }
        }
        Ok(Self(list))
    }
}

impl Parse for BitAndType {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let bit = input.parse::<LitInt>()?.to_string().parse::<i32>().unwrap();
        assert!(input.parse::<Token![,]>().is_ok());
        let ty = input.parse::<Ident>()?;
        Ok(Self(bit, ty))
    }
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
