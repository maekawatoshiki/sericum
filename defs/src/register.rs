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

// class GR32 (32, Int32) < GR64 {
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
        let super_rc = match input.parse::<Token![<]>() {
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

trait DefinitionConstructible {
    fn construct(&self) -> TS;
}

impl DefinitionConstructible for Registers {
    fn construct(&self) -> TS {
        let mut constants = quote! {};
        let mut class_definition = quote! {};
        let mut regs_total_num = 0;

        for class in &self.class {
            let name = str2ident(format!("{}_NUM", class.name.to_string()).as_str());
            let num = class.body.0.len() as isize;
            constants = quote! {
                #constants
                const #name: isize = #num;
            };
            regs_total_num += num as usize;

            let class = &class.construct();
            class_definition = quote! {
                #class_definition
                #class
            };
        }

        constants = quote! {
            #constants
            pub const PHYS_REGISTERS_NUM: usize = #regs_total_num;
        };

        quote! {
            #constants
            #class_definition
        }
    }
}

impl DefinitionConstructible for RegisterClass {
    fn construct(&self) -> TS {
        // body, name
        let list = self
            .body
            .0
            .iter()
            .fold(quote! {}, |acc, x| quote! { #acc #x, });
        let name = &self.name;
        quote! {
            #[derive(Debug, Clone, Copy, Hash, PartialEq)]
            pub enum #name {
                #list
            }
        }
    }
}

// impl DefinitionConstructible for RegisterOrder {
//     fn construct(&self) -> TS {}
// }

// RegisterClass GR32 (i32) {
//    EAX, EBX, .... }
pub fn run(item: TokenStream) -> TokenStream {
    let regs = parse_macro_input!(item as Registers);
    TokenStream::from(regs.construct())
}

fn str2ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}
