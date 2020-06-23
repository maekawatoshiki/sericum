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

// class GR32 (32, Int32, [Int32, Int64, Pointer!]) < GR64 {
//      EAX,
//      ECX,
//      ...,
//      R15D,
// }

struct RegisterClass {
    name: Ident,
    bit: i32,
    rc2ty: TS,
    tys2rc: Vec<TS>,
    super_rc: Option<Ident>,
    body: RegisterList,
}

struct BitAndTypes(pub i32, pub TS, pub Vec<TS>);
struct Types(pub Vec<TS>);

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
        let BitAndTypes(bit, rc2ty, tys2rc) =
            syn::parse2::<BitAndTypes>(input.parse::<Group>()?.stream())?;
        let super_rc = match input.parse::<Token![<]>() {
            Ok(_) => Some(input.parse::<Ident>()?),
            Err(_) => None,
        };
        let body: RegisterList = syn::parse2(input.parse::<Group>()?.stream())?;
        Ok(Self {
            name,
            bit,
            rc2ty,
            tys2rc,
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

impl Parse for BitAndTypes {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let bit = input.parse::<LitInt>()?.to_string().parse::<i32>().unwrap();
        assert!(input.parse::<Token![,]>().is_ok());
        let rc2ty = parse_ty(input)?;
        assert!(input.parse::<Token![,]>().is_ok());
        let tys2rc = syn::parse2::<Types>(input.parse::<Group>()?.stream())?.0;
        Ok(Self(bit, rc2ty, tys2rc))
    }
}

impl Parse for Types {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut types = vec![];
        loop {
            types.push(parse_ty(input)?);
            if input.parse::<Token![,]>().is_err() {
                break;
            }
        }
        Ok(Self(types))
    }
}

fn parse_ty(input: ParseStream) -> Result<TS, Error> {
    let ty = input.parse::<Ident>()?;
    Ok(if input.parse::<Token![!]>().is_ok() {
        quote! { #ty(_) }
    } else {
        quote! { #ty }
    })
}

trait DefinitionConstructible {
    fn construct(&self) -> TS;
}

impl DefinitionConstructible for Registers {
    fn construct(&self) -> TS {
        let mut constants = quote! {};
        let mut ty2rc = quote! {};
        let mut rc2ty = quote! {};
        let mut reg_class_kind = quote! {};
        let mut class_definition = quote! {};
        let mut regs_total_num = 0;

        let mut reg_enum_num = quote! { 0 };
        for class in &self.class {
            let name = &class.name;
            let const_name = str2ident(format!("{}_NUM", name.to_string()).as_str());
            let num = class.body.0.len() as isize;
            constants = quote! {
                #constants
                const #const_name: isize = #num;
            };
            regs_total_num += num as usize;
            reg_class_kind = quote! {
                #reg_class_kind
                #name = #reg_enum_num,
            };
            reg_enum_num = quote! { #reg_enum_num + #const_name };

            for t in &class.tys2rc {
                ty2rc = quote! { #ty2rc Type::#t => Some(RegisterClassKind::#name), }
            }
            let t = &class.rc2ty;
            rc2ty = quote! {
                #rc2ty
                RegisterClassKind::#name => Type::#t,
            };

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

        ty2rc = quote! {
            pub fn ty2rc(ty: &Type) -> Option<RegisterClassKind> {
                match ty {
                    Type::Void => None,
                    Type::Array(_) => None,
                    #ty2rc
                    e => unimplemented!("{:?}", e),
                }
            }
        };

        rc2ty = quote! {
            pub fn rc2ty(rc: RegisterClassKind) -> Type {
                match rc {
                    #rc2ty
                }
            }
        };

        reg_class_kind = quote! {
            #[derive(Debug, Clone, Copy, Hash, PartialEq)]
            pub enum RegisterClassKind {
                #reg_class_kind
            }
        };

        quote! {
            #constants
            #ty2rc
            #rc2ty
            #reg_class_kind
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
