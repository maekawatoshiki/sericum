use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Group, Ident, Span},
    *,
};
use proc_quote::quote;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Error, LitInt, Token};

type TS = proc_macro2::TokenStream;

struct Registers {
    class: Vec<(String, RegisterClass)>,
    order: Vec<RegisterOrder>,
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
    ret_regs: RegisterList,
    super_rc: Option<Ident>,
    body: RegisterList,
}

struct RCInfo(pub i32, pub TS, pub Vec<TS>, pub RegisterList);
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
                "class" => {
                    let c = input.parse::<RegisterClass>()?;
                    class.push((c.name.to_string(), c));
                }
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
        let RCInfo(bit, rc2ty, tys2rc, ret_regs) =
            syn::parse2::<RCInfo>(input.parse::<Group>()?.stream())?;
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
            ret_regs,
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

impl Parse for RCInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let bit = input.parse::<LitInt>()?.to_string().parse::<i32>().unwrap();
        assert!(input.parse::<Token![,]>().is_ok());
        let rc2ty = parse_ty(input)?;
        assert!(input.parse::<Token![,]>().is_ok());
        let tys2rc = syn::parse2::<Types>(input.parse::<Group>()?.stream())?.0;
        assert!(input.parse::<Token![,]>().is_ok());
        let ret_regs = syn::parse2::<RegisterList>(input.parse::<Group>()?.stream())?;
        Ok(Self(bit, rc2ty, tys2rc, ret_regs))
    }
}

impl Parse for Types {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut types = vec![];
        while !input.cursor().eof() {
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
        let mut physreg_reg_class = quote![unreachable!()];
        let mut class_definition = quote! {};
        let mut trt_physreg_sub = quote! {};
        let mut trt_physreg_super = quote! {};
        let mut registers_info_new = quote![];
        let mut physregs_name = quote![];
        let mut str2reg = quote! {};
        let mut reg_class_kind_size_in_bits = quote! {};
        let mut ret_val_regs = quote! {};
        let mut regs_total_num = 0;
        let mut reg_sub_super: HashMap<String, SubSuper> = HashMap::new();
        let mut reg_set: Vec<Vec<String>> = Vec::new();
        struct SubSuper {
            sub: Option<TS>,
            super_: Option<TS>,
        };
        impl SubSuper {
            pub fn new() -> Self {
                Self {
                    sub: None,
                    super_: None,
                }
            }
        }
        fn add_reg(reg_set: &mut Vec<Vec<String>>, a: String, b: Option<String>) {
            if let Some(b) = &b {
                if let Some(x) = reg_set.iter_mut().find(|x| x.contains(&b)) {
                    let p = x.iter().position(|x| x == b).unwrap();
                    x.insert(p, a);
                    return;
                }
            }
            if let Some(x) = reg_set.iter_mut().find(|x| x.contains(&a)) {
                if let Some(b) = b {
                    let p = x.iter().position(|x| x == &a).unwrap();
                    x.insert(p + 1, b);
                }
                return;
            }
            let mut s = vec![a];
            if let Some(b) = b {
                s.push(b)
            }
            reg_set.push(s)
        }

        let mut reg_enum_num = quote! { 0 };
        for (_name, class) in &self.class {
            let class_name = &class.name;
            let const_name = str2ident(format!("{}_NUM", class_name.to_string()).as_str());
            let num = class.body.0.len() as isize;
            constants = quote! {
                #constants
                const #const_name: isize = #num;
            };
            regs_total_num += num as usize;
            reg_class_kind = quote! {
                #reg_class_kind
                #class_name = #reg_enum_num,
            };
            reg_enum_num = quote! { #reg_enum_num + #const_name };
            physreg_reg_class = quote! {
                if RegisterClassKind::#class_name as usize <= n {
                    return RegisterClassKind::#class_name;
                } else {
                    #physreg_reg_class
                }
            };
            let bit = class.bit as usize;
            reg_class_kind_size_in_bits = quote! {
                #reg_class_kind_size_in_bits
                Self::#class_name => #bit,
            };
            let r = &class.ret_regs.0[0];
            ret_val_regs = quote! {
                #ret_val_regs
                Self::#class_name => #class_name::#r.as_phys_reg(),
            };

            for t in &class.tys2rc {
                ty2rc = quote! { #ty2rc Type::#t => Some(RegisterClassKind::#class_name), }
            }
            let t = &class.rc2ty;
            rc2ty = quote! {
                #rc2ty
                RegisterClassKind::#class_name => Type::#t,
            };

            let class_name_str = class_name.to_string();
            reg_sub_super
                .entry(class_name_str.clone())
                .or_insert(SubSuper::new());
            let super_name = if let Some(super_rc) = &class.super_rc {
                let super_name = super_rc.to_string();
                add_reg(
                    &mut reg_set,
                    class_name_str.clone(),
                    Some(super_name.clone()),
                );

                reg_sub_super
                    .entry(super_name.clone())
                    .or_insert(SubSuper::new());
                Some(super_name)
            } else {
                add_reg(&mut reg_set, class_name_str.clone(), None);
                None
            };
            for (i, r) in class.body.0.iter().enumerate() {
                let name = r.to_string().to_ascii_lowercase();
                physregs_name = quote! { #physregs_name #name, };
                registers_info_new = quote! {
                    #registers_info_new
                    phys_regs_list.push(f(&mut arena, #class_name::#r));
                };
                str2reg = quote! {
                    #str2reg
                    #name => #class_name::#r.as_phys_reg(),
                };
                trt_physreg_sub = quote! {
                    #trt_physreg_sub
                    #class_name::#r.sub_reg(),
                };
                trt_physreg_super = quote! {
                    #trt_physreg_super
                    #class_name::#r.super_reg(),
                };
                if let Some(super_name_str) = &super_name {
                    let super_name = str2ident(super_name_str.as_str());
                    let super_ = &mut reg_sub_super
                        .get_mut(class_name_str.as_str())
                        .unwrap()
                        .super_;
                    let name2 = &self
                        .class
                        .iter()
                        .find(|(name, _)| name == super_name_str)
                        .unwrap()
                        .1
                        .body
                        .0[i];
                    let name = str2ident(r.to_string().as_str());
                    if let Some(super_) = super_ {
                        *super_ = quote! {
                            #super_
                            #class_name::#name => #super_name::#name2.as_phys_reg(),
                        };
                    } else {
                        *super_ = Some(quote! {
                            #class_name::#name => #super_name::#name2.as_phys_reg(),
                        });
                    }
                    let sub = &mut reg_sub_super.get_mut(super_name_str).unwrap().sub;
                    if let Some(sub) = sub {
                        *sub = quote! {
                            #sub
                            #super_name::#name2 => #class_name::#name.as_phys_reg(),
                        };
                    } else {
                        *sub = Some(quote! {
                            #super_name::#name2 => #class_name::#name.as_phys_reg(),
                        });
                    }
                }
            }

            let class = &class.construct();
            class_definition = quote! {
                #class_definition
                #class
            };
        }

        let mut arg_orders = quote! {};
        let mut gp_orders = quote! {};
        for order in &self.order {
            match &order.kind {
                RegisterOrderKind::Argument => {
                    let mut arg_order = quote! {};
                    let class = &order.class;
                    for r in &order.body.0 {
                        arg_order = quote! {
                            #arg_order
                            #class::#r.as_phys_reg(),
                        };
                    }

                    arg_order = quote! {
                        RegisterClassKind::#class => vec![
                            #arg_order
                        ]
                    };

                    arg_orders = quote! {
                        #arg_orders
                        #arg_order,
                    };
                }
                RegisterOrderKind::GeneralPurpose => {
                    let mut gp_order = quote! {};
                    let class = &order.class;
                    for r in &order.body.0 {
                        gp_order = quote! {
                            #gp_order
                            #class::#r.as_phys_reg(),
                        };
                    }

                    gp_order = quote! {
                        RegisterClassKind::#class => vec![
                            #gp_order
                        ]
                    };

                    gp_orders = quote! {
                        #gp_orders
                        #gp_order,
                    };
                }
            }
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
            #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
            pub enum RegisterClassKind {
                #reg_class_kind
            }
        };

        physreg_reg_class = quote! {
            impl PhysReg {
                pub fn reg_class(&self) -> RegisterClassKind {
                    let n = self.retrieve();
                    #physreg_reg_class
                }
            }
        };

        registers_info_new = quote! {
            impl RegistersInfo {
                pub fn new() -> Self {
                    let mut arena = Arena::new();
                    let mut phys_regs_list = vec![];
                    fn f<T: TargetRegisterTrait>(arena: &mut Arena<RegisterInfo>, r: T) -> RegisterId {
                        let id = arena.alloc(RegisterInfo::new_phys_reg(r));
                        RegisterId {
                            id,
                            kind: VirtOrPhys::Phys(r.as_phys_reg()),
                            fix: None
                        }
                    }
                    #registers_info_new
                    Self {
                        arena: RefCell::new(RegisterArena(arena)),
                        cur_virt_reg: RefCell::new(0),
                        phys_regs_list,
                    }
                }
            }
        };

        physregs_name = quote! {
            impl PhysReg {
                pub fn name(&self) -> &str {
                    let reg_names = [
                        #physregs_name
                    ];
                    reg_names[self.retrieve()]
                }
            }
        };

        str2reg = quote! {
            pub fn str2reg(s: &str) -> Option<PhysReg> {
                Some(match s.to_ascii_lowercase().as_str() {
                    #str2reg
                    _ => return None,
                })
            }
        };

        let trt_physreg = quote! {
            impl TargetRegisterTrait for PhysReg {
                fn as_phys_reg(&self) -> PhysReg {
                    *self
                }

                fn sub_reg(&self) -> Option<PhysReg> {
                    let r = self.as_phys_reg().retrieve();
                    let subs: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
                        #trt_physreg_sub
                    ];
                    subs[r]
                }

                fn super_reg(&self) -> Option<PhysReg> {
                    let r = self.as_phys_reg().retrieve();
                    let supers: [Option<PhysReg>; PHYS_REGISTERS_NUM] = [
                        #trt_physreg_super
                    ];
                    supers[r]
                }

                fn regs_sharing_same_register_file(&self) -> PhysRegSet {
                    if let Some(set) = REG_FILE.with(|f| f.borrow().get(self).map(|s| s.clone())) {
                        return set;
                    }
                    let mut set = PhysRegSet::new();
                    let mut cur = *self;
                    set.set(*self);
                    while let Some(r) = cur.sub_reg() {
                        set.set(r);
                        cur = r;
                    }
                    while let Some(r) = cur.super_reg() {
                        set.set(r);
                        cur = r;
                    }
                    REG_FILE.with(|f| f.borrow_mut().insert(*self, set.clone()));
                    set
                }
            }
        };

        let mut trt_regs = quote! {};
        for (name, SubSuper { sub, super_ }) in &reg_sub_super {
            let sub = sub
                .as_ref()
                .map_or(quote! { None }, |sub| quote! { Some(match self{#sub})});
            let super_ = super_.as_ref().map_or(
                quote! { None },
                |super_| quote! { Some(match self{#super_})},
            );
            let name_ = str2ident(name.as_str());
            trt_regs = quote! {
                #trt_regs
                impl TargetRegisterTrait for #name_ {
                    fn as_phys_reg(&self) -> PhysReg {
                        PhysReg(*self as usize + RegisterClassKind::#name_ as usize)
                    }

                    fn sub_reg(&self) -> Option<PhysReg> {
                        #sub
                    }

                    fn super_reg(&self) -> Option<PhysReg> {
                        #super_
                    }

                    fn regs_sharing_same_register_file(&self) -> PhysRegSet {
                        self.as_phys_reg().regs_sharing_same_register_file()
                    }
                }
            };
        }

        let mut reg_file = quote![];
        for set in &reg_set {
            let mut t = quote! {};
            for (i, s) in set.iter().enumerate() {
                let s = str2ident(s.as_str());
                if i == set.len() - 1 {
                    t = quote! { #t Self::#s };
                } else {
                    t = quote! { #t Self::#s | };
                }
            }

            let r = str2ident(&set[0]);
            t = quote! {
                #t => Self::#r
            };
            reg_file = quote! {
                #reg_file
                #t,
            };
        }

        let impl_reg_class_kind = quote! {
            impl RegisterClassKind {
                pub fn size_in_bits(&self) -> usize {
                    match self {
                        #reg_class_kind_size_in_bits
                    }
                }

                pub fn register_file_base_class(&self) -> Self {
                    match self {
                        #reg_file
                    }
                }

                pub fn return_value_register(&self) -> PhysReg {
                    match self {
                        #ret_val_regs
                    }
                }

                pub fn get_arg_reg_order_vec(&self) -> Vec<PhysReg> {
                    match self {
                        #arg_orders
                    }
                }

                pub fn get_gp_reg_order_vec(&self) -> Vec<PhysReg> {
                    match self {
                        #gp_orders
                    }
                }
            }
        };

        quote! {
            #constants
            #ty2rc
            #rc2ty
            #reg_class_kind
            #physreg_reg_class
            #class_definition
            #impl_reg_class_kind
            #trt_physreg
            #registers_info_new
            #physregs_name
            #trt_regs
            #str2reg
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
            enum_from_primitive! {
            #[derive(Debug, Clone, Copy, Hash, PartialEq)]
            pub enum #name {
                #list
            }
            }
        }
    }
}

// RegisterClass GR32 (i32) {
//    EAX, EBX, .... }
pub fn run(item: TokenStream) -> TokenStream {
    let regs = parse_macro_input!(item as Registers);
    TokenStream::from(regs.construct())
}

fn str2ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}
