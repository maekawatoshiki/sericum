use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Group, Ident, Span},
    *,
};
use proc_quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, token, Error, LitInt, Token};

pub fn run(item: TokenStream) -> TokenStream {
    let patterns = parse_macro_input!(item as Patterns);
    let ts = construct(patterns);
    TokenStream::from(ts)
}

type TS = proc_macro2::TokenStream;
type Pattern = Box<dyn PatternMatchConstructible>;

struct Patterns(pub Vec<Pattern>);

enum Selected {
    Inst(Inst),
    Register(String),
    Immediate(i32),
    Addressing(Addressing),
    Ident(Ident),
    User(TS),
    None,
}

struct InstPattern {
    opcode: Opcode,
    operands: Vec<Selected>,
    ty: Option<TS>,
    parent: TS,
    body: Pattern,
}

struct OperandPattern {
    name: String,
    ty: Option<TS>,
    parent: TS,
    body: Pattern,
}

struct Addressing(pub Ident, pub Vec<Selected>);

struct Opcode(pub TS);

struct Inst {
    opcode: Opcode,
    operands: Vec<Selected>,
}

fn parse_pattern(input: ParseStream) -> Result<Pattern, Error> {
    if input.parse::<Token![=>]>().is_ok() {
        return Ok(Box::new(input.parse::<Selected>()?));
    }

    if input.peek(token::Paren) {
        // (
        return Ok(Box::new(input.parse::<InstPattern>()?));
    }

    if let Ok(group) = input.parse::<Group>() {
        // {
        let patterns: Patterns = syn::parse2(group.stream())?;
        return Ok(Box::new(patterns));
    }

    Ok(Box::new(input.parse::<OperandPattern>()?))
}

impl Parse for Patterns {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut patterns = vec![];
        while !input.is_empty() {
            patterns.push(parse_pattern(input)?);
        }
        Ok(Patterns(patterns))
    }
}

impl Parse for InstPattern {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let group = input.parse::<Group>()?;
        let inst: Inst = syn::parse2(group.stream())?;
        let ty = parse_type(input)?;
        let parent = parse_parent(input)?;
        let body = parse_pattern(input)?;
        Ok(InstPattern {
            opcode: inst.opcode,
            operands: inst.operands,
            ty,
            parent,
            body,
        })
    }
}

impl Parse for OperandPattern {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let name = input.parse::<Ident>()?.to_string();
        let ty = parse_type(input)?;
        let parent = parse_parent(input)?;
        let body = parse_pattern(input)?;
        Ok(OperandPattern {
            name,
            ty,
            parent,
            body,
        })
    }
}

impl Parse for Opcode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let ir_or_mi: Ident = input.parse()?;
        assert!(input.parse::<Token![.]>().is_ok());
        let name: Ident = input.parse()?;
        let opcode = match ir_or_mi.to_string().as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => return Err(Error::new(ir_or_mi.span(), "expected 'ir' or 'mi'")),
        };
        Ok(Opcode(opcode))
    }
}

impl Parse for Selected {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(token::Paren) {
            let group = input.parse::<Group>()?;
            return Ok(Selected::Inst(syn::parse2(group.stream())?));
        }

        if let Ok(_) = input.parse::<Token![%]>() {
            let reg_str = input.parse::<Ident>()?.to_string();
            return Ok(Selected::Register(reg_str));
        }

        if let Ok(_) = input.parse::<Token![$]>() {
            let imm = input.parse::<LitInt>()?.to_string().parse::<i32>().unwrap();
            return Ok(Selected::Immediate(imm));
        }

        if input.peek(token::Brace) {
            let group = input.parse::<Group>()?;
            return Ok(Selected::User(group.stream()));
        }

        if let Ok(group) = input.parse::<Group>() {
            let addressing: Addressing = syn::parse2(group.stream())?;
            return Ok(Selected::Addressing(addressing));
        }

        let name = input.parse::<Ident>()?;
        Ok(match name.to_string().as_str() {
            "none" => Selected::None,
            _ => Selected::Ident(name),
        })
    }
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let opcode: Opcode = input.parse()?;
        let operands = parse_inst_operands(input)?;
        Ok(Inst { opcode, operands })
    }
}

impl Parse for Addressing {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let name = input.parse::<Ident>()?;
        let operands = parse_inst_operands(input)?;
        Ok(Addressing(name, operands))
    }
}

fn parse_inst_operands(input: ParseStream) -> Result<Vec<Selected>, Error> {
    let mut operands = vec![];
    loop {
        operands.push(input.parse::<Selected>()?);
        if input.parse::<Token![,]>().is_err() {
            break;
        }
    }
    Ok(operands)
}

fn parse_type(input: ParseStream) -> Result<Option<TS>, Error> {
    if !input.peek(Token![:]) {
        return Ok(None);
    }
    assert!(input.parse::<Token![:]>().is_ok());
    let ty: Ident = input.parse()?;
    Ok(Some(if input.peek(Token![!]) {
        assert!(input.parse::<Token![!]>().is_ok());
        quote! { Type::#ty(_) }
    } else {
        quote! { Type::#ty }
    }))
}

fn parse_parent(input: ParseStream) -> Result<TS, Error> {
    Ok(match input.parse::<Ident>() {
        Ok(parent) => quote! { #parent },
        Err(_) => quote! { node },
    })
}

//// construct pattern matching code for instruction selection

fn construct(pats: Patterns) -> TS {
    let t = pats.construct();
    quote! {
        (|| -> Raw<DAGNode> {
            #t
            node.operand = node
                .operand
                .iter()
                .map(|op| self.run_on_node(tys, regs_info, heap, *op))
                .collect();
            node
        }) ()
    }
}

trait PatternMatchConstructible {
    fn construct(&self) -> TS;
}

impl PatternMatchConstructible for Patterns {
    fn construct(&self) -> TS {
        let mut output = quote! {};
        for pat in &self.0 {
            let ts = pat.construct();
            output = quote! {
                #output
                #ts
            };
        }
        output
    }
}

impl PatternMatchConstructible for InstPattern {
    fn construct(&self) -> TS {
        let body = self.body.construct();
        let mut def_operands = quote! {};
        let inst = &self.opcode.0;
        let parent = &self.parent;
        for (i, operand) in self.operands.iter().enumerate() {
            match operand {
                Selected::Ident(name) => {
                    def_operands = quote! {
                        #def_operands
                        let #name = #parent.operand[#i];
                    }
                }
                _ => unimplemented!(),
            }
        }
        if let Some(ty) = &self.ty {
            quote! { if #parent.kind == #inst && matches!(#parent.ty, #ty) {
                #def_operands
                #body
            }}
        } else {
            quote! { if #parent.kind == #inst {
                #def_operands
                #body
            }}
        }
    }
}

impl PatternMatchConstructible for OperandPattern {
    fn construct(&self) -> TS {
        let body = self.body.construct();
        let parent = &self.parent;
        match self.name.as_str() {
            "imm8" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::i8) { #body } }
            }
            "imm32" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::i32) { #body } }
            }
            "imm_f64" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::F64) {  #body } }
            }
            _ if self.name.as_str().starts_with("imm") => {
                let bit = self.name.as_str()["imm".len()..].parse::<u32>().unwrap();
                quote! { if #parent.is_constant()
                && #parent.ty.is_integer()
                && #parent.as_constant().bits_within(#bit).unwrap() { #body } }
            }
            // TODO
            "mem" => quote! { if #parent.is_frame_index() {  #body } },
            "mem32" | "mem64" => {
                let bits = match self.name.as_str() {
                    "mem32" => 32usize,
                    "mem64" => 64usize,
                    _ => unimplemented!(),
                };
                quote! {
                    if #parent.is_frame_index() && #parent.ty.size_in_bits(tys) == #bits {
                          #body
                    }
                }
            }
            "f64mem" => {
                let ty = match self.name.as_str() {
                    "f64mem" => quote! { Type::F64 },
                    _ => unimplemented!(),
                };
                quote! {
                    if #parent.is_frame_index() && matches!(#parent.ty, #ty) {
                          #body
                    }
                }
            }
            "addr" => {
                quote! {
                    if #parent.is_address() {
                        #body
                    }
                }
            }

            reg_class => {
                let reg_class = str2ident(reg_class);
                if let Some(ty) = &self.ty {
                    quote! { if #parent.is_maybe_register()
                        && matches!(ty2rc(&#parent.ty), Some(RegisterClassKind::#reg_class))
                        && matches!(#parent.ty, #ty) {
                                #body
                    } }
                } else {
                    quote! { if #parent.is_maybe_register()
                        && matches!(ty2rc(&#parent.ty), Some(RegisterClassKind::#reg_class)) {
                                #body
                    }}
                }
            }
        }
    }
}

impl PatternMatchConstructible for Selected {
    fn construct(&self) -> TS {
        match self {
            Self::Inst(x) => x.construct(),
            Self::User(code) => quote! { return #code },
            _ => unimplemented!(),
        }
    }
}

impl PatternMatchConstructible for Inst {
    fn construct(&self) -> TS {
        let opcode = &self.opcode.0;
        let (defs, ops) = selected_operands(&self.operands);
        quote! {
            #defs
            return heap.alloc(DAGNode::new(
                    #opcode,
                    vec![#ops],
                    node.ty));
        }
    }
}

fn selected_operands(operands: &Vec<Selected>) -> (TS, TS) {
    let mut def_operands_ts = quote! {};
    let mut operands_ts = quote! {};
    for operand in operands {
        match operand {
            Selected::Ident(name) => {
                def_operands_ts = quote! {
                    #def_operands_ts
                    let #name = self.run_on_node(tys, regs_info, heap, #name);
                };
                operands_ts = quote! { #operands_ts #name, };
            }
            Selected::Register(name) => {
                let name_s = name.as_str();
                let name = str2ident(name.as_str());
                def_operands_ts = quote! {
                    #def_operands_ts
                    let #name = heap.alloc_phys_reg(regs_info, str2reg(#name_s).unwrap());
                };
                operands_ts = quote! {
                    #operands_ts #name,
                };
            }
            Selected::Immediate(i) => {
                let imm = str2ident(&format!("imm_{}", unique_name()));
                def_operands_ts = quote! {
                    #def_operands_ts
                    let #imm = heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::Constant(
                                    ConstantKind::Int32(#i))),
                            vec![],
                            Type::i32
                    ));
                };
                operands_ts = quote! {
                    #operands_ts #imm,
                }
            }
            Selected::Addressing(addressing) => {
                let name = &addressing.0;
                let (defs, ops) = selected_operands(&addressing.1);
                let mem = str2ident(&format!("mem_{}", unique_name()));
                def_operands_ts = quote! {
                    #def_operands_ts
                    #defs
                    let #mem = heap.alloc(DAGNode::new_mem(
                            MemNodeKind::#name, vec![#ops])); // TODO: Uniquify mem__
                };
                operands_ts = quote! {
                    #operands_ts #mem,
                }
            }
            Selected::Inst(inst) => {
                let (defs, ops) = selected_operands(&inst.operands);
                let opcode = &inst.opcode.0;
                let iname = str2ident(&format!("inst_{}", unique_name()));
                def_operands_ts = quote! {
                    #def_operands_ts
                    #defs
                    let #iname = heap.alloc(DAGNode::new(
                        #opcode,
                        vec![#ops],
                        #opcode.as_mi().get_def_type()));
                };
                operands_ts = quote! {
                    #operands_ts #iname,
                }
            }
            Selected::None => {
                operands_ts = quote! {
                    #operands_ts
                    heap.alloc_none(),
                };
            }
            _ => unimplemented!(),
        }
    }
    (def_operands_ts, operands_ts)
}

fn str2ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}

fn unique_name() -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                             abcdefghijklmnopqrstuvwxyz\
                             0123456789";
    const LEN: usize = 16;
    use rand::Rng;
    let mut rng = rand::thread_rng();
    (0..LEN)
        .map(|_| {
            let idx = rng.gen_range(0, CHARSET.len());
            CHARSET[idx] as char
        })
        .collect()
}
