use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Group, Ident},
    *,
};
use proc_quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, token, Error, Expr, Token};

pub fn run(item: TokenStream) -> TokenStream {
    TokenStream::from(expand(parse_macro_input!(item as Node)))
}

type TS = proc_macro2::TokenStream;

enum Node {
    Inst(InstNode),
    Arg(ArgNode),
}

enum InstNode {
    IR(IRNode),
    MI(MINode),
}

struct IRNode {
    opcode: Ident,
    args: Vec<Node>,
    ty: Option<Expr>,
}

struct MINode {
    opcode: Ident,
    args: Vec<Node>,
}

enum ArgNode {
    User(TS),
    Mem(MemNode),
    Reg(String),
}

struct MemNode {
    kind: Ident,
    args: Vec<Node>,
}

struct Args(Vec<Node>);

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(token::Paren) {
            let group = input.parse::<Group>()?;
            Ok(Node::Inst(syn::parse2(group.stream())?))
        } else {
            Ok(Node::Arg(input.parse::<ArgNode>()?))
        }
    }
}

impl Parse for InstNode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let kind = input.parse::<Ident>()?;
        match kind.to_string().as_str() {
            "IR" => input.parse::<IRNode>().map(|n| InstNode::IR(n)),
            "MI" => input.parse::<MINode>().map(|n| InstNode::MI(n)),
            _ => return Err(Error::new(kind.span(), "expected IR or MI")),
        }
    }
}

impl Parse for IRNode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        assert!(input.parse::<Token![.]>().is_ok());
        let opcode = input.parse::<Ident>()?;
        let ty = if input.parse::<Token![.]>().is_ok() {
            Some(syn::parse2(input.parse::<Group>()?.stream())?)
        } else {
            None
        };
        let args = input.parse::<Args>()?;
        Ok(IRNode {
            opcode,
            args: args.0,
            ty,
        })
    }
}

impl Parse for MINode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        assert!(input.parse::<Token![.]>().is_ok());
        let opcode = input.parse::<Ident>()?;
        let args = input.parse::<Args>()?;
        Ok(MINode {
            opcode,
            args: args.0,
        })
    }
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.is_empty() {
            return Ok(Args(vec![]));
        }

        let mut args = vec![];
        loop {
            args.push(input.parse::<Node>()?);
            if input.parse::<Token![,]>().is_err() {
                break;
            }
        }
        Ok(Args(args))
    }
}

impl Parse for ArgNode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        // Memory operand
        if input.peek(token::Bracket) {
            let group = input.parse::<Group>()?;
            return Ok(Self::Mem(syn::parse2(group.stream())?));
        }

        // Register
        if input.parse::<Token![%]>().is_ok() {
            let name = input.parse::<Ident>()?;
            return Ok(Self::Reg(name.to_string()));
        }

        // Single expression
        if let Ok(expr) = input.parse::<Expr>() {
            return Ok(Self::User(quote! { #expr }));
        }

        todo!()
    }
}

impl Parse for MemNode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let kind = input.parse::<Ident>()?;
        let args = input.parse::<Args>()?;
        Ok(MemNode { kind, args: args.0 })
    }
}

fn expand(node: Node) -> TS {
    match node {
        Node::Inst(inst) => expand_inst(inst),
        Node::Arg(arg) => expand_arg(arg),
    }
}

fn expand_inst(node: InstNode) -> TS {
    match node {
        InstNode::IR(ir) => {
            let mut args = quote! {};
            for arg in ir.args {
                let arg = expand(arg);
                args = quote! { #args #arg , };
            }
            let opcode = ir.opcode;
            let ty = match ir.ty {
                Some(ty) => quote! { .ty(#ty) },
                None => quote! {},
            };
            quote! {{
                let node = IRNode::new(IROpcode::#opcode).args(vec![#args]) #ty;
                c.arena.alloc(node.into())
            }}
        }
        InstNode::MI(mi) => {
            let mut args = quote! {};
            for arg in mi.args {
                let arg = expand(arg);
                args = quote! { #args #arg , };
            }
            let opcode = mi.opcode;
            quote! {{
                let mut node = MINode::new(MO::#opcode).args(vec![#args]);
                if MO::#opcode.inst_def().unwrap().defs.len() > 0 {
                    node = node.reg_class(MO::#opcode.inst_def().unwrap().defs[0].as_reg_class());
                }
                c.arena.alloc(node.into())
            }}
        }
    }
}

fn expand_arg(node: ArgNode) -> TS {
    match node {
        ArgNode::User(u) => {
            quote! { #u }
        }
        ArgNode::Mem(MemNode { kind, args: args_ }) => {
            let mut args = quote! {};
            let one_arg = args_.len() == 1;
            for arg in args_ {
                let arg = expand(arg);
                args = quote! {#args #arg , };
            }
            let node = if one_arg {
                quote! { let node = MemKind::#kind(#args); }
            } else {
                quote! { let node = MemKind::#kind([#args]); }
            };
            quote! {{
                #node
                c.arena.alloc(node.into())
            }}
        }
        ArgNode::Reg(name) => {
            quote! {{
                c.arena.alloc(c.regs.get_phys_reg(str2reg(#name).unwrap()).into())
            }}
        }
    }
}
