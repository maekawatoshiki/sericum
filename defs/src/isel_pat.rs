use crate::parser::*;
use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenTree},
    *,
};
use proc_quote::{quote, TokenStreamExt};

pub fn run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());
    let mut parser = ISelPatParser::new(&mut reader, quote! {node}, quote! { node });

    let output = parser.parse();

    TokenStream::from(output)
}

struct ISelPatParser<'a> {
    reader: &'a mut TokenStreamReader,
    root: proc_macro2::TokenStream,
    parent: proc_macro2::TokenStream,
}

impl<'a> ISelPatParser<'a> {
    // (ir.Add a, b) {
    //     GR32 a {
    //         GR32  b => (mi.MOVrr32 a b)
    //         imm32 b => () }
    //     GR64 a {
    //         GR32  b => () } }
    pub fn new(
        reader: &'a mut TokenStreamReader,
        root: proc_macro2::TokenStream,
        parent: proc_macro2::TokenStream,
    ) -> Self {
        Self {
            reader,
            parent,
            root,
        }
    }

    pub fn parse(&mut self) -> proc_macro2::TokenStream {
        let body = self.parse_pats(0);
        // let p = &self.parent;
        body
    }

    pub fn parse_pats(&mut self, depth: usize) -> proc_macro2::TokenStream {
        let mut output = quote! {};
        let mut i = 0;
        while self.reader.peak().is_some() {
            let t = self.parse_pat(depth);
            if i == 0 {
                output = quote! {
                    #output
                    #t
                }
            } else {
                output = quote! {
                    #output
                    else #t
                }
            }
            i += 1;
        }
        if !output.is_empty() {
            output = quote! {
                #output else { unimplemented!() }
            };
        }
        output
    }

    // e.g. (ir.INST a b)
    pub fn parse_pat(&mut self, depth: usize) -> proc_macro2::TokenStream {
        if self.reader.cur_is_group('(') {
            return self.parse_inst_pat(depth);
        }

        self.parse_op_pat(depth)
    }

    pub fn parse_op_pat(&mut self, depth: usize) -> proc_macro2::TokenStream {
        let ty = self.reader.get_ident().unwrap();
        let node = ident_tok(self.reader.get_ident().unwrap().as_str());

        let body = if self.reader.cur_is_group('{') {
            let group = self.reader.get().unwrap();
            // quote! {}
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, self.root.clone(), quote! { #node });
            parser.parse_pats(depth + 1)
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, self.root.clone(), quote! { #node });
            parser.parse_selected_inst_pat(depth + 1)
        } else {
            abort!(0, "expected ':' or '=>'")
        };

        match ty.as_str() {
            "imm32" => {
                quote! {
                    if #node.is_constant() && matches!(#node.ty, Type::Int32) {
                        #body
                    }
                }
            }
            r => {
                // register class
                let r = ident_tok(r);
                quote! {
                    if #node.is_maybe_register()
                      && matches!(ty2rc(&#node.ty), Some(RegisterClassKind::#r)) {
                          #body
                      }
                }
            }
        }
    }

    pub fn parse_selected_inst_pat(&mut self, depth: usize) -> proc_macro2::TokenStream {
        let ir_or_mi = self.reader.get_ident().unwrap();
        assert!(self.reader.skip_punct('.'));
        let name = ident_tok(self.reader.get_ident().unwrap().as_str());
        let inst = match ir_or_mi.as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => abort!(0, "expected 'ir' or 'mi'"),
        };

        let mut operands = quote! {};
        let mut ope2 = quote! {};

        let mut i = 0usize;
        loop {
            let op = ident_tok(self.reader.get_ident().unwrap().as_str());
            let p = &self.parent;
            operands = quote! {
                #operands
                let #op = self.run_on_node(heap, #op);
            };
            ope2 = quote! { #ope2 #op, };
            if !self.reader.skip_punct(',') {
                break;
            }
            i += 1
        }
        let root = &self.root;
        quote! {
            #operands
            heap.alloc(DAGNode::new(
                #inst,
                vec![#ope2],
                #root.ty.clone()
            ))
        }
    }

    pub fn parse_inst_pat(&mut self, depth: usize) -> proc_macro2::TokenStream {
        let group = self.reader.get().unwrap();
        let mut reader = TokenStreamReader::new(group_stream(group).into_iter());

        let ir_or_mi = reader.get_ident().unwrap();
        assert!(reader.skip_punct('.'));
        let name = ident_tok(reader.get_ident().unwrap().as_str());
        let inst = match ir_or_mi.as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => abort!(0, "expected 'ir' or 'mi'"),
        };

        let mut operands = quote! {};

        let mut i = 0usize;
        loop {
            let op_name = ident_tok(reader.get_ident().unwrap().as_str());
            let p = &self.parent;
            operands = quote! {
                #operands
                let #op_name = #p.operand[#i];
            };
            if !reader.skip_punct(',') {
                break;
            }
            i += 1
        }

        let new_parent = if depth > 0 {
            let node = ident_tok(self.reader.get_ident().unwrap().as_str());
            quote! { #node }
        } else {
            self.parent.clone()
        };

        let body = if self.reader.cur_is_group('{') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, self.root.clone(), new_parent);
            parser.parse_pats(depth + 1)
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
            unimplemented!()
        } else {
            abort!(0, "expected ':' or '=>'")
        };

        let p = &self.parent;
        quote! {
            if #p.kind == #inst {
                #operands
                #body
            }
        }
    }
}

fn ident_tok(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}
