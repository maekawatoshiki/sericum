use crate::parser::*;
use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{
        // Delimiter, Group, Punct, Spacing, TokenTree
        Ident,
        Span,
    },
    *,
};
use proc_quote::quote;

pub fn run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());
    let output = ISelPatParser::new(&mut reader, &quote! {node}, quote! { node }).parse();
    TokenStream::from(output)
}

struct ISelPatParser<'a> {
    reader: &'a mut TokenStreamReader,
    root: &'a proc_macro2::TokenStream,
    parent: proc_macro2::TokenStream,
}

impl<'a> ISelPatParser<'a> {
    pub fn new(
        reader: &'a mut TokenStreamReader,
        root: &'a proc_macro2::TokenStream,
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

        if output.is_empty() {
            return output;
        }

        let root = &self.root;
        return quote! {
            #output else {
                #root.operand = #root
                    .operand
                    .iter()
                    .map(|op| self.run_on_node(heap, *op))
                    .collect();
                #root
            }
        };
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
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, &self.root, quote! { #node });
            parser.parse_pats(depth + 1)
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, &self.root, quote! { #node });
            parser.parse_selected_inst_pat()
        } else {
            abort!(0, "expected ':' or '=>'")
        };

        match ty.as_str() {
            "imm32" => quote! {
                if #node.is_constant() && matches!(#node.ty, Type::Int32) { #body }
            },
            "i32mem" | "i64mem" => {
                let bits = match ty.as_str() {
                    "i32mem" => 32usize,
                    "i64mem" => 64usize,
                    _ => unimplemented!(),
                };
                quote! {
                    if #node.is_frame_index() && #node.ty.size_in_bits() == #bits {
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

    pub fn parse_selected_inst_pat(&mut self) -> proc_macro2::TokenStream {
        let ir_or_mi = self.reader.get_ident().unwrap();
        assert!(self.reader.skip_punct('.'));
        let name = ident_tok(self.reader.get_ident().unwrap().as_str());
        let inst = match ir_or_mi.as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => abort!(0, "expected 'ir' or 'mi'"),
        };

        let mut def_operands = quote! {};
        let mut operands = quote! {};

        loop {
            let op = ident_tok(self.reader.get_ident().unwrap().as_str());
            def_operands = quote! {
                #def_operands
                let #op = self.run_on_node(heap, #op);
            };
            operands = quote! { #operands #op, };
            if !self.reader.skip_punct(',') {
                break;
            }
        }

        let root = &self.root;
        quote! {
            #def_operands
            heap.alloc(DAGNode::new(
                #inst,
                vec![#operands],
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

        let mut operand_names = vec![];
        loop {
            operand_names.push(ident_tok(reader.get_ident().unwrap().as_str()));
            if !reader.skip_punct(',') {
                break;
            }
        }

        let new_parent = if depth > 0 {
            let node = ident_tok(self.reader.get_ident().unwrap().as_str());
            quote! { #node }
        } else {
            self.parent.clone()
        };

        let mut operands = quote! {};
        for (i, op_name) in operand_names.iter().enumerate() {
            operands = quote! {
                #operands
                let #op_name = #new_parent.operand[#i];
            };
        }

        let body = if self.reader.cur_is_group('{') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, &self.root, new_parent.clone());
            parser.parse_pats(depth + 1)
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = ISelPatParser::new(&mut reader, &self.root, new_parent.clone());
            parser.parse_selected_inst_pat()
        } else {
            abort!(0, "expected ':' or '=>'")
        };

        quote! {
            if #new_parent.kind == #inst {
                #operands
                #body
            }
        }
    }
}

fn ident_tok(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}
