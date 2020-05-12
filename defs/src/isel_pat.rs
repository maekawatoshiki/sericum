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
    let output = ISelPatParser::new(&mut reader, &quote! { node }, quote! { node }).parse();
    TokenStream::from(output)
}

pub fn parser_run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());
    let output = PatternParser::new(&mut reader).parse();
    abort!(0, "{:?}", output);
}

type TS = proc_macro2::TokenStream;

#[derive(Debug, Clone)]
enum Node {
    Patterns(Vec<Node>),
    InstPattern(TS, Vec<Node>, TS, Box<Node>), // inst, operands, parent, body
    OperandPattern(String, TS, Box<Node>),     // operand kind, parent, body
    Inst(TS, Vec<Node>),
    // Operand(String),
    Register(String),
    Addressing(String, Vec<Node>),
    Ident(String),
    User(TS),
    None,
}

struct PatternParser<'a> {
    pub reader: &'a mut TokenStreamReader,
}

impl<'a> PatternParser<'a> {
    pub fn new(reader: &'a mut TokenStreamReader) -> Self {
        Self { reader }
    }

    pub fn parse(&mut self) -> Node {
        let body = self.parse_pats(true);
        body
    }

    pub fn parse_pats(&mut self, toplevel: bool) -> Node {
        let mut pats = vec![];
        while self.reader.peak().is_some() {
            pats.push(self.parse_pat(toplevel));
        }
        Node::Patterns(pats)
    }

    pub fn parse_pat(&mut self, toplevel: bool) -> Node {
        if self.reader.cur_is_group('(') {
            return self.parse_inst_pat(toplevel);
        }
        self.parse_operand_pat(toplevel)
    }

    pub fn parse_inst_pat(&mut self, toplevel: bool) -> Node {
        let group = self.reader.get().unwrap();
        let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
        let mut parser = PatternParser::new(&mut reader);
        let inst_kind = parser.parse_inst_kind();
        let operands = parser.parse_inst_operands();
        let parent = if !toplevel {
            let node = ident_tok(self.reader.get_ident().unwrap().as_str());
            quote! { #node }
        } else {
            quote! { node }
        };
        let body = self.parse_body();
        Node::InstPattern(inst_kind, operands, parent, Box::new(body))
    }

    pub fn parse_operand_pat(&mut self, toplevel: bool) -> Node {
        let ty = self.reader.get_ident().unwrap();
        let parent = if !toplevel {
            let node = ident_tok(self.reader.get_ident().unwrap().as_str());
            quote! { #node }
        } else {
            quote! { node }
        };
        let body = self.parse_body();
        Node::OperandPattern(ty, quote! { #parent }, Box::new(body))
    }

    pub fn parse_inst_kind(&mut self) -> TS {
        let ir_or_mi = self.reader.get_ident().unwrap();
        assert!(self.reader.skip_punct('.'));
        let name = ident_tok(self.reader.get_ident().unwrap().as_str());
        let inst = match ir_or_mi.as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => abort!(0, "expected 'ir' or 'mi'"),
        };
        inst
    }

    pub fn parse_inst_operands(&mut self) -> Vec<Node> {
        let mut operands = vec![];
        loop {
            if self.reader.cur_is_group('(') {
                operands.push(self.parse_inst());
            } else {
                operands.push(self.parse_operand())
            }
            if !self.reader.skip_punct(',') {
                break;
            }
        }
        operands
    }

    pub fn parse_inst(&mut self) -> Node {
        let group = self.reader.get().unwrap();
        let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
        let mut parser = PatternParser::new(&mut reader);
        let inst_kind = parser.parse_inst_kind();
        let operands = parser.parse_inst_operands();
        Node::Inst(inst_kind, operands)
    }

    pub fn parse_operand(&mut self) -> Node {
        if self.reader.skip_punct('%') {
            let reg_str = self.reader.get_ident().unwrap();
            Node::Register(reg_str)
        } else if self.reader.cur_is_group('[') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = PatternParser::new(&mut reader);
            let addressing_name = parser.reader.get_ident().unwrap();
            Node::Addressing(addressing_name, parser.parse_inst_operands())
        } else {
            let name = self.reader.get_ident().unwrap();
            match name.as_str() {
                "none" => Node::None,
                _ => Node::Ident(name),
            }
        }
    }

    pub fn parse_body(&mut self) -> Node {
        if self.reader.cur_is_group('{') {
            let group = self.reader.get().unwrap();
            let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
            let mut parser = PatternParser::new(&mut reader);
            parser.parse_pats(false)
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
            let group = self.reader.peak().unwrap();
            let user_code = tok_is_group(&group, '{');
            if user_code {
                let body = proc_macro2::TokenStream::from(group_stream(self.reader.get().unwrap()));
                Node::User(quote! { { #body } })
            } else {
                // let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
                // let mut parser = PatternParser::new(&mut reader);
                self.parse_inst()
            }
        } else {
            abort!(0, "expected ':' or '=>'")
        }
    }
}

struct ISelPatParser<'a> {
    pub reader: &'a mut TokenStreamReader,
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
                    #t
                }
            }
            i += 1;
        }

        if output.is_empty() {
            return output;
        }

        let root = &self.root;
        if depth == 0 {
            quote! {
                (|| -> Raw<DAGNode> {
                    #output

                    #root.operand = #root
                        .operand
                        .iter()
                        .map(|op| self.run_on_node(tys, regs_info, heap, *op))
                        .collect();
                    #root
                }) ()
            }
        } else {
            output
        }
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
            let one = tok_is_group(&group, '(');
            if one {
                let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
                let mut parser = ISelPatParser::new(&mut reader, &self.root, quote! { #node });
                parser.parse_selected_inst_pat()
            } else {
                // for user own code
                let body = proc_macro2::TokenStream::from(group_stream(group));
                quote! {
                    return {
                         #body
                    };
                }
            }
        } else {
            abort!(0, "expected ':' or '=>'")
        };

        match ty.as_str() {
            "imm32" => {
                quote! { if #node.is_constant() && matches!(#node.ty, Type::Int32) { #body } }
            }
            "imm_f64" => {
                quote! { if #node.is_constant() && matches!(#node.ty, Type::F64) {  #body } }
            }
            // TODO
            "mem" => quote! { if #node.is_frame_index() {  #body } },
            "mem32" | "mem64" => {
                let bits = match ty.as_str() {
                    "mem32" => 32usize,
                    "mem64" => 64usize,
                    _ => unimplemented!(),
                };
                quote! {
                    if #node.is_frame_index() && #node.ty.size_in_bits(tys) == #bits {
                          #body
                    }
                }
            }
            "f64mem" => {
                let ty = match ty.as_str() {
                    "f64mem" => quote! { Type::F64 },
                    _ => unimplemented!(),
                };
                quote! {
                    if #node.is_frame_index() && matches!(#node.ty, #ty) {
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

    pub fn parse_selected_inst_arguments_pat(
        &mut self,
    ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        let mut def_operands = quote! {};
        let mut operands = quote! {};

        loop {
            if self.reader.skip_punct('%') {
                let reg_s = self.reader.get_ident().unwrap();
                let reg = ident_tok(reg_s.as_str());
                def_operands = quote! {
                    #def_operands
                    let #reg = heap.alloc_phys_reg(regs_info, str2reg(#reg_s).unwrap());
                };
                operands = quote! {
                    #operands #reg,
                };
            } else if self.reader.cur_is_group('[') {
                let group = self.reader.get().unwrap();
                let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
                let mut parser = ISelPatParser::new(&mut reader, &self.root, self.parent.clone());
                let addressing_name = ident_tok(parser.reader.get_ident().unwrap().as_str());
                let (defo, o) = parser.parse_selected_inst_arguments_pat();
                def_operands = quote! {
                    #def_operands
                    #defo
                    let mem__ = heap.alloc(DAGNode::new_mem( MemNodeKind::#addressing_name, vec![#o])); // TODO: Uniquify mem__
                };
                operands = quote! {
                    #operands mem__,
                };
            } else {
                let name = self.reader.get_ident().unwrap();
                let op = ident_tok(name.as_str());
                match name.as_str() {
                    "none" => {
                        operands = quote! {
                            #operands
                            heap.alloc_none(),
                        }
                    }
                    _ => {
                        def_operands = quote! {
                            #def_operands
                            let #op = self.run_on_node(tys, regs_info, heap, #op);
                        };
                        operands = quote! { #operands #op, };
                    }
                }
            }

            if !self.reader.skip_punct(',') {
                break;
            }
        }

        (def_operands, operands)
    }

    // TODO: refine code asap
    pub fn parse_selected_inst_pat(&mut self) -> proc_macro2::TokenStream {
        let ir_or_mi = self.reader.get_ident().unwrap();
        assert!(self.reader.skip_punct('.'));
        let name = ident_tok(self.reader.get_ident().unwrap().as_str());
        let inst = match ir_or_mi.as_str() {
            "ir" => quote! { NodeKind::IR(IRNodeKind::#name) },
            "mi" => quote! { NodeKind::MI(MINodeKind::#name) },
            _ => abort!(0, "expected 'ir' or 'mi'"),
        };

        let (def_operands, operands) = self.parse_selected_inst_arguments_pat();

        let root = &self.root;
        quote! {
            #def_operands
            return heap.alloc(DAGNode::new(
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
            let one = tok_is_group(&group, '(');
            if one {
                let mut reader = TokenStreamReader::new(group_stream(group).into_iter());
                let mut parser = ISelPatParser::new(&mut reader, &self.root, new_parent.clone());
                parser.parse_selected_inst_pat()
            } else {
                // for user own code
                let body = proc_macro2::TokenStream::from(group_stream(group));
                quote! {
                    { return { #body }; }
                }
            }
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
