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
    let pats = PatternParser::new(&mut reader).parse();
    TokenStream::from(TokenStreamConstructor::new().run(pats))
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

struct TokenStreamConstructor {}

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
                self.parse_inst()
            }
        } else {
            abort!(0, "expected ':' or '=>'")
        }
    }
}

impl TokenStreamConstructor {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, node: Node) -> TS {
        let output = self.run_sub(node);
        quote! {
            (|| -> Raw<DAGNode> {
                #output

                node.operand = node
                    .operand
                    .iter()
                    .map(|op| self.run_on_node(tys, regs_info, heap, *op))
                    .collect();
                node
            }) ()
        }
    }

    pub fn run_sub(&mut self, node: Node) -> TS {
        match node {
            Node::Patterns(pats) => self.run_on_patterns(pats),
            Node::InstPattern(inst, operands, parent, body) => {
                self.run_on_inst_patterns(inst, operands, parent, body)
            }
            Node::OperandPattern(kind, parent, body) => {
                self.run_on_operand_pattern(kind, parent, body)
            }
            Node::Inst(inst, operands) => self.run_on_inst(inst, operands),
            Node::Register(_name) => unimplemented!(),
            Node::Addressing(_name, _operands) => unimplemented!(),
            Node::Ident(_name) => unimplemented!(),
            Node::User(code) => quote! { return #code },
            Node::None => unimplemented!(),
        }
    }

    fn run_on_patterns(&mut self, pats: Vec<Node>) -> TS {
        let mut output = quote! {};
        for pat in pats {
            let ts = self.run_sub(pat);
            output = quote! {
                #output
                #ts
            };
        }
        output
    }

    fn run_on_inst_patterns(
        &mut self,
        inst: TS,
        operands: Vec<Node>,
        parent: TS,
        body: Box<Node>,
    ) -> TS {
        let body = self.run_sub(*body);
        let mut def_operands = quote! {};
        for (i, operand) in operands.into_iter().enumerate() {
            match operand {
                Node::Ident(name) => {
                    let name = ident_tok(name.as_str());
                    def_operands = quote! {
                        #def_operands
                        let #name = #parent.operand[#i];
                    }
                }
                _ => unimplemented!(),
            }
        }
        quote! {
            if #parent.kind == #inst {
                #def_operands
                #body
            }
        }
    }

    fn run_on_operand_pattern(&mut self, kind: String, parent: TS, body: Box<Node>) -> TS {
        let body = self.run_sub(*body);
        match kind.as_str() {
            "imm8" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::Int8) { #body } }
            }
            "imm32" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::Int32) { #body } }
            }
            "imm_f64" => {
                quote! { if #parent.is_constant() && matches!(#parent.ty, Type::F64) {  #body } }
            }
            // TODO
            "mem" => quote! { if #parent.is_frame_index() {  #body } },
            "mem32" | "mem64" => {
                let bits = match kind.as_str() {
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
                let ty = match kind.as_str() {
                    "f64mem" => quote! { Type::F64 },
                    _ => unimplemented!(),
                };
                quote! {
                    if #parent.is_frame_index() && matches!(#parent.ty, #ty) {
                          #body
                    }
                }
            }
            reg_class => {
                let reg_class = ident_tok(reg_class);
                quote! {
                    if #parent.is_maybe_register()
                      && matches!(ty2rc(&#parent.ty), Some(RegisterClassKind::#reg_class)) {
                            #body
                    }
                }
            }
        }
    }

    fn run_on_inst(&mut self, inst: TS, operands: Vec<Node>) -> TS {
        let (defs, ops) = self.selected_operands(operands);
        quote! {
            #defs
            return heap.alloc(DAGNode::new(
                    #inst,
                    vec![#ops],
                    node.ty));
        }
    }

    // def, operands
    fn selected_operands(&mut self, operands: Vec<Node>) -> (TS, TS) {
        let mut def_operands_ts = quote! {};
        let mut operands_ts = quote! {};
        for operand in operands {
            match operand {
                Node::Ident(name) => {
                    let name = ident_tok(name.as_str());
                    def_operands_ts = quote! {
                        #def_operands_ts
                        let #name = self.run_on_node(tys, regs_info, heap, #name);
                    };
                    operands_ts = quote! { #operands_ts #name, };
                }
                Node::Register(name) => {
                    let name_s = name.as_str();
                    let name = ident_tok(name.as_str());
                    def_operands_ts = quote! {
                        #def_operands_ts
                        let #name = heap.alloc_phys_reg(regs_info, str2reg(#name_s).unwrap());
                    };
                    operands_ts = quote! {
                        #operands_ts #name,
                    };
                }
                Node::Addressing(name, operands) => {
                    let name = ident_tok(name.as_str());
                    let (defs, ops) = self.selected_operands(operands);
                    let mem = ident_tok(&format!("mem_{}", unique_name()));
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
                Node::Inst(inst, operands) => {
                    let (defs, ops) = self.selected_operands(operands);
                    let iname = ident_tok(&format!("inst_{}", unique_name()));
                    def_operands_ts = quote! {
                        #def_operands_ts
                        #defs
                        let #iname = heap.alloc(DAGNode::new(
                            #inst,
                            vec![#ops],
                            #inst.as_mi().get_def_type()));
                    };
                    operands_ts = quote! {
                        #operands_ts #iname,
                    }
                }
                Node::None => {
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
}

fn ident_tok(s: &str) -> Ident {
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
