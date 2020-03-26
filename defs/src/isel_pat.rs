use crate::parser::*;
use proc_macro::TokenStream;
use proc_macro_error::{
    proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenTree},
    *,
};
use proc_quote::{quote, TokenStreamExt};

pub fn run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());
    let mut parser = ISelPatParser::new(&mut reader, quote! { node });

    let output = parser.parse();

    TokenStream::from(output)
}

struct ISelPatParser<'a> {
    reader: &'a mut TokenStreamReader,
    parent_node: proc_macro2::TokenStream,
}

impl<'a> ISelPatParser<'a> {
    // (ir.Add a, b):
    //     GR32 a:
    //         GR32  b => ()
    //         imm32 b => ()
    //     GR64 a:
    //         GR32 b => ()
    pub fn new(reader: &'a mut TokenStreamReader, parent_node: proc_macro2::TokenStream) -> Self {
        Self {
            reader,
            parent_node,
        }
    }

    pub fn parse(&mut self) -> proc_macro2::TokenStream {
        let body = self.parse_pat();
        let p = &self.parent_node;
        quote! {
            match #p.kind {
                #body
                _ => unimplemented!()
            }
        }
    }

    // e.g. (ir.INST a b)
    pub fn parse_pat(&mut self) -> proc_macro2::TokenStream {
        if !self.reader.cur_is_group('(') {
            return quote! {};
        }

        self.parse_inst_pat()
    }

    pub fn parse_inst_pat(&mut self) -> proc_macro2::TokenStream {
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
            let p = &self.parent_node;
            operands = quote! {
                #operands
                let #op_name = #p.operand[#i];
            };
            if !reader.skip_punct(',') {
                break;
            }
            i += 1
        }

        if self.reader.skip_punct(':') {
        } else if self.reader.skip_punct('=') && self.reader.skip_punct('>') {
        } else {
            abort!(0, "expected ':' or '=>'")
        }

        quote! {
            #inst => {
                #operands
            }
        }
    }
}

fn ident_tok(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}
