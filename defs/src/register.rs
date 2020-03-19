use proc_macro::{token_stream::IntoIter, TokenStream, TokenTree};
use proc_quote::quote;
use std::collections::VecDeque;

macro_rules! retrieve {
    ($e:expr) => {
        match $e {
            Some(x) => x,
            None => return false,
        }
    };
}

// RegisterClass GR32 (i32) {
//    EAX, EBX, .... }

struct TokenStreamReader {
    stream: IntoIter,
    buf: VecDeque<TokenTree>,
}

impl TokenStreamReader {
    pub fn new(stream: IntoIter) -> Self {
        Self {
            stream,
            buf: VecDeque::new(),
        }
    }

    pub fn cur_is_ident(&mut self, s: &str) -> bool {
        let tok = retrieve!(self.peak());
        tok_is_ident(tok, s)
    }

    pub fn skip_if_cur_is_ident(&mut self, s: &str) -> bool {
        let skip = self.cur_is_ident(s);
        if skip {
            self.get();
        }
        skip
    }

    pub fn get_ident(&mut self) -> Option<String> {
        let may_ident = self.get();
        match may_ident {
            Some(TokenTree::Ident(i)) => i.span().source_text(),
            _ => None,
        }
    }

    pub fn get(&mut self) -> Option<TokenTree> {
        if self.buf.len() > 0 {
            return self.buf.pop_front();
        }
        self.stream.next()
    }

    pub fn peak(&mut self) -> Option<&TokenTree> {
        if self.buf.len() > 0 {
            return self.buf.front();
        }
        let tok = self.get()?;
        self.unget(tok);
        self.peak()
    }

    pub fn unget(&mut self, tree: TokenTree) {
        self.buf.push_back(tree)
    }
}

fn tok_is_ident(tok: &TokenTree, ident: &str) -> bool {
    match tok {
        TokenTree::Ident(i) => match i.span().source_text() {
            Some(s) => s == ident,
            None => false,
        },
        _ => false,
    }
}

pub fn run(item: TokenStream) -> TokenStream {
    let mut reader = TokenStreamReader::new(item.into_iter());

    while reader.skip_if_cur_is_ident("RegisterClass") {
        let _reg_class = reader.get_ident().unwrap();
        // panic!("reg class name: {}", reg_class);
    }

    let expanded = quote! {

    #[derive(Debug, Clone, Copy, Hash, PartialEq)]
    pub enum GR32 {
        EAX,
        EBX,
        ECX,
        EDX,
        ESI,
        EDI,
        EBP,
        ESP,
        R8D,
        R9D,
        R10D,
        R11D,
        R12D,
        R13D,
        R14D,
        R15D,
    }

    };

    TokenStream::from(expanded)
}
