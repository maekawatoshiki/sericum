use proc_macro::{token_stream::IntoIter, Delimiter, TokenStream, TokenTree};
// use proc_quote::quote;
use std::collections::VecDeque;

macro_rules! retrieve {
    ($e:expr) => {
        match $e {
            Some(x) => x,
            None => return false,
        }
    };
}

pub struct TokenStreamReader {
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

    pub fn cur_is_punct(&mut self, p: char) -> bool {
        let tok = retrieve!(self.peak());
        tok_is_punct(tok, p)
    }

    pub fn cur_is_group(&mut self, p: char) -> bool {
        let tok = retrieve!(self.peak());
        tok_is_group(tok, p)
    }

    pub fn skip_ident(&mut self, s: &str) -> bool {
        let skip = self.cur_is_ident(s);
        if skip {
            self.get();
        }
        skip
    }

    pub fn skip_punct(&mut self, p: char) -> bool {
        let skip = self.cur_is_punct(p);
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

    pub fn get_literal(&mut self) -> Option<String> {
        let may_ident = self.get();
        match may_ident {
            Some(TokenTree::Literal(i)) => i.span().source_text(),
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

fn tok_is_punct(tok: &TokenTree, punct: char) -> bool {
    match tok {
        TokenTree::Punct(p) => p.as_char() == punct,
        _ => false,
    }
}

pub fn tok_is_group(tok: &TokenTree, d: char) -> bool {
    let d = match d {
        '(' => Delimiter::Parenthesis,
        '{' => Delimiter::Brace,
        '[' => Delimiter::Bracket,
        _ => Delimiter::None,
    };
    match tok {
        TokenTree::Group(g) => g.delimiter() == d,
        _ => false,
    }
}

pub fn group_stream(tok: TokenTree) -> TokenStream {
    match tok {
        TokenTree::Group(g) => g.stream(),
        _ => panic!(),
    }
}
