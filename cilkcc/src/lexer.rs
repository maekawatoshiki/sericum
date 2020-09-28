use super::token::{Keyword, SourceLoc, Symbol, Token, TokenKind};
use id_arena::{Arena, Id};
use rustc_hash::FxHashMap;
use std::cell::{Ref, RefCell};
use std::collections::VecDeque;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

pub type PathArena = Rc<RefCell<Arena<PathBuf>>>;

pub struct Lexer {
    path_arena: PathArena,
    sub_lexers: VecDeque<SubLexer>,
    macros: FxHashMap<String, Macro>,
    cond_stack: VecDeque<bool>,
}

pub struct SubLexer {
    path_arena: PathArena,
    pub path: Id<PathBuf>,
    buf: VecDeque<Token>,
    source: String,
    loc: SourceLoc,
}

pub type Result<T> = result::Result<T, LexerError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LexerError {
    Message(SourceLoc, String),
    EOF,
}

#[derive(Debug, Clone)]
pub enum Macro {
    Object(Vec<Token>),
    FuncLike(Vec<Token>),
}

macro_rules! retrieve_ident {
    ($e:expr) => {
        match &$e.kind {
            &TokenKind::Identifier(ref ident) => ident.to_string(),
            _ => "".to_string(),
        }
    };
}

macro_rules! retrieve_str {
    ($e:expr) => {
        match &$e.kind {
            &TokenKind::String(ref s) => s.to_string(),
            _ => panic!(),
        }
    };
}

fn retrieve_ident(tok: Token) -> Result<(SourceLoc, String)> {
    match tok.kind {
        TokenKind::Identifier(ident) => Ok((tok.loc, ident)),
        _ => return Err(LexerError::msg(tok.loc, "expected identifier")),
    }
}

impl Lexer {
    pub fn new(path: PathBuf) -> Self {
        let path_arena = PathArena::new(RefCell::new(Arena::new()));
        Self {
            sub_lexers: {
                let mut sub_lexers = VecDeque::new();
                sub_lexers.push_back(SubLexer::new(path_arena.clone(), path));
                sub_lexers
            },
            path_arena,
            macros: FxHashMap::default(),
            cond_stack: VecDeque::new(),
        }
    }

    pub fn add_lexer(&mut self, path: PathBuf) {
        self.sub_lexers
            .push_back(SubLexer::new(self.path_arena.clone(), path))
    }

    // reading char

    pub fn get_char(&mut self) -> Result<char> {
        self.sub_lexers.back_mut().unwrap().get_char()
    }

    pub fn next_char(&mut self) -> Result<char> {
        self.sub_lexers.back_mut().unwrap().next_char()
    }

    // reading token

    pub fn get_token(&mut self) -> Result<Token> {
        self.do_get_token().and_then(|tok| {
            if matches!(tok.kind, TokenKind::String(_))
                && matches!(self.peek_token()?.kind, TokenKind::String(_))
            {
                let s1 = retrieve_str!(tok);
                let s2 = retrieve_str!(self.get_token()?);
                let mut new_tok = tok;
                let mut concat_str = s1;
                concat_str.push_str(s2.as_str());
                new_tok.kind = TokenKind::String(concat_str);
                Ok(new_tok)
            } else {
                Ok(maybe_convert_to_keyword(tok))
            }
        })
    }

    pub fn peek_token(&mut self) -> Result<Token> {
        self.get_token().and_then(|tok| {
            let tok = maybe_convert_to_keyword(tok);
            self.unget(tok.clone());
            Ok(tok)
        })
    }

    pub fn do_get_token(&mut self) -> Result<Token> {
        let tok = self.read_token().and_then(|tok| match &tok.kind {
            &TokenKind::Symbol(Symbol::Hash) => {
                self.read_cpp_directive()?;
                self.do_get_token()
            }
            _ => Ok(tok),
        });
        tok
        // self.expand(tok)
    }

    pub fn unget(&mut self, t: Token) {
        self.sub_lexers.back_mut().unwrap().unget(t)
    }

    pub fn unget_all(&mut self, ts: Vec<Token>) {
        self.sub_lexers.back_mut().unwrap().unget_all(ts)
    }

    pub fn do_read_token(&mut self) -> Result<Token> {
        if self.sub_lexers.len() == 0 {
            return Err(LexerError::EOF);
        }

        match self.sub_lexers.back_mut().unwrap().do_read_token() {
            Ok(token) => Ok(token),
            Err(LexerError::EOF) => {
                self.sub_lexers.pop_back().unwrap();
                if self.sub_lexers.len() == 0 {
                    return Err(LexerError::EOF);
                }
                return self.do_read_token();
            }
            Err(e) => return Err(e),
        }
    }

    pub fn read_token(&mut self) -> Result<Token> {
        let token = self.do_read_token();
        token.and_then(|tok| match tok.kind {
            TokenKind::Newline => self.read_token(),
            TokenKind::Identifier(_) => Ok(convert_to_symbol(tok)),
            _ => Ok(tok),
        })
    }

    pub fn skip_symbol(&mut self, sym: Symbol) -> bool {
        if let Ok(tok) = self.get_token() {
            match tok.kind {
                TokenKind::Symbol(s) if s == sym => return true,
                _ => self.unget(tok),
            }
        }
        false
    }

    pub fn expect_skip_symbol(&mut self, sym: Symbol) -> Result<()> {
        if !self.skip_symbol(sym) {
            return Err(LexerError::Message(
                self.loc(),
                format!("expected '{:?}'", sym),
            ));
        }
        Ok(())
    }

    fn try_read_ident(&mut self) -> Result<(SourceLoc, String)> {
        let tok = self.do_read_token()?;
        match tok.kind {
            TokenKind::Identifier(ident) => Ok((tok.loc, ident)),
            _ => return Err(LexerError::msg(tok.loc, "expected identifier")),
        }
    }

    // preprocessor

    fn read_cpp_directive(&mut self) -> Result<()> {
        let tok = self.do_read_token(); // cpp directive
        tok.and_then(|t| match retrieve_ident!(t).as_str() {
            "include" => self.read_include(),
            "define" => self.read_define(),
            "undef" => self.read_undef(),
            "if" => self.read_if(),
            "ifdef" => self.read_ifdef(),
            "ifndef" => self.read_ifndef(),
            "elif" => self.read_elif(),
            "else" => self.read_else(),
            _ => Ok(()),
        })
    }

    fn read_include(&mut self) -> Result<()> {
        let path = self.read_header_file_path()?;
        let abs_path = self.try_include(path.clone()).map_or(
            Err(LexerError::Message(
                self.sub_lexers.back().unwrap().loc,
                format!("not found '{}'", path.as_path().display().to_string(),),
            )),
            |ok| Ok(ok),
        )?;

        self.sub_lexers
            .push_back(SubLexer::new(self.path_arena.clone(), abs_path));

        Ok(())
    }

    fn read_header_file_path(&mut self) -> Result<PathBuf> {
        let mut name = "".to_string();
        if self.skip_symbol(Symbol::Lt) {
            while self.get_char()? != '>' {
                name.push(self.next_char()?);
            }
            self.next_char()?; // >
        } else {
            let tok = self.do_read_token()?;
            if let TokenKind::String(s) = tok.kind {
                println!("sorry, using \"double quote\" in #include is currently not supported.");
                name = s;
            } else {
                return Err(LexerError::Message(
                    tok.loc,
                    "expected '<' or '\"'".to_string(),
                ));
            }
        }
        Ok(PathBuf::from(name))
    }

    fn try_include(&mut self, path: PathBuf) -> Option<PathBuf> {
        let header_paths = vec![
            "./include/",
            "/include/",
            "/usr/include/",
            "/usr/include/linux/",
            "/usr/include/x86_64-linux-gnu/",
            "./include/",
            "",
        ];
        header_paths
            .iter()
            .map(|p| PathBuf::from(p))
            .find_map(|mut header_path| {
                header_path.push(path.clone());
                if Path::new(&header_path).exists() {
                    Some(header_path)
                } else {
                    None
                }
            })
    }

    fn read_define(&mut self) -> Result<()> {
        let name = self.try_read_ident()?.1;
        let t = self.do_read_token()?;
        if !t.leading_space && retrieve_ident!(t) == "(" {
            self.read_define_func_macro(name)
        } else {
            self.unget(t);
            self.read_define_obj_macro(name)
        }
    }

    fn read_define_func_macro(&mut self, name: String) -> Result<()> {
        let mut params = FxHashMap::default();
        let mut count = 0;
        loop {
            let (loc, mut arg) = self.try_read_ident()?;
            if arg == ")" {
                break;
            }
            if count > 0 {
                if arg != "," {
                    return Err(LexerError::msg(loc, "expected comma"));
                }
                arg = self.try_read_ident()?.1;
            }
            params.insert(arg, count);
            count += 1;
        }

        let mut body = vec![];

        loop {
            let tok = self.do_read_token()?;
            if tok.kind == TokenKind::Newline {
                break;
            }

            let maybe_macro_name = self.try_read_ident()?.1;
            if let Some(&nth) = params.get(maybe_macro_name.as_str()) {
                let mut macro_param = tok;
                macro_param.kind = TokenKind::MacroParam { nth };
                body.push(macro_param);
            } else {
                body.push(tok);
            }
        }

        self.register_funclike_macro(name, body);

        Ok(())
    }

    fn read_define_obj_macro(&mut self, name: String) -> Result<()> {
        let mut body = vec![];
        loop {
            let t = self.do_read_token()?;
            if t.kind == TokenKind::Newline {
                break;
            }
            body.push(t);
        }
        self.register_funclike_macro(name, body);
        Ok(())
    }

    fn register_funclike_macro(&mut self, name: String, body: Vec<Token>) {
        self.macros.insert(name, Macro::FuncLike(body));
    }

    fn read_undef(&mut self) -> Result<()> {
        let name = self.try_read_ident()?.1;
        self.macros.remove(name.as_str());
        Ok(())
    }

    fn read_if(&mut self) -> Result<()> {
        let cond = self.read_constexpr()?;
        self.do_read_if(cond)
    }

    fn do_read_if(&mut self, cond: bool) -> Result<()> {
        self.cond_stack.push_back(cond);
        if !cond {
            self.skip_cond_include()?;
        }
        Ok(())
    }

    fn read_ifdef(&mut self) -> Result<()> {
        let name = self.try_read_ident()?.1;
        let is_defined = self.macros.contains_key(name.as_str());
        self.do_read_if(is_defined)
    }

    fn read_ifndef(&mut self) -> Result<()> {
        let name = self.try_read_ident()?.1;
        let is_undefined = !self.macros.contains_key(name.as_str());
        self.do_read_if(is_undefined)
    }

    fn read_elif(&mut self) -> Result<()> {
        if *self.cond_stack.back().unwrap() || !self.read_constexpr()? {
            self.skip_cond_include()?
        } else {
            *self.cond_stack.back_mut().unwrap() = true;
        }
        Ok(())
    }

    fn read_else(&mut self) -> Result<()> {
        if *self.cond_stack.back().unwrap() {
            self.skip_cond_include()?;
        }
        Ok(())
    }

    fn skip_cond_include(&mut self) -> Result<()> {
        let mut nest = 0;
        loop {
            let loc = self.sub_lexers.back().unwrap().loc;
            if self.next_char()? != '#' {
                continue;
            }

            let tok = self.do_read_token()?;
            let val = retrieve_ident(tok.clone())?.1;

            match val.as_str() {
                "else" | "elif" | "endif" if nest == 0 => {
                    self.unget(tok);
                    self.unget(Token::new(TokenKind::Identifier("#".to_string()), loc));
                    return Ok(());
                }
                "if" | "ifdef" | "ifndef" => nest += 1,
                "endif" => nest -= 1,
                _ => {}
            }
        }
    }

    fn read_constexpr(&mut self) -> Result<bool> {
        let expr = self.read_intexpr_line()?;
        let mut sub_lexer = SubLexer::new_expr(
            self.path_arena.clone(),
            self.sub_lexers.back().unwrap().path,
        );
        sub_lexer.unget(Token::new(
            TokenKind::Symbol(Symbol::Semicolon),
            SourceLoc::new(self.sub_lexers.back().unwrap().path),
        ));
        sub_lexer.unget_all(expr);

        self.sub_lexers.push_back(sub_lexer);

        // let node = parser::Parser::new(self).run_as_expr().ok().unwrap();
        todo!();
        // Ok(false)
    }

    fn read_intexpr_line(&mut self) -> Result<Vec<Token>> {
        let mut expr = vec![];
        loop {
            let tok = self.do_read_token()?;
            let tok = self.expand(tok)?;
            if tok.kind == TokenKind::Newline {
                break;
            }
            let tok = convert_to_symbol(tok);
            match tok.kind {
                TokenKind::Identifier(ident) if ident == "defined" => {
                    expr.push(self.read_defined_op()?)
                }
                TokenKind::Identifier(_) => expr.push(Token::new(
                    TokenKind::Int { n: 0, bits: 32 },
                    SourceLoc::new(self.sub_lexers.back().unwrap().path),
                )),
                _ => expr.push(tok),
            }
        }
        Ok(vec![])
    }

    fn read_defined_op(&mut self) -> Result<Token> {
        let mut tok = self.do_read_token()?;
        if retrieve_ident(tok.clone()).map_or(false, |(_, s)| s == "(") {
            tok = self.do_read_token()?;
            self.expect_skip_symbol(Symbol::ClosingParen)?;
        }
        if self.macros.contains_key(retrieve_ident(tok)?.1.as_str()) {
            Ok(Token::new(TokenKind::Int { n: 1, bits: 32 }, self.loc()))
        } else {
            Ok(Token::new(TokenKind::Int { n: 0, bits: 32 }, self.loc()))
        }
    }

    fn expand(&mut self, tok: Token) -> Result<Token> {
        let (loc, name) = retrieve_ident(tok.clone())?;

        match name.as_str() {
            "__LINE__" => {
                return Ok(Token::new(
                    TokenKind::Int {
                        n: loc.line as i64,
                        bits: 32,
                    },
                    loc,
                ));
            }
            "__FILE__" => {
                return Ok(Token::new(
                    TokenKind::String(
                        self.path_arena.borrow()[self.path()]
                            .as_path()
                            .display()
                            .to_string(),
                    ),
                    loc,
                ));
            }
            _ => {}
        }

        if tok.hideset.contains(name.as_str()) || !self.macros.contains_key(name.as_str()) {
            return Ok(tok);
        }

        match self.macros.get(name.as_str()).unwrap().clone() {
            Macro::Object(body) => self.expand_obj_macro(tok, name, body),
            Macro::FuncLike(body) => self.expand_func_macro(tok, name, body),
        }?;

        self.do_get_token()
    }

    fn expand_obj_macro(&mut self, tok: Token, name: String, body: Vec<Token>) -> Result<()> {
        let body = body
            .into_iter()
            .map(|mut t| {
                t.hideset.insert(name.clone());
                t.loc = tok.loc;
                t
            })
            .collect();
        self.unget_all(body);
        Ok(())
    }

    fn expand_func_macro(&mut self, tok: Token, name: String, body: Vec<Token>) -> Result<()> {

        Ok(())
    }

    // utils

    fn loc(&self) -> SourceLoc {
        self.sub_lexers.back().unwrap().loc
    }

    fn path(&self) -> Id<PathBuf> {
        self.sub_lexers.back().unwrap().path
    }
}

impl SubLexer {
    pub fn new(path_arena: PathArena, path: PathBuf) -> Self {
        let source = fs::read_to_string(path.clone()).unwrap();
        let path = path_arena.borrow_mut().alloc(path);
        Self {
            path_arena,
            path,
            source,
            buf: VecDeque::new(),
            loc: SourceLoc::new(path),
        }
    }

    pub fn new_expr(path_arena: PathArena, path: Id<PathBuf>) -> Self {
        Self {
            path_arena,
            path,
            source: "".to_string(),
            buf: VecDeque::new(),
            loc: SourceLoc::new(path),
        }
    }

    pub fn file_path(&self) -> Ref<Arena<PathBuf>> {
        self.path_arena.borrow()
    }

    // reading char

    pub fn get_char(&mut self) -> Result<char> {
        if self.loc.pos >= self.source.len() {
            return Err(LexerError::EOF);
        }
        self.source[self.loc.pos..]
            .chars()
            .next()
            .map_or(Err(LexerError::EOF), |c| Ok(c))
    }

    pub fn get_char2(&mut self) -> Result<char> {
        if self.loc.pos + 1 >= self.source.len() {
            return Err(LexerError::EOF);
        }
        self.source[self.loc.pos + 1..]
            .chars()
            .next()
            .map_or(Err(LexerError::EOF), |c| Ok(c))
    }

    pub fn next_char(&mut self) -> Result<char> {
        if self.loc.pos >= self.source.len() {
            return Err(LexerError::EOF);
        }
        let c = self.source[self.loc.pos..]
            .chars()
            .next()
            .map_or(Err(LexerError::EOF), |c| Ok(c));

        if c == Ok('\n') {
            self.loc.line += 1;
        }

        self.loc.pos += 1;
        c
    }

    pub fn skip_char(&mut self, c: char) -> bool {
        if let Ok(c_) = self.get_char() {
            return c_ == c;
        }
        false
    }

    // reading token

    // pub fn read_token(&mut self) -> Result<Token> {
    //     let token = self.do_read_token();
    //     token.and_then(|tok| match tok.kind {
    //         TokenKind::Newline => self.read_token(),
    //         TokenKind::Identifier(_) => Ok(convert_to_symbol(tok)),
    //         _ => Ok(tok),
    //     })
    // }

    pub fn do_read_token(&mut self) -> Result<Token> {
        if let Some(tok) = self.buf.pop_back() {
            return Ok(tok);
        }

        match self.get_char()? {
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),
            ' ' | '\t' => {
                self.next_char()?;
                self.do_read_token()
                    // set a leading space
                    .and_then(|tok| Ok(tok.leading_space(true)))
            }
            '0'..='9' => self.read_number_literal(),
            '\"' => self.read_string_literal(),
            '\'' => self.read_char_literal(),
            '\n' => self.read_newline(),
            '\\' => {
                while self.next_char()? != '\n' {}
                self.do_read_token()
            }
            '/' => {
                if self.get_char2()? == '*' {
                    assert_eq!(self.next_char()?, '/');
                    assert_eq!(self.next_char()?, '*');
                    let mut last = ' ';
                    while !(last == '*' && self.get_char()? == '/') {
                        last = self.next_char()?;
                    }
                    self.next_char()?; // /
                    self.do_read_token()
                } else if self.get_char2()? == '/' {
                    assert_eq!(self.next_char()?, '/'); // /
                    assert_eq!(self.next_char()?, '/'); // /
                    while self.get_char()? != '\n' {
                        self.next_char()?;
                    }
                    self.do_read_token()
                } else {
                    self.read_symbol()
                }
            }
            _ => self.read_symbol(),
        }
    }

    pub fn read_identifier(&mut self) -> Result<Token> {
        let mut ident = "".to_string();
        let loc = self.loc;

        loop {
            let c = self.get_char()?;
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.next_char()?;
                continue;
            }
            break;
        }

        Ok(Token::new(TokenKind::Identifier(ident), loc))
    }

    pub fn read_number_literal(&mut self) -> Result<Token> {
        let mut num = "".to_string();
        let mut is_float = false;
        let mut last = self.get_char()?;
        let loc = self.loc;

        loop {
            let c = self.get_char()?;
            num.push(c);
            is_float = is_float || c == '.';
            let is_float2 = "eEpP".contains(last) && "+-".contains(c);
            if c.is_alphanumeric() || c == '.' {
                is_float = is_float || is_float2;
                last = self.next_char()?;
                continue;
            }
            num.pop();
            break;
        }

        if is_float {
            let num = num.parse().unwrap();
            return Ok(Token::new(TokenKind::Float(num), loc));
        }

        let n = if num.len() > 2 && num.starts_with("0x") {
            self.parse_hex_number(&num[2..])
        } else if num.chars().nth(0).unwrap() == '0' {
            self.parse_oct_number(&num[1..])
        } else {
            self.parse_dec_number(num.as_str())
        };
        let max_32bits = 0xffffffff;
        let bits = if 0 == (n & !max_32bits) { 32 } else { 64 };

        Ok(Token::new(TokenKind::Int { n, bits }, loc))
    }

    fn parse_dec_number(&mut self, num_literal: &str) -> i64 {
        let n = num_literal.chars().fold(0, |n, c| match c {
            '0'..='9' => n * 10 + c.to_digit(10).unwrap() as u64,
            _ => n,
        });
        n as i64
    }

    fn parse_oct_number(&mut self, num_literal: &str) -> i64 {
        let n = num_literal.chars().fold(0, |n, c| match c {
            '0'..='7' => n * 8 + c.to_digit(8).unwrap() as u64,
            _ => n,
        });
        n as i64
    }

    fn parse_hex_number(&mut self, num_literal: &str) -> i64 {
        let n = num_literal.chars().fold(0, |n, c| match c {
            '0'..='9' | 'A'..='F' | 'a'..='f' => n * 16 + c.to_digit(16).unwrap() as u64,
            _ => n,
        });
        n as i64
    }

    pub fn read_symbol(&mut self) -> Result<Token> {
        let mut sym = "".to_string();
        let loc = self.loc;

        let c = self.get_char()?;
        sym.push(c);

        match self.next_char()? {
            '+' | '-' => {
                if self.get_char()? == '='
                    || self.get_char()? == '>'
                    || self.get_char()? == '+'
                    || self.get_char()? == '-'
                {
                    sym.push(self.next_char()?);
                }
            }
            '*' | '/' | '%' | '=' | '^' | '!' => {
                if self.get_char()? == '=' {
                    sym.push(self.next_char()?);
                }
            }
            '<' | '>' | '&' | '|' => {
                if self.get_char()? == c {
                    sym.push(self.next_char()?);
                }
                if self.get_char()? == '=' {
                    sym.push(self.next_char()?);
                }
            }
            '.' => {
                if self.get_char()? == '.' {
                    sym.push(self.next_char()?);
                    sym.push(self.next_char()?);
                }
            }
            _ => {}
        };

        Ok(Token::new(TokenKind::Identifier(sym), loc))
    }

    pub fn read_escaped_char(&mut self) -> Result<char> {
        let c = self.next_char()?;
        match c {
            '\'' | '"' | '?' | '\\' => Ok(c),
            'a' => Ok('\x07'),
            'b' => Ok('\x08'),
            'f' => Ok('\x0c'),
            'n' => Ok('\x0a'),
            'r' => Ok('\x0d'),
            't' => Ok('\x09'),
            'v' => Ok('\x0b'),
            'x' => {
                let mut hex = "".to_string();
                loop {
                    let c = self.get_char()?;
                    if c.is_alphanumeric() {
                        hex.push(self.next_char()?);
                        continue;
                    }
                    break;
                }
                Ok(self.parse_hex_number(hex.as_str()) as i32 as u8 as char)
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
                // if '0', check whether octal number \nnn or null \0
                if self.get_char()?.is_numeric() {
                    let mut oct = "".to_string();
                    oct.push(c);
                    loop {
                        let c = self.get_char()?;
                        if c.is_numeric() {
                            oct.push(self.next_char()?);
                            continue;
                        }
                        break;
                    }
                    Ok(self.parse_oct_number(oct.as_str()) as i32 as u8 as char)
                } else {
                    assert!(c == '0');
                    Ok('\x00')
                }
            }
            _ => Ok(c),
        }
    }

    pub fn read_newline(&mut self) -> Result<Token> {
        assert!(self.next_char()? == '\n');
        Ok(Token::new(TokenKind::Newline, self.loc))
    }

    pub fn read_string_literal(&mut self) -> Result<Token> {
        self.next_char()?;
        let loc = self.loc;
        let mut s = "".to_string();
        loop {
            match self.next_char()? {
                '"' => break,
                '\\' => s.push(self.read_escaped_char()?),
                c => s.push(c),
            }
        }
        Ok(Token::new(TokenKind::String(s), loc))
    }

    pub fn read_char_literal(&mut self) -> Result<Token> {
        self.next_char()?;
        let loc = self.loc;
        let c = {
            let c = self.next_char()?;
            if c == '\\' {
                self.read_escaped_char()?
            } else {
                c
            }
        };
        if self.next_char()? != '\'' {
            return Err(LexerError::Message(
                loc,
                "missing terminating '\''".to_string(),
            ));
        }
        Ok(Token::new(TokenKind::Char(c), loc))
    }

    // utils for token

    pub fn unget(&mut self, t: Token) {
        self.buf.push_back(t)
    }

    pub fn unget_all(&mut self, ts: Vec<Token>) {
        self.buf.extend(ts.into_iter().rev());
    }
}

fn convert_to_symbol(token: Token) -> Token {
    let ident = match token.kind {
        TokenKind::Identifier(ref ident) => ident,
        _ => panic!(),
    };
    let symbol = match ident.as_str() {
        "sizeof" => TokenKind::Symbol(Symbol::Sizeof),
        "++" => TokenKind::Symbol(Symbol::Inc),
        "--" => TokenKind::Symbol(Symbol::Dec),
        "(" => TokenKind::Symbol(Symbol::OpeningParen),
        ")" => TokenKind::Symbol(Symbol::ClosingParen),
        "[" => TokenKind::Symbol(Symbol::OpeningBoxBracket),
        "]" => TokenKind::Symbol(Symbol::ClosingBoxBracket),
        "{" => TokenKind::Symbol(Symbol::OpeningBrace),
        "}" => TokenKind::Symbol(Symbol::ClosingBrace),
        "." => TokenKind::Symbol(Symbol::Point),
        "," => TokenKind::Symbol(Symbol::Comma),
        ";" => TokenKind::Symbol(Symbol::Semicolon),
        ":" => TokenKind::Symbol(Symbol::Colon),
        "->" => TokenKind::Symbol(Symbol::Arrow),
        "+" => TokenKind::Symbol(Symbol::Add),
        "-" => TokenKind::Symbol(Symbol::Sub),
        "!" => TokenKind::Symbol(Symbol::Not),
        "~" => TokenKind::Symbol(Symbol::BitwiseNot),
        "*" => TokenKind::Symbol(Symbol::Asterisk),
        "&" => TokenKind::Symbol(Symbol::Ampersand),
        "/" => TokenKind::Symbol(Symbol::Div),
        "%" => TokenKind::Symbol(Symbol::Mod),
        "<<" => TokenKind::Symbol(Symbol::Shl),
        ">>" => TokenKind::Symbol(Symbol::Shr),
        "<" => TokenKind::Symbol(Symbol::Lt),
        "<=" => TokenKind::Symbol(Symbol::Le),
        ">" => TokenKind::Symbol(Symbol::Gt),
        ">=" => TokenKind::Symbol(Symbol::Ge),
        "==" => TokenKind::Symbol(Symbol::Eq),
        "!=" => TokenKind::Symbol(Symbol::Ne),
        "^" => TokenKind::Symbol(Symbol::Xor),
        "|" => TokenKind::Symbol(Symbol::Or),
        "&&" => TokenKind::Symbol(Symbol::LAnd),
        "||" => TokenKind::Symbol(Symbol::LOr),
        "?" => TokenKind::Symbol(Symbol::Question),
        "=" => TokenKind::Symbol(Symbol::Assign),
        "+=" => TokenKind::Symbol(Symbol::AssignAdd),
        "-=" => TokenKind::Symbol(Symbol::AssignSub),
        "*=" => TokenKind::Symbol(Symbol::AssignMul),
        "/=" => TokenKind::Symbol(Symbol::AssignDiv),
        "%=" => TokenKind::Symbol(Symbol::AssignMod),
        "<<=" => TokenKind::Symbol(Symbol::AssignShl),
        ">>=" => TokenKind::Symbol(Symbol::AssignShr),
        "&=" => TokenKind::Symbol(Symbol::AssignAnd),
        "^=" => TokenKind::Symbol(Symbol::AssignXor),
        "|=" => TokenKind::Symbol(Symbol::AssignOr),
        "#" => TokenKind::Symbol(Symbol::Hash),
        "..." => TokenKind::Symbol(Symbol::Vararg),
        _ => return token,
    };
    Token::new(symbol, token.loc)
}

fn maybe_convert_to_keyword(token: Token) -> Token {
    let ident = match token.kind {
        TokenKind::Identifier(ref ident) => ident,
        _ => return token,
    };
    let keyw = match ident.as_str() {
        "typedef" => TokenKind::Keyword(Keyword::Typedef),
        "extern" => TokenKind::Keyword(Keyword::Extern),
        "auto" => TokenKind::Keyword(Keyword::Auto),
        "register" => TokenKind::Keyword(Keyword::Register),
        "static" => TokenKind::Keyword(Keyword::Static),
        "restrict" => TokenKind::Keyword(Keyword::Restrict),
        "const" => TokenKind::Keyword(Keyword::Const),
        "constexpr" => TokenKind::Keyword(Keyword::ConstExpr),
        "volatile" => TokenKind::Keyword(Keyword::Volatile),
        "void" => TokenKind::Keyword(Keyword::Void),
        "signed" => TokenKind::Keyword(Keyword::Signed),
        "unsigned" => TokenKind::Keyword(Keyword::Unsigned),
        "char" => TokenKind::Keyword(Keyword::Char),
        "int" => TokenKind::Keyword(Keyword::Int),
        "bool" => TokenKind::Keyword(Keyword::Int),
        "short" => TokenKind::Keyword(Keyword::Short),
        "long" => TokenKind::Keyword(Keyword::Long),
        "float" => TokenKind::Keyword(Keyword::Float),
        "double" => TokenKind::Keyword(Keyword::Double),
        "struct" => TokenKind::Keyword(Keyword::Struct),
        "union" => TokenKind::Keyword(Keyword::Union),
        "enum" => TokenKind::Keyword(Keyword::Enum),
        "inline" => TokenKind::Keyword(Keyword::Inline),
        "noreturn" => TokenKind::Keyword(Keyword::Noreturn),
        "if" => TokenKind::Keyword(Keyword::If),
        "else" => TokenKind::Keyword(Keyword::Else),
        "for" => TokenKind::Keyword(Keyword::For),
        "while" => TokenKind::Keyword(Keyword::While),
        "do" => TokenKind::Keyword(Keyword::Do),
        "switch" => TokenKind::Keyword(Keyword::Switch),
        "case" => TokenKind::Keyword(Keyword::Case),
        "default" => TokenKind::Keyword(Keyword::Default),
        "goto" => TokenKind::Keyword(Keyword::Goto),
        "break" => TokenKind::Keyword(Keyword::Break),
        "continue" => TokenKind::Keyword(Keyword::Continue),
        "return" => TokenKind::Keyword(Keyword::Return),
        _ => return token,
    };
    Token::new(keyw, token.loc)
}

impl LexerError {
    pub fn msg(loc: SourceLoc, msg: &'static str) -> Self {
        Self::Message(loc, msg.to_string())
    }
}
