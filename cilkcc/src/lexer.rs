use super::{
    parser, token,
    token::{Keyword, SourceLoc, Symbol, Token},
};
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
    last_loc: SourceLoc,
    is_temporary: bool,
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
    Message(SourceLoc, String),
    EOF,
}

#[derive(Debug, Clone)]
pub enum Macro {
    Object(Vec<Token>),
    FuncLike(Vec<Token>),
}

#[macro_export]
macro_rules! retrieve_ident {
    ($e:expr) => {
        match &$e.kind {
            &token::Kind::Identifier(ref ident) => ident.to_string(),
            _ => "".to_string(),
        }
    };
}

macro_rules! retrieve_ident_mut {
    ($e:expr) => {
        match &mut $e.kind {
            &mut token::Kind::Identifier(ref mut ident) => ident,
            _ => panic!(),
        }
    };
}

macro_rules! retrieve_str {
    ($e:expr) => {
        match &$e.kind {
            &token::Kind::String(ref s) => s.to_string(),
            _ => panic!(),
        }
    };
}
fn retrieve_ident(tok: Token) -> Result<(SourceLoc, String)> {
    match tok.kind {
        token::Kind::Identifier(ident) => Ok((tok.loc, ident)),
        _ => return Err(Error::msg(tok.loc, "expected identifier")),
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
            if matches!(tok.kind, token::Kind::String(_))
                && matches!(self.peek_token()?.kind, token::Kind::String(_))
            {
                let s1 = retrieve_str!(tok);
                let s2 = retrieve_str!(self.get_token()?);
                let mut new_tok = tok;
                let mut concat_str = s1;
                concat_str.push_str(s2.as_str());
                new_tok.kind = token::Kind::String(concat_str);
                Ok(new_tok)
            } else {
                Ok(maybe_convert_to_keyword(tok))
            }
        })
    }

    pub fn peek_token(&mut self) -> Result<Token> {
        self.get_token().and_then(|tok| {
            self.unget(tok.clone());
            Ok(tok)
        })
    }

    pub fn peek2_token(&mut self) -> Result<Token> {
        let p1 = self.get_token()?;
        let p2 = self.get_token()?;
        self.unget(p2.clone());
        self.unget(p1);
        Ok(p2)
    }

    pub fn do_get_token(&mut self) -> Result<Token> {
        let tok = self.read_token().and_then(|tok| match &tok.kind {
            &token::Kind::Symbol(Symbol::Hash) => {
                self.read_cpp_directive()?;
                self.do_get_token()
            }
            _ => Ok(tok),
        })?;
        self.expand(tok)
    }

    pub fn unget(&mut self, t: Token) {
        self.sub_lexers.back_mut().unwrap().unget(t)
    }

    pub fn unget_all(&mut self, ts: Vec<Token>) {
        self.sub_lexers.back_mut().unwrap().unget_all(ts)
    }

    pub fn do_read_token(&mut self) -> Result<Token> {
        if self.sub_lexers.len() == 0 {
            return Err(Error::EOF);
        }

        match self.sub_lexers.back_mut().unwrap().do_read_token() {
            Ok(token) => Ok(token),
            Err(Error::EOF) => {
                let is_temporary = self.sub_lexers.pop_back().unwrap().is_temporary;
                if is_temporary || self.sub_lexers.len() == 0 {
                    return Err(Error::EOF);
                }
                return self.do_read_token();
            }
            Err(e) => return Err(e),
        }
    }

    pub fn read_token(&mut self) -> Result<Token> {
        let token = self.do_read_token();
        token.and_then(|tok| match tok.kind {
            token::Kind::Newline => self.read_token(),
            token::Kind::Identifier(_) => Ok(convert_to_symbol(tok)),
            _ => Ok(tok),
        })
    }

    pub fn skip_symbol(&mut self, sym: Symbol) -> Result<bool> {
        let tok = self.get_token()?;
        match tok.kind {
            token::Kind::Symbol(s) if s == sym => return Ok(true),
            _ => self.unget(tok),
        }
        Ok(false)
    }

    pub fn skip_keyword(&mut self, keyw: Keyword) -> Result<bool> {
        let tok = self.get_token()?;
        match tok.kind {
            token::Kind::Keyword(k) if k == keyw => return Ok(true),
            _ => self.unget(tok),
        }
        Ok(false)
    }

    pub fn expect_skip_symbol(&mut self, sym: Symbol) -> Result<()> {
        if !self.skip_symbol(sym)? {
            return Err(Error::Message(self.loc(), format!("expected '{:?}'", sym)));
        }
        Ok(())
    }

    pub fn expect_skip_keyword(&mut self, keyw: Keyword) -> Result<()> {
        if !self.skip_keyword(keyw)? {
            return Err(Error::Message(self.loc(), format!("expected '{:?}'", keyw)));
        }
        Ok(())
    }

    fn try_read_ident(&mut self) -> Result<(SourceLoc, String)> {
        let tok = self.do_read_token()?;
        match tok.kind {
            token::Kind::Identifier(ident) => Ok((tok.loc, ident)),
            _ => return Err(Error::msg(tok.loc, "expected identifier")),
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
            // "endif" => panic!(),
            _ => Ok(()),
        })
    }

    fn read_include(&mut self) -> Result<()> {
        let (path, is_dquote) = self.read_header_file_path()?;
        let abs_path = self.try_include(path.clone(), is_dquote).map_or(
            Err(Error::Message(
                self.sub_lexers.back().unwrap().loc,
                format!("not found '{}'", path.as_path().display().to_string(),),
            )),
            |ok| Ok(ok),
        )?;

        self.sub_lexers
            .push_back(SubLexer::new(self.path_arena.clone(), abs_path));

        Ok(())
    }

    fn read_header_file_path(&mut self) -> Result<(PathBuf, bool)> {
        let mut name = "".to_string();
        let mut is_dquote = false;
        if self.skip_symbol(Symbol::Lt)? {
            while self.get_char()? != '>' {
                name.push(self.next_char()?);
            }
            self.next_char()?; // >
        } else {
            let tok = self.do_read_token()?;
            if let token::Kind::String(s) = tok.kind {
                name = s;
                is_dquote = true;
            } else {
                return Err(Error::Message(tok.loc, "expected '<' or '\"'".to_string()));
            }
        }
        Ok((PathBuf::from(name), is_dquote))
    }

    fn try_include(&mut self, path: PathBuf, is_dquote: bool) -> Option<PathBuf> {
        let mut header_paths: Vec<PathBuf> = vec![
            "./include/",
            "/include/",
            "/usr/include/",
            "/usr/include/linux/",
            "/usr/include/x86_64-linux-gnu/",
            "./include/", // todo
            "./examples", // todo
            "",
        ]
        .into_iter()
        .map(|p| PathBuf::from(p))
        .collect();
        if is_dquote {
            let parent = self.path_arena.borrow()[self.path()]
                .parent()
                .unwrap()
                .to_path_buf();
            header_paths.insert(0, parent);
        }
        header_paths.into_iter().find_map(|mut header_path| {
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
                    return Err(Error::msg(loc, "expected comma"));
                }
                arg = retrieve_ident!(self.do_read_token()?);
            }
            params.insert(arg, count);
            count += 1;
        }

        let mut body = vec![];

        loop {
            let tok = self.do_read_token()?;
            if tok.kind == token::Kind::Newline {
                break;
            }

            let maybe_macro_name = retrieve_ident!(tok);
            if let Some(&nth) = params.get(maybe_macro_name.as_str()) {
                let mut macro_param = tok;
                macro_param.kind = token::Kind::MacroParam { nth };
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
            if t.kind == token::Kind::Newline {
                break;
            }
            body.push(t);
        }
        self.register_obj_macro(name, body);
        Ok(())
    }

    fn register_obj_macro(&mut self, name: String, body: Vec<Token>) {
        self.macros.insert(name, Macro::Object(body));
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
            let val = retrieve_ident!(tok.clone());

            match val.as_str() {
                "else" | "elif" | "endif" if nest == 0 => {
                    self.unget(tok);
                    self.unget(Token::new(token::Kind::Identifier("#".to_string()), loc));
                    return Ok(());
                }
                "if" | "ifdef" | "ifndef" => nest += 1,
                "endif" => nest -= 1,
                _ => {}
            }
        }
    }

    fn read_constexpr(&mut self) -> Result<bool> {
        let loc = self.loc();
        let expr = self.read_intexpr_line()?;
        let mut sub_lexer = SubLexer::new_expr(
            self.path_arena.clone(),
            self.sub_lexers.back().unwrap().path,
        )
        .temporary(true);
        sub_lexer.unget(Token::new(
            token::Kind::Symbol(Symbol::Semicolon),
            SourceLoc::new(self.sub_lexers.back().unwrap().path),
        ));
        sub_lexer.unget_all(expr);
        self.sub_lexers.push_back(sub_lexer);
        let node = match parser::Parser::new(self).parse_as_expr() {
            Err(Error::EOF) => {
                return Err(Error::msg(self.loc(), "reached end of expression"));
            }
            Err(Error::Message(loc, m)) => {
                self.sub_lexers.pop_back();
                return Err(Error::Message(loc, m));
            }
            Ok(node) => {
                self.sub_lexers.pop_back();
                node
            }
        };
        if let Some(e) = node.eval() {
            Ok(e != 0)
        } else {
            Err(Error::msg(loc, "expected constexpr"))
        }
    }

    fn read_intexpr_line(&mut self) -> Result<Vec<Token>> {
        let mut expr = vec![];
        loop {
            let tok = self.do_read_token()?;
            let tok = self.expand(tok)?;
            if tok.kind == token::Kind::Newline {
                break;
            }
            let tok = convert_to_symbol(tok);
            match tok.kind {
                token::Kind::Identifier(ident) if ident == "defined" => {
                    expr.push(self.read_defined_op()?)
                }
                token::Kind::Identifier(_) => expr.push(Token::new(
                    token::Kind::Int { n: 0, bits: 32 },
                    SourceLoc::new(self.sub_lexers.back().unwrap().path),
                )),
                _ => expr.push(tok),
            }
        }
        Ok(expr)
    }

    fn read_defined_op(&mut self) -> Result<Token> {
        let mut tok = self.do_read_token()?;
        if retrieve_ident(tok.clone()).map_or(false, |(_, s)| s == "(") {
            tok = self.do_read_token()?;
            self.expect_skip_symbol(Symbol::ClosingParen)?;
        }
        if self.macros.contains_key(retrieve_ident(tok)?.1.as_str()) {
            Ok(Token::new(token::Kind::Int { n: 1, bits: 32 }, self.loc()))
        } else {
            Ok(Token::new(token::Kind::Int { n: 0, bits: 32 }, self.loc()))
        }
    }

    fn expand(&mut self, tok: Token) -> Result<Token> {
        let name = retrieve_ident!(tok.clone());

        match name.as_str() {
            "__LINE__" => {
                return Ok(Token::new(
                    token::Kind::Int {
                        n: tok.loc.line as i64,
                        bits: 32,
                    },
                    tok.loc,
                ));
            }
            "__FILE__" => {
                return Ok(Token::new(
                    token::Kind::String(
                        self.path_arena.borrow()[self.path()]
                            .as_path()
                            .display()
                            .to_string(),
                    ),
                    tok.loc,
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
        let paren = self.read_token()?;
        if paren.kind != token::Kind::Symbol(Symbol::OpeningParen) {
            return Err(Error::msg(paren.loc, "expected '('"));
        }

        let mut args = vec![];
        loop {
            let (arg, is_end) = self.read_one_arg()?;
            args.push(arg);
            if is_end {
                break;
            }
        }

        let mut expanded = vec![];
        let mut is_stringize = false;
        let mut is_combine = false;

        for mcro_tok in body {
            if retrieve_ident!(mcro_tok.clone()) == "#" {
                if is_stringize {
                    is_stringize = false;
                    is_combine = true;
                } else {
                    is_stringize = true;
                }
                continue;
            }

            if let token::Kind::MacroParam { nth } = mcro_tok.kind {
                if is_stringize {
                    let stringized = stringize(tok.loc, args[nth].clone());
                    expanded.push(stringized);
                    is_stringize = false;
                } else if is_combine {
                    let mut last = expanded.pop().unwrap();
                    for t in &args[nth] {
                        *retrieve_ident_mut!(last) += retrieve_ident(t.clone())?.1.as_str();
                    }
                    expanded.push(last);
                    is_combine = false;
                } else {
                    let mut sub_lexer =
                        SubLexer::new_expr(self.path_arena.clone(), self.path()).temporary(true);
                    sub_lexer.unget_all(args[nth].clone());
                    self.sub_lexers.push_back(sub_lexer);
                    loop {
                        match self.do_get_token() {
                            Ok(ok) => expanded.push(ok),
                            Err(Error::EOF) => break,
                            Err(e) => return Err(e),
                        }
                    }
                }
            } else {
                if is_combine {
                    let mut last = expanded.pop().unwrap();
                    *retrieve_ident_mut!(last) += retrieve_ident(mcro_tok)?.1.as_str();
                    expanded.push(last);
                } else {
                    expanded.push(mcro_tok.clone());
                }
            }
        }

        for t in &mut expanded {
            t.hideset.insert(name.to_string());
            t.loc = tok.loc;
        }

        self.unget_all(expanded);

        Ok(())
    }

    fn read_one_arg(&mut self) -> Result<(Vec<Token>, bool)> {
        let mut nest = 0;
        let mut arg = vec![];
        loop {
            let tok = self.do_read_token()?;
            let val = retrieve_ident!(tok.clone());
            match val.as_str() {
                ")" if nest == 0 => return Ok((arg, true)),
                "," if nest == 0 => return Ok((arg, false)),
                "(" => nest += 1,
                ")" => nest -= 1,
                _ => {}
            }
            arg.push(tok);
        }
    }

    // utils

    pub fn loc(&self) -> SourceLoc {
        self.sub_lexers.back().unwrap().loc
    }

    pub fn last_loc(&self) -> SourceLoc {
        self.sub_lexers.back().unwrap().last_loc
    }

    pub fn path(&self) -> Id<PathBuf> {
        self.sub_lexers.back().unwrap().path
    }

    pub fn path_arena(&self) -> &PathArena {
        &self.path_arena
    }

    // TODO: REFINE CODE
    pub fn get_surrounding_line(&self, loc: SourceLoc) -> String {
        let source =
            if self.sub_lexers.len() > 0 && self.sub_lexers.back().unwrap().path == loc.file {
                self.sub_lexers.back().unwrap().source.clone()
            } else {
                let path = &self.path_arena.borrow()[loc.file];
                fs::read_to_string(path).unwrap()
            };
        let mut peek_pos = loc.pos;
        if peek_pos >= source.len() {
            peek_pos = source.len() - 1
        }
        let start_pos = {
            let mut p = peek_pos as i32;
            while p >= 0 && source[p as usize..].chars().next().unwrap() as char != '\n' {
                p -= 1;
            }
            p += 1; // '\n'
            p as usize
        };
        let end_pos = {
            let mut p = peek_pos as i32;
            while p < source.len() as i32 && source[p as usize..].chars().next().unwrap() != '\n' {
                p += 1;
            }
            p as usize
        };
        let surrounding_code = source[start_pos..end_pos].to_string();
        let mut err_point = String::new();
        for _ in 0..(peek_pos - start_pos) {
            err_point.push(' ');
        }
        err_point.push('^');
        surrounding_code + "\n" + err_point.as_str()
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
            last_loc: SourceLoc::new(path),
            is_temporary: false,
        }
    }

    pub fn new_expr(path_arena: PathArena, path: Id<PathBuf>) -> Self {
        Self {
            path_arena,
            path,
            source: "".to_string(),
            buf: VecDeque::new(),
            loc: SourceLoc::new(path),
            last_loc: SourceLoc::new(path),
            is_temporary: false,
        }
    }

    pub fn file_path(&self) -> Ref<Arena<PathBuf>> {
        self.path_arena.borrow()
    }

    pub fn temporary(mut self, t: bool) -> Self {
        self.is_temporary = t;
        self
    }

    // reading char

    pub fn get_char(&mut self) -> Result<char> {
        if self.loc.pos >= self.source.len() {
            return Err(Error::EOF);
        }
        self.source[self.loc.pos..]
            .chars()
            .next()
            .map_or(Err(Error::EOF), |c| Ok(c))
    }

    pub fn get_char2(&mut self) -> Result<char> {
        if self.loc.pos + 1 >= self.source.len() {
            return Err(Error::EOF);
        }
        self.source[self.loc.pos + 1..]
            .chars()
            .next()
            .map_or(Err(Error::EOF), |c| Ok(c))
    }

    pub fn next_char(&mut self) -> Result<char> {
        if self.loc.pos >= self.source.len() {
            return Err(Error::EOF);
        }
        let c = self.source[self.loc.pos..]
            .chars()
            .next()
            .map_or(Err(Error::EOF), |c| Ok(c));

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

    pub fn do_read_token(&mut self) -> Result<Token> {
        if let Some(tok) = self.buf.pop_back() {
            return Ok(tok);
        }

        self.last_loc = self.loc;

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

        Ok(Token::new(token::Kind::Identifier(ident), loc))
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
            return Ok(Token::new(token::Kind::Float(num), loc));
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

        Ok(Token::new(token::Kind::Int { n, bits }, loc))
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

        Ok(Token::new(token::Kind::Identifier(sym), loc))
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
        Ok(Token::new(token::Kind::Newline, self.loc))
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
        Ok(Token::new(token::Kind::String(s), loc))
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
            return Err(Error::Message(loc, "missing terminating '\''".to_string()));
        }
        Ok(Token::new(token::Kind::Char(c), loc))
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
    let ident = retrieve_ident!(token);
    let symbol = match ident.as_str() {
        "sizeof" => token::Kind::Symbol(Symbol::Sizeof),
        "++" => token::Kind::Symbol(Symbol::Inc),
        "--" => token::Kind::Symbol(Symbol::Dec),
        "(" => token::Kind::Symbol(Symbol::OpeningParen),
        ")" => token::Kind::Symbol(Symbol::ClosingParen),
        "[" => token::Kind::Symbol(Symbol::OpeningBoxBracket),
        "]" => token::Kind::Symbol(Symbol::ClosingBoxBracket),
        "{" => token::Kind::Symbol(Symbol::OpeningBrace),
        "}" => token::Kind::Symbol(Symbol::ClosingBrace),
        "." => token::Kind::Symbol(Symbol::Point),
        "," => token::Kind::Symbol(Symbol::Comma),
        ";" => token::Kind::Symbol(Symbol::Semicolon),
        ":" => token::Kind::Symbol(Symbol::Colon),
        "->" => token::Kind::Symbol(Symbol::Arrow),
        "+" => token::Kind::Symbol(Symbol::Add),
        "-" => token::Kind::Symbol(Symbol::Sub),
        "!" => token::Kind::Symbol(Symbol::Not),
        "~" => token::Kind::Symbol(Symbol::BitwiseNot),
        "*" => token::Kind::Symbol(Symbol::Asterisk),
        "&" => token::Kind::Symbol(Symbol::Ampersand),
        "/" => token::Kind::Symbol(Symbol::Div),
        "%" => token::Kind::Symbol(Symbol::Mod),
        "<<" => token::Kind::Symbol(Symbol::Shl),
        ">>" => token::Kind::Symbol(Symbol::Shr),
        "<" => token::Kind::Symbol(Symbol::Lt),
        "<=" => token::Kind::Symbol(Symbol::Le),
        ">" => token::Kind::Symbol(Symbol::Gt),
        ">=" => token::Kind::Symbol(Symbol::Ge),
        "==" => token::Kind::Symbol(Symbol::Eq),
        "!=" => token::Kind::Symbol(Symbol::Ne),
        "^" => token::Kind::Symbol(Symbol::Xor),
        "|" => token::Kind::Symbol(Symbol::Or),
        "&&" => token::Kind::Symbol(Symbol::LAnd),
        "||" => token::Kind::Symbol(Symbol::LOr),
        "?" => token::Kind::Symbol(Symbol::Question),
        "=" => token::Kind::Symbol(Symbol::Assign),
        "+=" => token::Kind::Symbol(Symbol::AssignAdd),
        "-=" => token::Kind::Symbol(Symbol::AssignSub),
        "*=" => token::Kind::Symbol(Symbol::AssignMul),
        "/=" => token::Kind::Symbol(Symbol::AssignDiv),
        "%=" => token::Kind::Symbol(Symbol::AssignMod),
        "<<=" => token::Kind::Symbol(Symbol::AssignShl),
        ">>=" => token::Kind::Symbol(Symbol::AssignShr),
        "&=" => token::Kind::Symbol(Symbol::AssignAnd),
        "^=" => token::Kind::Symbol(Symbol::AssignXor),
        "|=" => token::Kind::Symbol(Symbol::AssignOr),
        "#" => token::Kind::Symbol(Symbol::Hash),
        "..." => token::Kind::Symbol(Symbol::Vararg),
        _ => return token,
    };
    Token::new(symbol, token.loc)
}

fn maybe_convert_to_keyword(token: Token) -> Token {
    let ident = match token.kind {
        token::Kind::Identifier(ref ident) => ident,
        _ => return token,
    };
    let keyw = match ident.as_str() {
        "typedef" => token::Kind::Keyword(Keyword::Typedef),
        "extern" => token::Kind::Keyword(Keyword::Extern),
        "auto" => token::Kind::Keyword(Keyword::Auto),
        "register" => token::Kind::Keyword(Keyword::Register),
        "static" => token::Kind::Keyword(Keyword::Static),
        "restrict" => token::Kind::Keyword(Keyword::Restrict),
        "const" => token::Kind::Keyword(Keyword::Const),
        "constexpr" => token::Kind::Keyword(Keyword::ConstExpr),
        "volatile" => token::Kind::Keyword(Keyword::Volatile),
        "void" => token::Kind::Keyword(Keyword::Void),
        "signed" => token::Kind::Keyword(Keyword::Signed),
        "unsigned" => token::Kind::Keyword(Keyword::Unsigned),
        "char" => token::Kind::Keyword(Keyword::Char),
        "int" => token::Kind::Keyword(Keyword::Int),
        "bool" => token::Kind::Keyword(Keyword::Int),
        "short" => token::Kind::Keyword(Keyword::Short),
        "long" => token::Kind::Keyword(Keyword::Long),
        "float" => token::Kind::Keyword(Keyword::Float),
        "double" => token::Kind::Keyword(Keyword::Double),
        "struct" => token::Kind::Keyword(Keyword::Struct),
        "union" => token::Kind::Keyword(Keyword::Union),
        "enum" => token::Kind::Keyword(Keyword::Enum),
        "inline" => token::Kind::Keyword(Keyword::Inline),
        "noreturn" => token::Kind::Keyword(Keyword::Noreturn),
        "if" => token::Kind::Keyword(Keyword::If),
        "else" => token::Kind::Keyword(Keyword::Else),
        "for" => token::Kind::Keyword(Keyword::For),
        "while" => token::Kind::Keyword(Keyword::While),
        "do" => token::Kind::Keyword(Keyword::Do),
        "switch" => token::Kind::Keyword(Keyword::Switch),
        "case" => token::Kind::Keyword(Keyword::Case),
        "default" => token::Kind::Keyword(Keyword::Default),
        "goto" => token::Kind::Keyword(Keyword::Goto),
        "break" => token::Kind::Keyword(Keyword::Break),
        "continue" => token::Kind::Keyword(Keyword::Continue),
        "return" => token::Kind::Keyword(Keyword::Return),
        _ => return token,
    };
    Token::new(keyw, token.loc)
}

fn stringize(loc: SourceLoc, ts: Vec<Token>) -> Token {
    let string = ts
        .into_iter()
        .map(|t| {
            format!(
                "{}{}",
                if t.leading_space { " " } else { "" },
                match t.kind {
                    token::Kind::String(s) => format!("\"{}\"", s),
                    token::Kind::Int { n, .. } => format!("{}", n),
                    token::Kind::Float(ref f) => format!("{}", *f),
                    token::Kind::Identifier(ref i) => format!("{}", *i),
                    token::Kind::Char(ref c) => format!("\'{}\'", *c),
                    _ => "".to_string(),
                }
            )
        })
        .fold("".to_string(), |a, s| a + s.as_str())
        .trim_start() // remove leading spaces
        .to_string();
    Token::new(token::Kind::String(string), loc)
}

impl Error {
    pub fn msg(loc: SourceLoc, msg: &'static str) -> Self {
        Self::Message(loc, msg.to_string())
    }
}
