use super::token::{SourceLoc, Symbol, Token, TokenKind};
use id_arena::{Arena, Id};
use std::cell::{Ref, RefCell};
use std::collections::VecDeque;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

pub type PathArena = Rc<RefCell<Arena<PathBuf>>>;

pub struct Lexer {
    path_arena: PathArena,
    sub_lexers: VecDeque<FileLexer>,
}

pub struct FileLexer {
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

impl Lexer {
    pub fn new(path: PathBuf) -> Self {
        let path_arena = PathArena::new(RefCell::new(Arena::new()));
        Self {
            sub_lexers: {
                let mut sub_lexers = VecDeque::new();
                sub_lexers.push_back(FileLexer::new(path_arena.clone(), path));
                sub_lexers
            },
            path_arena,
        }
    }

    pub fn add_lexer(&mut self, path: PathBuf) {
        self.sub_lexers
            .push_back(FileLexer::new(self.path_arena.clone(), path))
    }

    // reading token

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
}

impl FileLexer {
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

    pub fn read_token(&mut self) -> Result<Token> {
        let token = self.do_read_token();
        token.and_then(|tok| match tok.kind {
            TokenKind::Newline => self.read_token(),
            TokenKind::Identifier(_) => Ok(convert_to_symbol(tok)),
            _ => Ok(tok),
        })
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

// fn maybe_convert_to_keyword(token: Token) -> Token {
//     let ident = match token.kind {
//         TokenKind::Identifier(ref ident) => ident,
//         _ => panic!(),
//     };
//     let keyw = match ident.as_str() {
//         "typedef" => TokenKind::Keyword(Keyword::Typedef),
//         "extern" => TokenKind::Keyword(Keyword::Extern),
//         "auto" => TokenKind::Keyword(Keyword::Auto),
//         "register" => TokenKind::Keyword(Keyword::Register),
//         "static" => TokenKind::Keyword(Keyword::Static),
//         "restrict" => TokenKind::Keyword(Keyword::Restrict),
//         "const" => TokenKind::Keyword(Keyword::Const),
//         "constexpr" => TokenKind::Keyword(Keyword::ConstExpr),
//         "volatile" => TokenKind::Keyword(Keyword::Volatile),
//         "void" => TokenKind::Keyword(Keyword::Void),
//         "signed" => TokenKind::Keyword(Keyword::Signed),
//         "unsigned" => TokenKind::Keyword(Keyword::Unsigned),
//         "char" => TokenKind::Keyword(Keyword::Char),
//         "int" => TokenKind::Keyword(Keyword::Int),
//         "bool" => TokenKind::Keyword(Keyword::Int),
//         "short" => TokenKind::Keyword(Keyword::Short),
//         "long" => TokenKind::Keyword(Keyword::Long),
//         "float" => TokenKind::Keyword(Keyword::Float),
//         "double" => TokenKind::Keyword(Keyword::Double),
//         "struct" => TokenKind::Keyword(Keyword::Struct),
//         "union" => TokenKind::Keyword(Keyword::Union),
//         "enum" => TokenKind::Keyword(Keyword::Enum),
//         "inline" => TokenKind::Keyword(Keyword::Inline),
//         "noreturn" => TokenKind::Keyword(Keyword::Noreturn),
//         "if" => TokenKind::Keyword(Keyword::If),
//         "else" => TokenKind::Keyword(Keyword::Else),
//         "for" => TokenKind::Keyword(Keyword::For),
//         "while" => TokenKind::Keyword(Keyword::While),
//         "do" => TokenKind::Keyword(Keyword::Do),
//         "switch" => TokenKind::Keyword(Keyword::Switch),
//         "case" => TokenKind::Keyword(Keyword::Case),
//         "default" => TokenKind::Keyword(Keyword::Default),
//         "goto" => TokenKind::Keyword(Keyword::Goto),
//         "break" => TokenKind::Keyword(Keyword::Break),
//         "continue" => TokenKind::Keyword(Keyword::Continue),
//         "return" => TokenKind::Keyword(Keyword::Return),
//         _ => return token,
//     };
//     Token::new(keyw, token.loc)
// }

impl SourceLoc {
    pub fn new(file: Id<PathBuf>) -> Self {
        Self {
            file,
            line: 1,
            pos: 0,
        }
    }
}
