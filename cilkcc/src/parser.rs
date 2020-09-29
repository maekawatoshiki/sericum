use super::lexer::{Error, Lexer, Result};
use super::{ast, ast::AST};
use super::{token, token::Symbol};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer,
}

pub struct Env<T: Clone>(pub VecDeque<FxHashMap<String, T>>);

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Parser<'a> {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>> {
        let mut node = vec![];
        while let Ok(n) = self.read_toplevel() {
            node.push(n);
        }
        Ok(node)
    }

    pub fn parse_as_expr(&mut self) -> Result<AST> {
        self.read_expr()
    }

    fn read_toplevel(&mut self) -> Result<AST> {
        todo!()
    }

    fn read_expr(&mut self) -> Result<AST> {
        self.read_comma()
    }

    fn read_comma(&mut self) -> Result<AST> {
        let mut lhs = self.read_assign()?;
        while self.lexer.skip_symbol(Symbol::Comma)? {
            let rhs = self.read_assign()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::Comma, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_assign(&mut self) -> Result<AST> {
        let mut lhs = self.read_logical_or()?;
        if self.lexer.skip_symbol(Symbol::Question)? {
            todo!("ternary operator");
        }

        macro_rules! asgn {
            ($lhs:expr, $rhs:expr) => {{
                let loc = $lhs.loc;
                AST::new(
                    ast::Kind::Assign {
                        dst: Box::new($lhs),
                        src: Box::new($rhs),
                    },
                    loc,
                )
            }};
        }
        macro_rules! asgn_op {
            ($op:ident) => {
                lhs = asgn!(
                    lhs.clone(),
                    AST::new(
                        ast::Kind::BinaryOp(
                            ast::BinaryOp::$op,
                            Box::new(lhs),
                            Box::new(self.read_assign()?),
                        ),
                        self.lexer.loc(),
                    )
                );
            };
        }

        loop {
            let tok = self.lexer.get_token()?;
            match tok.kind {
                token::Kind::Symbol(Symbol::Assign) => lhs = asgn!(lhs, self.read_assign()?),
                token::Kind::Symbol(Symbol::AssignAdd) => asgn_op!(Add),
                token::Kind::Symbol(Symbol::AssignSub) => asgn_op!(Sub),
                token::Kind::Symbol(Symbol::AssignMul) => asgn_op!(Mul),
                token::Kind::Symbol(Symbol::AssignDiv) => asgn_op!(Div),
                token::Kind::Symbol(Symbol::AssignMod) => asgn_op!(Rem),
                token::Kind::Symbol(Symbol::AssignShl) => asgn_op!(Shl),
                token::Kind::Symbol(Symbol::AssignShr) => asgn_op!(Shr),
                token::Kind::Symbol(Symbol::AssignAnd) => asgn_op!(And),
                token::Kind::Symbol(Symbol::AssignOr) => asgn_op!(Or),
                token::Kind::Symbol(Symbol::AssignXor) => asgn_op!(Xor),
                _ => {
                    self.lexer.unget(tok);
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn read_logical_or(&mut self) -> Result<AST> {
        let mut lhs = self.read_logical_and()?;
        while self.lexer.skip_symbol(Symbol::Or)? {
            let rhs = self.read_logical_and()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::LogicalOr, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_logical_and(&mut self) -> Result<AST> {
        let mut lhs = self.read_or()?;
        while self.lexer.skip_symbol(Symbol::Ampersand)? {
            let rhs = self.read_or()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::LogicalAnd, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_or(&mut self) -> Result<AST> {
        let mut lhs = self.read_xor()?;
        while self.lexer.skip_symbol(Symbol::Or)? {
            let rhs = self.read_xor()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::Or, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_xor(&mut self) -> Result<AST> {
        let mut lhs = self.read_and()?;
        while self.lexer.skip_symbol(Symbol::Xor)? {
            let rhs = self.read_and()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::Xor, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_and(&mut self) -> Result<AST> {
        let mut lhs = self.read_eq_ne()?;
        while self.lexer.skip_symbol(Symbol::Ampersand)? {
            let rhs = self.read_eq_ne()?;
            lhs = AST::new(
                ast::Kind::BinaryOp(ast::BinaryOp::And, Box::new(lhs), Box::new(rhs)),
                self.lexer.loc(),
            )
        }
        Ok(lhs)
    }

    fn read_eq_ne(&mut self) -> Result<AST> {
        let mut lhs = self.read_relation()?;
        loop {
            if self.lexer.skip_symbol(Symbol::Eq)? {
                let rhs = self.read_primary()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Eq, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Ne)? {
                let rhs = self.read_relation()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Ne, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_relation(&mut self) -> Result<AST> {
        let mut lhs = self.read_shl_shr()?;
        loop {
            if self.lexer.skip_symbol(Symbol::Lt)? {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Lt, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Le)? {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Le, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Gt)? {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Gt, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Ge)? {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Ge, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_shl_shr(&mut self) -> Result<AST> {
        let mut lhs = self.read_add_sub()?;
        loop {
            if self.lexer.skip_symbol(Symbol::Shl)? {
                let rhs = self.read_add_sub()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Shl, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Shr)? {
                let rhs = self.read_add_sub()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Shr, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_add_sub(&mut self) -> Result<AST> {
        let mut lhs = self.read_mul_div_rem()?;
        loop {
            if self.lexer.skip_symbol(Symbol::Add)? {
                let rhs = self.read_mul_div_rem()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Add, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Sub)? {
                let rhs = self.read_mul_div_rem()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Sub, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_mul_div_rem(&mut self) -> Result<AST> {
        let mut lhs = self.read_cast()?;
        loop {
            if self.lexer.skip_symbol(Symbol::Asterisk)? {
                let rhs = self.read_cast()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Mul, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Div)? {
                let rhs = self.read_cast()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Div, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if self.lexer.skip_symbol(Symbol::Mod)? {
                let rhs = self.read_cast()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Rem, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_cast(&mut self) -> Result<AST> {
        // todo!()
        self.read_unary()
    }

    fn read_unary(&mut self) -> Result<AST> {
        let tok = self.lexer.get_token()?;

        match tok.kind {
            token::Kind::Symbol(Symbol::Not) => {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::LogicalNot, Box::new(self.read_cast()?)),
                    self.lexer.loc(),
                ));
            }
            token::Kind::Symbol(Symbol::BitwiseNot) => {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::BitwiseNot, Box::new(self.read_cast()?)),
                    self.lexer.loc(),
                ));
            }
            token::Kind::Symbol(Symbol::Add) => return self.read_cast(),
            token::Kind::Symbol(Symbol::Sub) => {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::Minus, Box::new(self.read_cast()?)),
                    self.lexer.loc(),
                ))
            }
            token::Kind::Symbol(Symbol::Inc) => {
                let loc = self.lexer.loc();
                let var = self.read_cast()?;
                return Ok(AST::new(
                    ast::Kind::Assign {
                        dst: Box::new(var.clone()),
                        src: Box::new(AST::new(
                            ast::Kind::BinaryOp(
                                ast::BinaryOp::Add,
                                Box::new(var),
                                Box::new(AST::new(ast::Kind::Int { n: 1, bits: 32 }, loc)),
                            ),
                            loc,
                        )),
                    },
                    loc,
                ));
            }
            token::Kind::Symbol(Symbol::Dec) => {
                let loc = self.lexer.loc();
                let var = self.read_cast()?;
                return Ok(AST::new(
                    ast::Kind::Assign {
                        dst: Box::new(var.clone()),
                        src: Box::new(AST::new(
                            ast::Kind::BinaryOp(
                                ast::BinaryOp::Sub,
                                Box::new(var),
                                Box::new(AST::new(ast::Kind::Int { n: 1, bits: 32 }, loc)),
                            ),
                            loc,
                        )),
                    },
                    loc,
                ));
            }
            token::Kind::Symbol(Symbol::Asterisk) => {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::Deref, Box::new(self.read_cast()?)),
                    self.lexer.loc(),
                ))
            }
            token::Kind::Symbol(Symbol::Ampersand) => {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::Addr, Box::new(self.read_cast()?)),
                    self.lexer.loc(),
                ))
            }
            token::Kind::Symbol(Symbol::Sizeof) => {
                // TODO: must fix this sloppy implementation
                // return self.read_sizeof();
                todo!()
            }
            _ => {}
        }

        self.lexer.unget(tok);
        self.read_postfix()
    }

    fn read_postfix(&mut self) -> Result<AST> {
        let mut ast = self.read_primary()?;
        loop {
            if self.lexer.skip_symbol(Symbol::OpeningParen)? {
                ast = self.read_func_call(ast)?;
                continue;
            }
            if self.lexer.skip_symbol(Symbol::OpeningBoxBracket)? {
                ast = AST::new(
                    ast::Kind::Load(Box::new(self.read_index(ast)?)),
                    self.lexer.loc(),
                );
                continue;
            }
            if self.lexer.skip_symbol(Symbol::Point)? {
                ast = AST::new(
                    ast::Kind::Load(Box::new(self.read_field(ast)?)),
                    self.lexer.loc(),
                );
                continue;
            }
            if self.lexer.skip_symbol(Symbol::Arrow)? {
                let loc = self.lexer.loc();
                let field = self.read_field(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::Deref, Box::new(ast)),
                    loc,
                ))?;
                ast = AST::new(ast::Kind::Load(Box::new(field)), loc);
                continue;
            }
            if self.lexer.skip_symbol(Symbol::Inc)? {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::PostInc, Box::new(ast)),
                    self.lexer.loc(),
                ));
            }
            if self.lexer.skip_symbol(Symbol::Dec)? {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::PostDec, Box::new(ast)),
                    self.lexer.loc(),
                ));
            }
            break;
        }
        Ok(ast)
    }

    fn read_func_call(&mut self, _: AST) -> Result<AST> {
        todo!()
    }

    fn read_field(&mut self, _: AST) -> Result<AST> {
        todo!()
    }

    fn read_index(&mut self, _: AST) -> Result<AST> {
        todo!()
    }

    fn read_primary(&mut self) -> Result<AST> {
        let loc = self.lexer.loc();
        let tok = self.lexer.get_token()?;

        match tok.kind {
            token::Kind::Int { n, bits } => Ok(AST::new(ast::Kind::Int { n, bits }, tok.loc)),
            token::Kind::Float(f) => Ok(AST::new(ast::Kind::Float(f), tok.loc)),
            token::Kind::Identifier(_ident) => todo!(),
            token::Kind::String(s) => Ok(AST::new(ast::Kind::String(s), tok.loc)),
            token::Kind::Char(c) => Ok(AST::new(ast::Kind::Char(c), tok.loc)),
            token::Kind::Symbol(s) => match s {
                Symbol::OpeningParen => {
                    let expr = self.read_expr()?;
                    self.lexer.expect_skip_symbol(Symbol::ClosingParen)?;
                    Ok(expr)
                }
                _ => Err(Error::Message(
                    loc,
                    format!("expected '(', but got {:?}", tok.kind),
                )),
            },
            _ => Err(Error::Message(loc, format!("unknown token {:?}", tok.kind))),
        }
    }
}

impl<T: Clone> Env<T> {
    pub fn new() -> Env<T> {
        let mut env = VecDeque::new();
        env.push_back(FxHashMap::default());
        Env(env)
    }

    pub fn push(&mut self) {
        let localenv = (*self.0.back().unwrap()).clone();
        self.0.push_back(localenv);
    }

    pub fn pop(&mut self) {
        self.0.pop_back();
    }

    pub fn add(&mut self, name: String, val: T) {
        self.0.back_mut().unwrap().insert(name, val);
    }

    pub fn add_global(&mut self, name: String, val: T) {
        // self.0[0].insert(name.clone(), val.clone());
        // self.0.back_mut().unwrap().insert(name, val);
        for env in &mut self.0 {
            env.insert(name.clone(), val.clone());
        }
    }

    pub fn is_local(&self) -> bool {
        self.0.len() > 1
    }

    pub fn back_mut(&mut self) -> Option<&mut FxHashMap<String, T>> {
        self.0.back_mut()
    }

    pub fn get(&mut self, name: &str) -> Option<&T> {
        self.0.back_mut().unwrap().get(name)
    }

    pub fn contains(&mut self, name: &str) -> bool {
        self.0.back_mut().unwrap().contains_key(name)
    }
}
