use super::lexer::{Error, Lexer, Result};
use super::retrieve_ident;
use super::types::{CompoundType, CompoundTypes, Sign, StorageClass, Type};
use super::{ast, ast::AST};
use super::{
    token,
    token::{Keyword, Symbol},
};
use rustc_hash::FxHashMap;
use std::collections::hash_map;
use std::collections::VecDeque;
use {rand, rand::Rng};

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer,
    env: Env<AST>,
    tags: Env<Type>,
    pub compound_types: CompoundTypes,
}

pub struct Env<T: Clone>(pub VecDeque<FxHashMap<String, T>>);

pub struct Qualifiers {
    pub restrict: bool,
    pub const_: bool,
    pub constexpr: bool,
    pub volatile: bool,
    pub inline: bool,
    pub noreturn: bool,
}

macro_rules! expect_symbol_error {
    ($self:expr, $sym:ident, $msg:expr) => {{
        match $self.lexer.skip_symbol(Symbol::$sym) {
            Ok(true) => {}
            Ok(false) => return Err(Error::msg($self.lexer.loc(), $msg)),
            Err(Error::EOF) => return Err(Error::msg($self.lexer.loc(), $msg)),
            Err(e) => return Err(e),
        }
    }};
}

macro_rules! lexer {
    ($self:ident, $($e:tt)*) => {{
        let loc = $self.lexer.last_loc();
        match $self.lexer.$($e)* {
            Ok(node) => node,
            Err(Error::EOF) => return Err(Error::msg(loc, "unexpected EOF")),
            Err(e) => return Err(e)
        }
    }};
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Parser<'a> {
        Self {
            lexer,
            env: Env::new(),
            tags: Env::new(),
            compound_types: CompoundTypes::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>> {
        let mut nodes = vec![];
        loop {
            if matches!(self.lexer.peek_token(), Err(Error::EOF)) {
                break;
            }
            nodes.push(self.read_toplevel()?);
        }
        Ok(nodes)
    }

    pub fn parse_as_expr(&mut self) -> Result<AST> {
        self.read_expr()
    }

    fn read_toplevel(&mut self) -> Result<AST> {
        let loc = self.lexer.loc();
        let mut nodes = vec![];
        if self.is_func_def()? {
            nodes.push(self.read_func_def()?)
        } else {
            self.read_decl(&mut nodes)?
        }
        Ok(AST::new(ast::Kind::Block(nodes), loc))
    }

    fn is_func_def(&mut self) -> Result<bool> {
        let mut buf = vec![];
        let mut is_func_def = false;

        loop {
            let mut tok = lexer!(self, get_token());
            buf.push(tok.clone());

            if tok.kind == token::Kind::Symbol(Symbol::Semicolon) {
                break;
            }

            if self.is_type(&tok) {
                continue;
            }

            if tok.kind == token::Kind::Symbol(Symbol::OpeningParen) {
                self.skip_parens(&mut buf)?;
                continue;
            }

            if !tok.kind.is_identifier() {
                continue;
            }

            if lexer!(self, peek_token()).kind != token::Kind::Symbol(Symbol::OpeningParen) {
                continue;
            }

            let opening_paren = lexer!(self, get_token());
            buf.push(opening_paren);
            self.skip_parens(&mut buf)?;

            tok = lexer!(self, peek_token());
            is_func_def = tok.kind == token::Kind::Symbol(Symbol::OpeningBrace);
            break;
        }

        self.lexer.unget_all(buf);
        Ok(is_func_def)
    }

    fn skip_parens(&mut self, buf: &mut Vec<token::Token>) -> Result<()> {
        loop {
            let loc = self.lexer.loc();
            let tok = self.lexer.get_token().or_else(|e| match e {
                Error::EOF => Err(Error::msg(loc, "expected ')', but reached EOF")),
                e => Err(e),
            })?;
            buf.push(tok.clone());
            match tok.kind {
                token::Kind::Symbol(Symbol::OpeningParen) => self.skip_parens(buf)?,
                token::Kind::Symbol(Symbol::ClosingParen) => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn read_func_def(&mut self) -> Result<AST> {
        self.env.push();
        self.tags.push();

        let loc = self.lexer.loc();
        let (ret_ty, _, _) = self.read_type_spec()?;
        let (func_ty, name, param_names) = self.read_declarator(ret_ty)?;

        self.env.add_global(
            name.clone(),
            AST::new(ast::Kind::Variable(func_ty, name.clone()), loc),
        );
        self.env.add(
            "__func__".to_string(),
            AST::new(ast::Kind::String(name.clone()), loc),
        );

        expect_symbol_error!(self, OpeningBrace, "expected '('");
        let body = Box::new(self.read_func_body()?);

        self.env.pop();
        self.tags.pop();

        Ok(AST::new(
            ast::Kind::FuncDef {
                ty: func_ty,
                name,
                param_names,
                body,
            },
            loc,
        ))
    }

    fn read_func_body(&mut self) -> Result<AST> {
        self.read_block()
    }

    fn read_block(&mut self) -> Result<AST> {
        let mut stmts = vec![];
        let loc = self.lexer.loc();
        loop {
            let loc = self.lexer.loc();
            if self
                .lexer
                .skip_symbol(Symbol::ClosingBrace)
                .or_else(|e| match e {
                    Error::EOF => Err(Error::msg(loc, "expected '}'")),
                    e => Err(e),
                })?
            {
                break;
            }
            let peek = lexer!(self, peek_token());
            if self.is_type(&peek) {
                self.read_decl(&mut stmts)?;
            } else {
                stmts.push(self.read_stmt()?);
            }
        }
        Ok(AST::new(ast::Kind::Block(stmts), loc))
    }

    fn read_stmt(&mut self) -> Result<AST> {
        let tok = lexer!(self, get_token());
        match tok.kind {
            token::Kind::Keyword(Keyword::If) => return self.read_if_stmt(),
            token::Kind::Keyword(Keyword::While) => return self.read_while_stmt(),
            token::Kind::Keyword(Keyword::For) => return self.read_for_stmt(),
            token::Kind::Keyword(Keyword::Return) => return self.read_return_stmt(),
            token::Kind::Symbol(Symbol::OpeningBrace) => return self.read_block(),
            _ => {}
        }
        if tok.kind.is_identifier()
            && lexer!(self, peek_token()).kind == token::Kind::Symbol(Symbol::Colon)
        {
            todo!();
            // return self.read_label();
        }

        self.lexer.unget(tok);
        let expr = self.read_opt_expr();
        expect_symbol_error!(self, Semicolon, "expected ';'");
        expr
    }

    fn read_if_stmt(&mut self) -> Result<AST> {
        let loc = self.lexer.loc();
        expect_symbol_error!(self, OpeningParen, "expected '('");
        let cond = Box::new(self.read_expr()?);
        expect_symbol_error!(self, ClosingParen, "expected ')'");
        let then_ = Box::new(self.read_stmt()?);
        let else_ = if lexer!(self, skip_keyword(Keyword::Else)) {
            Box::new(self.read_stmt()?)
        } else {
            Box::new(AST::new(ast::Kind::Block(vec![]), self.lexer.loc()))
        };
        Ok(AST::new(ast::Kind::If { cond, then_, else_ }, loc))
    }

    fn read_while_stmt(&mut self) -> Result<AST> {
        let loc = self.lexer.loc();
        expect_symbol_error!(self, OpeningParen, "expected '('");
        let cond = Box::new(self.read_expr()?);
        expect_symbol_error!(self, ClosingParen, "expected ')'");
        let body = Box::new(self.read_stmt()?);
        Ok(AST::new(ast::Kind::While { cond, body }, loc))
    }

    fn read_for_stmt(&mut self) -> Result<AST> {
        lexer!(self, expect_skip_symbol(Symbol::OpeningParen));
        let init = Box::new(self.read_opt_decl_or_stmt()?);
        let cond = self.read_opt_expr()?;
        let cond = match cond.kind {
            ast::Kind::Block(_) => None,
            _ => Some(Box::new(cond)),
        };
        lexer!(self, expect_skip_symbol(Symbol::Semicolon));
        let step = if lexer!(self, peek_token()).kind == token::Kind::Symbol(Symbol::ClosingParen) {
            Box::new(AST::new(ast::Kind::Block(vec![]), self.lexer.loc()))
        } else {
            Box::new(self.read_opt_expr()?)
        };
        lexer!(self, expect_skip_symbol(Symbol::ClosingParen));
        let body = Box::new(self.read_stmt()?);
        Ok(AST::new(
            ast::Kind::For {
                init,
                cond,
                step,
                body,
            },
            self.lexer.loc(),
        ))
    }

    fn read_return_stmt(&mut self) -> Result<AST> {
        let loc = self.lexer.loc();
        if lexer!(self, skip_symbol(Symbol::Semicolon)) {
            Ok(AST::new(ast::Kind::Return(None), loc))
        } else {
            let val = Some(Box::new(self.read_expr()?));
            expect_symbol_error!(self, Semicolon, "expected ';'");
            Ok(AST::new(ast::Kind::Return(val), loc))
        }
    }

    fn read_opt_decl_or_stmt(&mut self) -> Result<AST> {
        if lexer!(self, skip_symbol(Symbol::Semicolon)) {
            return Ok(AST::new(ast::Kind::Block(vec![]), self.lexer.loc()));
        }

        let peek = lexer!(self, peek_token());
        if self.is_type(&peek) {
            let mut stmts = vec![];
            self.read_decl(&mut stmts)?;
            return Ok(AST::new(ast::Kind::Block(stmts), peek.loc));
        } else {
            self.read_stmt()
        }
    }

    fn read_decl(&mut self, stmts: &mut Vec<AST>) -> Result<()> {
        let (base, sclass, qual) = self.read_type_spec()?;
        let is_typedef = sclass == StorageClass::Typedef;

        if lexer!(self, skip_symbol(Symbol::Semicolon)) {
            return Ok(());
        }

        loop {
            let (mut ty, name, _) = self.read_declarator(base)?;

            if (qual.constexpr || qual.const_) && lexer!(self, skip_symbol(Symbol::Assign)) {
                let init = self.read_decl_init(&mut ty)?;
                self.env.add(name.clone(), init);
            } else {
                if is_typedef {
                    let typedef = AST::new(ast::Kind::Typedef(ty, name.clone()), self.lexer.loc());
                    self.env.add(name, typedef);
                    return Ok(());
                }
                let init = if lexer!(self, skip_symbol(Symbol::Assign)) {
                    Some(Box::new(self.read_decl_init(&mut ty)?))
                } else {
                    None
                };
                self.env.add(
                    name.clone(),
                    AST::new(ast::Kind::Variable(ty, name.clone()), self.lexer.loc()),
                );
                stmts.push(AST::new(
                    ast::Kind::VariableDecl(ty, name, sclass, init),
                    self.lexer.loc(),
                ))
            }

            if lexer!(self, skip_symbol(Symbol::Semicolon)) {
                return Ok(());
            }

            let loc = self.lexer.loc();
            if !lexer!(self, skip_symbol(Symbol::Comma)) {
                return Err(Error::msg(loc, "expected ','"));
            }
        }
    }

    fn read_decl_init(&mut self, ty: &mut Type) -> Result<AST> {
        if lexer!(self, peek_token()).kind == token::Kind::Symbol(Symbol::OpeningBrace) {
            return self.read_initializer_list(ty);
        } else if self.is_string(ty) {
            let tok = lexer!(self, get_token());
            if let token::Kind::String(s) = tok.kind {
                return self.read_string_initializer(ty, s);
            }
            self.lexer.unget(tok)
        }
        self.read_assign()
    }

    fn is_string(&self, ty: &Type) -> bool {
        if let &Type::Array(id) = ty {
            return matches!(
                self.compound_types[id].as_array().0,
                Type::Char(Sign::Signed)
            );
        }
        false
    }

    fn read_string_initializer(&mut self, _ty: &mut Type, _s: String) -> Result<AST> {
        todo!()
    }

    fn read_initializer_list(&mut self, _ty: &mut Type) -> Result<AST> {
        todo!()
    }

    fn read_declarator(&mut self, base: Type) -> Result<(Type, String, Vec<String>)> {
        if lexer!(self, skip_symbol(Symbol::OpeningParen)) {
            let peek = lexer!(self, peek_token());
            if self.is_type(&peek) {
                let (ty, params) = self.read_declarator_func(base)?;
                return Ok((ty, "".to_string(), params));
            }

            let mut buf = vec![];
            while !lexer!(self, skip_symbol(Symbol::ClosingParen)) {
                buf.push(lexer!(self, get_token()));
            }
            let (base, _) = self.read_declarator_tail(base)?;
            self.lexer.unget_all(buf);
            return self.read_declarator(base);
        }

        if lexer!(self, skip_symbol(Symbol::Asterisk)) {
            self.skip_type_qualifiers()?;
            let ptr = self.compound_types.pointer(base);
            return self.read_declarator(ptr);
        }

        let tok = lexer!(self, get_token());

        if let token::Kind::Identifier(ref name) = &tok.kind {
            let (ty, params) = self.read_declarator_tail(base)?;
            return Ok((ty, name.clone(), params));
        }

        self.lexer.unget(tok);
        let (ty, params) = self.read_declarator_tail(base)?;
        Ok((ty, "".to_string(), params))
    }

    fn read_declarator_func(&mut self, base: Type) -> Result<(Type, Vec<String>)> {
        if lexer!(self, peek_token()).kind == token::Kind::Keyword(Keyword::Void)
            && lexer!(self, peek2_token()).kind == token::Kind::Symbol(Symbol::ClosingParen)
        {
            lexer!(self, expect_skip_keyword(Keyword::Void));
            lexer!(self, expect_skip_symbol(Symbol::ClosingParen));
            return Ok((self.compound_types.func(base, vec![], false), vec![]));
        }

        if lexer!(self, skip_symbol(Symbol::ClosingParen)) {
            return Ok((self.compound_types.func(base, vec![], false), vec![]));
        }

        let (types, names, vararg) = self.read_declarator_params()?;
        Ok((self.compound_types.func(base, types, vararg), names))
    }

    fn read_declarator_tail(&mut self, base: Type) -> Result<(Type, Vec<String>)> {
        if lexer!(self, skip_symbol(Symbol::OpeningBoxBracket)) {
            return Ok((self.read_declarator_array(base)?, vec![]));
        }

        if lexer!(self, skip_symbol(Symbol::OpeningParen)) {
            return self.read_declarator_func(base);
        }

        Ok((base, vec![]))
    }

    fn read_declarator_array(&mut self, base: Type) -> Result<Type> {
        let len: i32 = if lexer!(self, skip_symbol(Symbol::ClosingBoxBracket)) {
            -1
        } else {
            let loc = self.lexer.loc();
            let len = match self.read_expr()?.eval() {
                Some(len) => len as i32,
                None => return Err(Error::msg(loc, "array size must be constant")),
            };
            expect_symbol_error!(self, ClosingBoxBracket, "expected ']'");
            len
        };
        let inner = self.read_declarator_tail(base)?.0;
        Ok(self.compound_types.array(inner, len))
    }

    fn read_declarator_params(&mut self) -> Result<(Vec<Type>, Vec<String>, bool)> {
        let mut types = vec![];
        let mut names = vec![];
        loop {
            if lexer!(self, skip_symbol(Symbol::Vararg)) {
                if types.len() == 0 {
                    return Err(Error::msg(
                        self.lexer.loc(),
                        "at least one parameter is required before '...'",
                    ));
                }
                expect_symbol_error!(self, ClosingParen, "expected ')'");
                return Ok((types, names, true));
            }

            let loc = self.lexer.loc();
            let (ty, name) = self.read_func_param()?;

            // if reading a parameter of a function to be defined
            if self.env.is_local() {
                self.env.add(
                    name.clone(),
                    AST::new(ast::Kind::Variable(ty, name.clone()), loc),
                )
            }

            types.push(ty);
            names.push(name);

            if lexer!(self, skip_symbol(Symbol::ClosingParen)) {
                return Ok((types, names, false));
            }

            if !lexer!(self, skip_symbol(Symbol::Comma)) {
                return Err(Error::msg(self.lexer.loc(), "expected ','"));
            }
        }
    }

    fn read_func_param(&mut self) -> Result<(Type, String)> {
        let base = self.read_type_spec()?.0;
        let (ty, name, _) = self.read_declarator(base)?;
        match ty {
            Type::Array(x) => {
                let inner = self.compound_types[x].as_array().0;
                Ok((self.compound_types.pointer(inner), name))
            }
            Type::Func(_) => Ok((self.compound_types.pointer(ty), name)),
            _ => Ok((ty, name)),
        }
    }

    fn skip_type_qualifiers(&mut self) -> Result<()> {
        while lexer!(self, skip_keyword(Keyword::Const))
            || lexer!(self, skip_keyword(Keyword::Volatile))
            || lexer!(self, skip_keyword(Keyword::Restrict))
        {}
        Ok(())
    }

    fn read_type_spec(&mut self) -> Result<(Type, StorageClass, Qualifiers)> {
        #[derive(PartialEq, Debug, Clone)]
        enum Size {
            Short,
            Normal,
            Long,
            LLong,
        };
        #[derive(PartialEq, Debug, Clone)]
        enum PrimitiveType {
            Void,
            Char,
            Int,
            Float,
            Double,
        };

        let mut kind: Option<PrimitiveType> = None;
        let mut sign = Sign::Signed;
        let mut size = Size::Normal;
        let mut sclass = StorageClass::Auto;
        let mut userty: Option<Type> = None;
        let mut qual = Qualifiers::new();

        loop {
            let tok = lexer!(self, get_token());

            if kind.is_none() {
                if let &token::Kind::Identifier(ref maybe_userty_name) = &tok.kind {
                    let maybe_userty = self.get_typedef(maybe_userty_name.as_str());
                    if let Some(userty) = maybe_userty {
                        return Ok((userty, sclass, qual));
                    }
                }
            }

            if !tok.kind.is_keyword() {
                self.lexer.unget(tok);
                break;
            }

            match tok.kind {
                token::Kind::Keyword(Keyword::Typedef) => sclass = StorageClass::Typedef,
                token::Kind::Keyword(Keyword::Extern) => sclass = StorageClass::Extern,
                token::Kind::Keyword(Keyword::Static) => sclass = StorageClass::Static,
                token::Kind::Keyword(Keyword::Auto) => sclass = StorageClass::Auto,
                token::Kind::Keyword(Keyword::Register) => sclass = StorageClass::Register,
                token::Kind::Keyword(Keyword::Const) => qual.const_ = true,
                token::Kind::Keyword(Keyword::ConstExpr) => qual.constexpr = true,
                token::Kind::Keyword(Keyword::Volatile) => qual.volatile = true,
                token::Kind::Keyword(Keyword::Inline) => qual.inline = true,
                token::Kind::Keyword(Keyword::Restrict) => qual.restrict = true,
                token::Kind::Keyword(Keyword::Noreturn) => qual.noreturn = true,
                token::Kind::Keyword(Keyword::Void)
                | token::Kind::Keyword(Keyword::Char)
                | token::Kind::Keyword(Keyword::Int)
                | token::Kind::Keyword(Keyword::Float)
                | token::Kind::Keyword(Keyword::Double)
                | token::Kind::Keyword(Keyword::Signed)
                | token::Kind::Keyword(Keyword::Unsigned)
                    if kind.is_some() =>
                {
                    return Err(Error::msg(tok.loc, "type mismatch"))
                }
                token::Kind::Keyword(Keyword::Void) => kind = Some(PrimitiveType::Void),
                token::Kind::Keyword(Keyword::Char) => kind = Some(PrimitiveType::Char),
                token::Kind::Keyword(Keyword::Int) => kind = Some(PrimitiveType::Int),
                token::Kind::Keyword(Keyword::Float) => kind = Some(PrimitiveType::Float),
                token::Kind::Keyword(Keyword::Double) => kind = Some(PrimitiveType::Double),
                token::Kind::Keyword(Keyword::Signed) => sign = Sign::Signed,
                token::Kind::Keyword(Keyword::Unsigned) => sign = Sign::Unsigned,
                token::Kind::Keyword(Keyword::Short) => size = Size::Short,
                token::Kind::Keyword(Keyword::Long) if size == Size::Normal => size = Size::Long,
                token::Kind::Keyword(Keyword::Long) if size == Size::Long => size = Size::LLong,
                token::Kind::Keyword(Keyword::Struct) => userty = Some(self.read_struct_def()?),
                token::Kind::Keyword(Keyword::Union) => userty = Some(self.read_union_def()?),
                token::Kind::Keyword(Keyword::Enum) => userty = Some(self.read_enum_def()?),
                token::Kind::Keyword(_) => {}
                _ => self.lexer.unget(tok),
            }
        }

        if let Some(userty) = userty {
            return Ok((userty, sclass, qual));
        }

        if let Some(kind) = kind {
            match kind {
                PrimitiveType::Void => return Ok((Type::Void, sclass, qual)),
                PrimitiveType::Char => return Ok((Type::Char(sign), sclass, qual)),
                PrimitiveType::Float => return Ok((Type::Float, sclass, qual)),
                PrimitiveType::Double => return Ok((Type::Double, sclass, qual)),
                _ => {}
            }
        }

        let ty = match size {
            Size::Short => Type::Short(sign),
            Size::Normal => Type::Int(sign),
            Size::Long => Type::Long(sign),
            Size::LLong => Type::LLong(sign),
        };

        Ok((ty, sclass, qual))
    }

    fn read_struct_def(&mut self) -> Result<Type> {
        self.read_rectype_def(true)
    }

    fn read_union_def(&mut self) -> Result<Type> {
        self.read_rectype_def(false)
    }

    fn read_enum_def(&mut self) -> Result<Type> {
        todo!()
    }

    fn read_rectype_def(&mut self, is_struct: bool) -> Result<Type> {
        let tag = self.read_rectype_tag()?.unwrap_or_else(|| unique_string(8));
        let fields = self.read_rectype_fields()?;
        let cur_tags = self.tags.back_mut().unwrap();
        if fields.is_empty() {
            Ok(match cur_tags.entry(tag) {
                hash_map::Entry::Occupied(o) => *o.get(),
                hash_map::Entry::Vacant(v) => {
                    let new_rectype = if is_struct {
                        self.compound_types.struct_(v.key().to_string(), vec![])
                    } else {
                        self.compound_types.union(v.key().to_string(), vec![])
                    };
                    *v.insert(new_rectype)
                }
            })
        } else {
            Ok(match cur_tags.entry(tag) {
                hash_map::Entry::Occupied(o) => {
                    match &mut self.compound_types[*o.get()] {
                        CompoundType::Struct { fields: f, .. }
                        | CompoundType::Union { fields: f, .. } => {
                            *f = fields;
                        }
                        _ => panic!(),
                    };
                    *o.get()
                }
                hash_map::Entry::Vacant(v) => {
                    let new_rectype = if is_struct {
                        self.compound_types.struct_(v.key().to_string(), fields)
                    } else {
                        self.compound_types.union(v.key().to_string(), fields)
                    };
                    *v.insert(new_rectype)
                }
            })
        }
    }

    fn read_rectype_tag(&mut self) -> Result<Option<String>> {
        if let token::Kind::Identifier(name) = lexer!(self, peek_token()).kind {
            lexer!(self, get_token());
            return Ok(Some(name));
        }
        Ok(None)
    }

    fn read_rectype_fields(&mut self) -> Result<Vec<(Type, String)>> {
        if !lexer!(self, skip_symbol(Symbol::OpeningBrace)) {
            return Ok(vec![]);
        }

        let mut decls = vec![];
        loop {
            let peek = lexer!(self, peek_token());
            if !self.is_type(&peek) {
                break;
            }
            let (base, _, _) = self.read_type_spec()?;
            loop {
                let (ty, name, _) = self.read_declarator(base)?;
                if lexer!(self, skip_symbol(Symbol::Colon)) {
                    // TODO: ignore bitwidth for now
                    self.read_expr()?;
                }
                decls.push((ty, name));
                if lexer!(self, skip_symbol(Symbol::Comma)) {
                    continue;
                } else {
                    lexer!(self, expect_skip_symbol(Symbol::Semicolon));
                }
                break;
            }
        }

        lexer!(self, expect_skip_symbol(Symbol::ClosingBrace));

        Ok(decls)
    }

    fn read_expr(&mut self) -> Result<AST> {
        self.read_comma()
    }

    fn read_opt_expr(&mut self) -> Result<AST> {
        if lexer!(self, peek_token()).kind == token::Kind::Symbol(Symbol::Semicolon) {
            Ok(AST::new(ast::Kind::Block(vec![]), self.lexer.loc()))
        } else {
            self.read_expr()
        }
    }

    fn read_comma(&mut self) -> Result<AST> {
        let mut lhs = self.read_assign()?;
        while lexer!(self, skip_symbol(Symbol::Comma)) {
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
        if lexer!(self, skip_symbol(Symbol::Question)) {
            return self.read_ternary(lhs);
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
            let tok = lexer!(self, get_token());
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

    fn read_ternary(&mut self, cond: AST) -> Result<AST> {
        let then_ = self.read_expr()?;
        let loc = then_.loc;
        lexer!(self, expect_skip_symbol(Symbol::Colon));
        let else_ = self.read_expr()?;
        // let then_ty = try!(self.get_expr_returning_ty(&then_expr));
        // let else_ty = try!(self.get_expr_returning_ty(&else_expr));
        // if then_ty.is_arith_ty() && else_ty.is_arith_ty() {
        //     let ty = self.usual_binary_ty_cov(then_ty, else_ty);
        //     then_expr = self.cast_ast(&then_expr, &ty);
        //     else_expr = self.cast_ast(&else_expr, &ty);
        // }
        Ok(AST::new(
            ast::Kind::TernaryOp(Box::new(cond), Box::new(then_), Box::new(else_)),
            loc,
        ))
    }

    fn read_logical_or(&mut self) -> Result<AST> {
        let mut lhs = self.read_logical_and()?;
        while lexer!(self, skip_symbol(Symbol::LOr)) {
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
        while lexer!(self, skip_symbol(Symbol::LAnd)) {
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
        while lexer!(self, skip_symbol(Symbol::Or)) {
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
        while lexer!(self, skip_symbol(Symbol::Xor)) {
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
        while lexer!(self, skip_symbol(Symbol::Ampersand)) {
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
            if lexer!(self, skip_symbol(Symbol::Eq)) {
                let rhs = self.read_primary()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Eq, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Ne)) {
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
            if lexer!(self, skip_symbol(Symbol::Lt)) {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Lt, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Le)) {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Le, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Gt)) {
                let rhs = self.read_shl_shr()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Gt, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Ge)) {
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
            if lexer!(self, skip_symbol(Symbol::Shl)) {
                let rhs = self.read_add_sub()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Shl, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Shr)) {
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
            if lexer!(self, skip_symbol(Symbol::Add)) {
                let rhs = self.read_mul_div_rem()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Add, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Sub)) {
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
            if lexer!(self, skip_symbol(Symbol::Asterisk)) {
                let rhs = self.read_cast()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Mul, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Div)) {
                let rhs = self.read_cast()?;
                lhs = AST::new(
                    ast::Kind::BinaryOp(ast::BinaryOp::Div, Box::new(lhs), Box::new(rhs)),
                    self.lexer.loc(),
                );
            } else if lexer!(self, skip_symbol(Symbol::Mod)) {
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
        let tok = lexer!(self, get_token());
        let peek = lexer!(self, peek_token());
        if tok.kind == token::Kind::Symbol(Symbol::OpeningParen) && self.is_type(&peek) {
            let basety = self.read_type_spec()?.0;
            let ty = self.read_declarator(basety)?.0;
            lexer!(self, expect_skip_symbol(Symbol::ClosingParen));
            return Ok(AST::new(
                ast::Kind::TypeCast(Box::new(self.read_cast()?), ty),
                tok.loc,
            ));
        } else {
            self.lexer.unget(tok);
        }

        self.read_unary()
    }

    fn read_unary(&mut self) -> Result<AST> {
        let tok = lexer!(self, get_token());

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
                return self.read_sizeof();
            }
            _ => {}
        }

        self.lexer.unget(tok);
        self.read_postfix()
    }

    fn read_sizeof(&mut self) -> Result<AST> {
        let tok = lexer!(self, get_token());
        let peek = lexer!(self, peek_token());
        if matches!(tok.kind, token::Kind::Symbol(Symbol::OpeningParen)) && self.is_type(&peek) {
            let (basety, _, _) = self.read_type_spec()?;
            let (_ty, _, _) = self.read_declarator(basety)?;
            lexer!(self, expect_skip_symbol(Symbol::ClosingParen));
            // TODO: calc type size
            return Ok(AST::new(ast::Kind::Int { n: 1, bits: 32 }, tok.loc));
        }
        self.lexer.unget(tok);
        let _expr = self.read_unary()?;
        // TODO: calc type size
        Ok(AST::new(ast::Kind::Int { n: 1, bits: 32 }, peek.loc))
    }

    fn read_postfix(&mut self) -> Result<AST> {
        let mut ast = self.read_primary()?;
        loop {
            if lexer!(self, skip_symbol(Symbol::OpeningParen)) {
                ast = self.read_func_call(ast)?;
                continue;
            }
            if lexer!(self, skip_symbol(Symbol::OpeningBoxBracket)) {
                ast = AST::new(
                    ast::Kind::Load(Box::new(self.read_index(ast)?)),
                    self.lexer.loc(),
                );
                continue;
            }
            if lexer!(self, skip_symbol(Symbol::Point)) {
                ast = AST::new(
                    ast::Kind::Load(Box::new(self.read_field(ast)?)),
                    self.lexer.loc(),
                );
                continue;
            }
            if lexer!(self, skip_symbol(Symbol::Arrow)) {
                let loc = self.lexer.loc();
                let field = self.read_field(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::Deref, Box::new(ast)),
                    loc,
                ))?;
                ast = AST::new(ast::Kind::Load(Box::new(field)), loc);
                continue;
            }
            if lexer!(self, skip_symbol(Symbol::Inc)) {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::PostInc, Box::new(ast)),
                    self.lexer.loc(),
                ));
            }
            if lexer!(self, skip_symbol(Symbol::Dec)) {
                return Ok(AST::new(
                    ast::Kind::UnaryOp(ast::UnaryOp::PostDec, Box::new(ast)),
                    self.lexer.loc(),
                ));
            }
            break;
        }
        Ok(ast)
    }

    fn read_func_call(&mut self, f: AST) -> Result<AST> {
        let loc = f.loc;
        let mut args = vec![];
        if !self.lexer.skip_symbol(Symbol::ClosingParen)? {
            loop {
                args.push(self.read_assign()?);
                if self.lexer.skip_symbol(Symbol::ClosingParen)? {
                    break;
                }
                self.lexer.expect_skip_symbol(Symbol::Comma)?;
            }
        }
        Ok(AST::new(ast::Kind::FuncCall(Box::new(f), args), loc))
    }

    fn read_field(&mut self, base: AST) -> Result<AST> {
        let field = lexer!(self, get_token());
        if !matches!(field.kind, token::Kind::Identifier(_)) {
            return Err(Error::msg(field.loc, "expected field name"));
        }
        let name = retrieve_ident!(field);
        Ok(AST::new(
            ast::Kind::FieldRef(Box::new(base), name),
            field.loc,
        ))
    }

    fn read_index(&mut self, base: AST) -> Result<AST> {
        let loc = base.loc;
        let idx = self.read_expr()?;
        self.lexer.expect_skip_symbol(Symbol::ClosingBoxBracket)?;
        Ok(AST::new(
            ast::Kind::BinaryOp(ast::BinaryOp::Add, Box::new(base), Box::new(idx)),
            loc,
        ))
    }

    fn read_primary(&mut self) -> Result<AST> {
        let tok = lexer!(self, get_token());

        match tok.kind {
            token::Kind::Int { n, bits } => Ok(AST::new(ast::Kind::Int { n, bits }, tok.loc)),
            token::Kind::Float(f) => Ok(AST::new(ast::Kind::Float(f), tok.loc)),
            token::Kind::Identifier(ident) => {
                if let Some(v) = self.env.get(ident.as_str()) {
                    return match v.kind {
                        ast::Kind::Variable(_, _) => Ok(AST::new(
                            ast::Kind::Load(Box::new(v.clone())),
                            self.lexer.loc(),
                        )),
                        _ => Ok(v.clone()),
                    };
                }
                Err(Error::Message(
                    tok.loc,
                    format!("variable not found '{}'", ident),
                ))
            }
            token::Kind::String(s) => Ok(AST::new(ast::Kind::String(s), tok.loc)),
            token::Kind::Char(c) => Ok(AST::new(ast::Kind::Char(c), tok.loc)),
            token::Kind::Symbol(s) => match s {
                Symbol::OpeningParen => {
                    let expr = self.read_expr()?;
                    lexer!(self, expect_skip_symbol(Symbol::ClosingParen));
                    Ok(expr)
                }
                _ => Err(Error::Message(
                    tok.loc,
                    format!("expected expression, but got {:?}", tok.kind),
                )),
            },
            _ => Err(Error::Message(
                tok.loc,
                format!("unknown token {:?}", tok.kind),
            )),
        }
    }

    fn get_typedef(&self, name: &str) -> Option<Type> {
        match self.env.get(name) {
            Some(ast) => match ast.kind {
                ast::Kind::Typedef(ref from, ref _to) => {
                    let ty = match from {
                        &Type::Union(_) => todo!(),
                        &Type::Struct(id) => {
                            let (name, fields) = self.compound_types[id].as_struct();
                            if fields.len() == 0 {
                                self.tags.get(name.as_str()).unwrap().clone()
                            } else {
                                *from
                            }
                        }
                        _ => *from,
                    };
                    return Some(ty);
                }
                _ => None,
            },
            None => None,
        }
    }

    fn is_type(&self, tok: &token::Token) -> bool {
        match tok.kind {
            token::Kind::Keyword(token::Keyword::Typedef)
            | token::Kind::Keyword(token::Keyword::Extern)
            | token::Kind::Keyword(token::Keyword::Static)
            | token::Kind::Keyword(token::Keyword::Auto)
            | token::Kind::Keyword(token::Keyword::Register)
            | token::Kind::Keyword(token::Keyword::Const)
            | token::Kind::Keyword(token::Keyword::Volatile)
            | token::Kind::Keyword(token::Keyword::Void)
            | token::Kind::Keyword(token::Keyword::Signed)
            | token::Kind::Keyword(token::Keyword::Unsigned)
            | token::Kind::Keyword(token::Keyword::Char)
            | token::Kind::Keyword(token::Keyword::Int)
            | token::Kind::Keyword(token::Keyword::Short)
            | token::Kind::Keyword(token::Keyword::Long)
            | token::Kind::Keyword(token::Keyword::Float)
            | token::Kind::Keyword(token::Keyword::Double)
            | token::Kind::Keyword(token::Keyword::Struct)
            | token::Kind::Keyword(token::Keyword::Enum)
            | token::Kind::Keyword(token::Keyword::Union)
            | token::Kind::Keyword(token::Keyword::Noreturn)
            | token::Kind::Keyword(token::Keyword::Inline)
            | token::Kind::Keyword(token::Keyword::Restrict) => true,
            token::Kind::Identifier(ref ident) => self
                .env
                .get(ident.as_str())
                .map_or(false, |x| matches!(x.kind, ast::Kind::Typedef(_, _))),
            _ => false,
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

    pub fn get(&self, name: &str) -> Option<&T> {
        self.0.back().unwrap().get(name)
    }

    pub fn contains(&mut self, name: &str) -> bool {
        self.0.back_mut().unwrap().contains_key(name)
    }
}

impl Qualifiers {
    pub fn new() -> Qualifiers {
        Qualifiers {
            restrict: false,
            const_: false,
            constexpr: false,
            volatile: false,
            inline: false,
            noreturn: false,
        }
    }
}

fn unique_string(len: usize) -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            0123456789";
    let mut rng = rand::thread_rng();
    (0..len)
        .map(|_| CHARSET[rng.gen_range(0, CHARSET.len())] as char)
        .collect()
}
