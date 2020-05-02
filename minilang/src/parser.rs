#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, String)>, // (name, type)
    pub ret_ty: String,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(String),
    Identifier(String),
    Assign(String, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    // Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    // Gt(Box<Expr>, Box<Expr>),
    // Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    // Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Option<Vec<Expr>>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    Return(Box<Expr>),
    // GlobalDataAddr(String),
}

peg::parser!(pub grammar parser() for str {
    pub rule module() -> Module
        = functions:((f:function() _ { f })*)  { Module { functions } }

    pub rule function() -> Function // (String, Vec<(String, String)>, String, Vec<Expr>)
        = [' ' | '\t' | '\n']* "function" _ name:identifier() _
        "(" params:((_ i:identifier() _ ":" _ t:types() _ {(i, t)}) ** ",") ")" _
        ":" _
        ret_ty:types() _
        "{" _
        body:statements()
        _ "}" _
        { Function { name, params, ret_ty, body } }

    rule statements() -> Vec<Expr>
        = s:((s:statement() _ {s})*) { s }

    rule statement() -> Expr
        = if_else()
        / while_loop()
        / return_stmt()
        / expression()

    rule expression() -> Expr
        = assignment()
        / binary_op()

    rule if_else() -> Expr
        = "if" _ e:expression() _ "{" _
        then_body:statements() _ "}" _
        else_body:("else" _ "{" _ e:statements() _ "}" { e })?
        { Expr::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expr
        = "while" _ e:expression() _ "{" _
        loop_body:statements() _ "}"
        { Expr::WhileLoop(Box::new(e), loop_body) }

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:expression() _ ";" {Expr::Assign(i, Box::new(e))}

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::Eq(Box::new(a), Box::new(b)) }
        // a:@ _ "!=" _ b:(@) { Expr::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expr::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expr::Le(Box::new(a), Box::new(b)) }
        // a:@ _ ">"  _ b:(@) { Expr::Gt(Box::new(a), Box::new(b)) }
        // a:@ _ ">=" _ b:(@) { Expr::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expr::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expr::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expr::Mul(Box::new(a), Box::new(b)) }
        // a:@ _ "/" _ b:(@) { Expr::Div(Box::new(a), Box::new(b)) }
        --
        i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(i, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule return_stmt() -> Expr
        = "return" _ e:expression() _ ";" { Expr::Return(Box::new(e)) }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }
        // / "&" i:identifier() { Expr::GlobalDataAddr(i) }

    rule types() -> String
        = quiet! { t:$("i32" / "i64") { t.to_owned() } }
        / expected!("type")

    rule _() =  quiet!{[' ' | '\t' | '\n']*}
});
