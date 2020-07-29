#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
    pub structs: Vec<(String, Vec<(String, Type)>)>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret_ty: Type,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub enum Node {
    Number(i32),
    FPNumber(f64),
    Identifier(String),
    VarDecl(String, Type), // name, type
    Eq(Box<Node>, Box<Node>),
    // Ne(Box<Node>, Box<Node>),
    Lt(Box<Node>, Box<Node>),
    Le(Box<Node>, Box<Node>),
    // Gt(Box<Node>, Box<Node>),
    // Ge(Box<Node>, Box<Node>),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Rem(Box<Node>, Box<Node>),
    IfElse(Box<Node>, Vec<Node>, Option<Vec<Node>>),
    WhileLoop(Box<Node>, Vec<Node>),
    Call(String, Vec<Node>),
    Return(Box<Node>),
    Dot(Box<Node>, Box<Node>),
    Index(Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Load(Box<Node>),
    Addr(Box<Node>),
    TypeCast(Box<Node>, Type),
    // GlobalDataAddr(String),
}

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int1,
    Int32,
    Int64,
    F64,
    Pointer(Box<Type>),
    Array(usize, Box<Type>),
    Struct(String),
}

enum ModuleItem {
    Function(Function),
    StructDecl(String, Vec<(String, Type)>),
}

peg::parser!(pub grammar parser() for str {
    pub rule module() -> Module
        = items:((_ i:module_item() _ { i })*) {
            let mut functions = vec![];
            let mut structs = vec![];
            for item in items {
                match item {
                    ModuleItem::Function(f) => functions.push(f),
                    ModuleItem::StructDecl(name, decls) => structs.push((name, decls)),
                }
            }
            Module { functions, structs }
        }

    rule module_item() -> ModuleItem
        = f:function()    { ModuleItem::Function(f) }
        / s:struct_decl() { ModuleItem::StructDecl(s.0, s.1) }

    rule struct_decl() -> (String, Vec<(String, Type)>)
        = "struct" _ name:identifier() _
        "{" _
        d:((_ name:identifier() _ ":" _ ty:types() _ {(name, ty)}) ** ",") _
        "}" _
        { (name, d) }

    pub rule function() -> Function // (String, Vec<(String, String)>, String, Vec<Node>)
        = [' ' | '\t' | '\n']* "function" _ name:identifier() _
        "(" params:((_ i:identifier() _ ":" _ t:types() _ {(i, t)}) ** ",") ")" _
        ":" _
        ret_ty:types() _
        "{" _
        body:statements()
        _ "}" _
        { Function { name, params, ret_ty, body } }

    rule statements() -> Vec<Node>
        = s:((s:statement() _ {s})*) { s }

    rule statement() -> Node
        = var_decl()
        / if_else()
        / while_loop()
        / return_stmt()
        / assignment()
        / e:expression(false) _ ";" { e }

    rule expression(assign: bool) -> Node
        = binary_op(assign)

    rule var_decl() -> Node
         = "var" _ name:identifier() _ ":" _ ty:types() _ ";" { Node::VarDecl(name, ty) }

    rule if_else() -> Node
        = "if" _ e:expression(false) _ "{" _
        then_body:statements() _ "}" _
        else_body:("else" _ "{" _ e:statements() _ "}" { e })?
        { Node::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Node
        = "while" _ e:expression(false) _ "{" _
        loop_body:statements() _ "}"
        { Node::WhileLoop(Box::new(e), loop_body) }

    rule assignment() -> Node
        = p:primary(true) _ "=" _ e:expression(false) _ ";" {Node::Assign(Box::new(p), Box::new(e))}

    rule binary_op(assign:bool) -> Node = precedence!{
        a:(@) _ "==" _ b:@ { Node::Eq(Box::new(a), Box::new(b)) }
        // a:@ _ "!=" _ b:(@) { Node::Ne(Box::new(a), Box::new(b)) }
        a:(@) _ "<"  _ b:@ { Node::Lt(Box::new(a), Box::new(b)) }
        a:(@) _ "<=" _ b:@ { Node::Le(Box::new(a), Box::new(b)) }
        // a:@ _ ">"  _ b:(@) { Node::Gt(Box::new(a), Box::new(b)) }
        // a:@ _ ">=" _ b:(@) { Node::Ge(Box::new(a), Box::new(b)) }
        --
        a:(@) _ "+" _ b:@ { Node::Add(Box::new(a), Box::new(b)) }
        a:(@) _ "-" _ b:@ { Node::Sub(Box::new(a), Box::new(b)) }
        --
        a:(@) _ "*" _ b:@ { Node::Mul(Box::new(a), Box::new(b)) }
        a:(@) _ "/" _ b:@ { Node::Div(Box::new(a), Box::new(b)) }
        a:(@) _ "%" _ b:@ { Node::Rem(Box::new(a), Box::new(b)) }
        --
        t:types() _ "(" _ a:expression(false) _ ")" { Node::TypeCast(Box::new(a), t) }
        i:identifier() _ "(" args:((_ e:expression(false) _ {e}) ** ",") ")" { Node::Call(i, args) }
        p:primary(assign) { if assign { p } else { Node::Load(Box::new(p)) } }
        "(" _ a:expression(false) _ ")" { a }
        l:number() { l }
    }

    rule primary(assign: bool) -> Node = precedence! {
        "(" _ a:primary(false) _ ")" { a }
        a:@ _ "[" _ b:expression(false) "]" { Node::Index(Box::new(a), Box::new(b)) }
        a:(@) _ "." _ b:@ { Node::Dot(Box::new(a), Box::new(b)) }
        "*" _ a:(@) { Node::Load(Box::new(a)) }
        "&" _ a:(@) { Node::Addr(Box::new(a)) }
        --
        i:identifier() { Node::Identifier(i) }
    }

    rule return_stmt() -> Node
        = "return" _ e:expression(false) _ ";" { Node::Return(Box::new(e)) }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule number() -> Node
        = n1:$(['0'..='9']+) _ "." _ n2:$(['0'..='9']+) { Node::FPNumber(format!("{}.{}", n1, n2).parse().unwrap()) }
        / n:$(['0'..='9']+) { Node::Number(n.parse().unwrap()) }

    rule number_usize() -> usize
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    rule types() -> Type
        = "i32" { Type::Int32 }
        / "i64" { Type::Int64 }
        / "f64" { Type::F64 }
        / "void" { Type::Void }
        / "struct" _ name:identifier() { Type::Struct(name) }
        / "*" _ t:types() { Type::Pointer(Box::new(t)) }
        / "[" _ s:number_usize() _ "]" _ t:types() { Type::Array(s, Box::new(t)) }

    rule _() =  quiet!{[' ' | '\t' | '\n']*}
});
