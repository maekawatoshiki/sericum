use crate::parser::{self, Node};
use sericum;
use sericum::ir::{builder::IRBuilder, module};
use std::collections::HashMap;

pub struct CodeGenerator {
    pub module: module::Module,
    types: Types,
}

pub struct CodeGeneratorForFunction<'a> {
    builder: sericum::builder::IRBuilderWithModuleAndFuncId<'a>,
    types: &'a Types,
    func: &'a parser::Function,
    vars: Vec<HashMap<String, (bool, parser::Type, sericum::value::Value)>>, // name, (is_arg, ty, val)
}

pub struct Types {
    records: HashMap<String, (sericum::types::Type, HashMap<String, (usize, parser::Type)>)>,
    functions: HashMap<sericum::function::FunctionId, (parser::Type, Vec<parser::Type>)>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            module: sericum::module::Module::new("minilang"),
            types: Types {
                records: HashMap::new(),
                functions: HashMap::new(),
            },
        }
    }

    pub fn run(&mut self, input: &str) {
        println!("input:\n{}", input);
        let module = parser::parser::module(input).expect("parse failed");
        let id = self.module.create_function(
            "sericum.println.i32",
            sericum::types::Type::Void,
            vec![sericum::types::Type::i32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "sericum.print.i32",
            sericum::types::Type::Void,
            vec![sericum::types::Type::i32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "sericum.printch.i32",
            sericum::types::Type::Void,
            vec![sericum::types::Type::i32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "sericum.println.f64",
            sericum::types::Type::Void,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.print.f64",
            sericum::types::Type::Void,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.sin.f64",
            sericum::types::Type::f64,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.cos.f64",
            sericum::types::Type::f64,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.sqrt.f64",
            sericum::types::Type::f64,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.floor.f64",
            sericum::types::Type::f64,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.fabs.f64",
            sericum::types::Type::f64,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "sericum.i32_to_f64.i32",
            sericum::types::Type::f64,
            vec![sericum::types::Type::i32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "sericum.f64_to_i32.f64",
            sericum::types::Type::i32,
            vec![sericum::types::Type::f64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Int32, vec![parser::Type::F64]));
        let ptr_i64 = self.module.types.new_pointer_ty(sericum::types::Type::i64);
        let id = self.module.create_function(
            "sericum.malloc.i32",
            ptr_i64,
            vec![sericum::types::Type::i32],
        );
        self.types.functions.insert(
            id,
            (
                parser::Type::Pointer(Box::new(parser::Type::Int64)),
                vec![parser::Type::Int32],
            ),
        );

        self.run_on_module(module);
    }

    pub fn run_on_module(&mut self, module: parser::Module) {
        println!("Parsed: {:#?}", module);

        // Declare struct
        for (name, decls) in &module.structs {
            let decls_ = decls
                .iter()
                .map(|(_, ty)| ty.into_sericum_type(&self.types, &mut self.module.types))
                .collect();
            let name2idx: HashMap<String, (usize, parser::Type)> = decls
                .iter()
                .enumerate()
                .map(|(i, (name, ty))| (name.clone(), (i, ty.clone())))
                .collect();
            let struct_ty = self.module.types.new_struct_ty(decls_);
            self.types
                .records
                .insert(name.clone(), (struct_ty, name2idx));
        }

        // Create function prototypes
        let mut worklist = vec![];
        for func in &module.functions {
            let ret_ty = func
                .ret_ty
                .into_sericum_type(&self.types, &mut self.module.types);
            let params_ty = func
                .params
                .iter()
                .map(|(_name, ty)| ty.into_sericum_type(&self.types, &mut self.module.types))
                .collect::<Vec<sericum::types::Type>>();
            let func_id = self
                .module
                .create_function(func.name.as_str(), ret_ty, params_ty);
            self.types.functions.insert(
                func_id,
                (
                    func.ret_ty.clone(),
                    func.params
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .collect::<Vec<parser::Type>>(),
                ),
            );
            worklist.push((func_id, func));
        }

        // Actaully generate function
        for (func_id, func) in worklist {
            CodeGeneratorForFunction::new(
                sericum::builder::IRBuilderWithModuleAndFuncId::new(&mut self.module, func_id),
                &self.types,
                func,
            )
            .run()
        }
    }
}

impl<'a> CodeGeneratorForFunction<'a> {
    pub fn new(
        builder: sericum::builder::IRBuilderWithModuleAndFuncId<'a>,
        types: &'a Types,
        func: &'a parser::Function,
    ) -> Self {
        Self {
            builder,
            types,
            func,
            vars: vec![HashMap::new()],
        }
    }

    pub fn run(&mut self) {
        for (i, (name, ty)) in self.func.params.iter().enumerate() {
            let param = self.builder.get_param(i).unwrap();
            let ty = if matches!(ty, parser::Type::Struct(_)) {
                parser::Type::Pointer(Box::new(ty.clone()))
            } else {
                ty.clone()
            };
            self.create_var(name.clone(), true, ty.clone(), param);
        }

        let entry = self.builder.append_basic_block();
        self.builder.set_insert_point(entry);

        for stmt in &self.func.body {
            self.run_on_node(stmt);
        }
    }

    pub fn run_on_node(&mut self, node: &Node) -> (sericum::value::Value, parser::Type) {
        match node {
            Node::VarDecl(name, ty) => {
                let ty_ = ty.into_sericum_type(&self.types, &mut self.builder.func_ref_mut().types);
                let alloca = self.builder.build_alloca(ty_);
                self.create_var(
                    name.clone(),
                    false,
                    parser::Type::Pointer(Box::new(ty.clone())),
                    alloca,
                );
                (sericum::value::Value::None, parser::Type::Void)
            }
            Node::Assign(dst, src) => {
                let (dst, _) = self.run_on_node(dst);
                let (src, _) = self.run_on_node(src);
                (self.builder.build_store(src, dst), parser::Type::Void)
            }
            Node::IfElse(cond, then_, else_) => {
                let (cond, _) = self.run_on_node(cond);
                let then_bb = self.builder.append_basic_block();
                let else_bb = self.builder.append_basic_block();
                let merge_bb = if else_.is_some() {
                    self.builder.append_basic_block()
                } else {
                    else_bb
                };
                self.builder.build_cond_br(cond, then_bb, else_bb);
                self.builder.set_insert_point(then_bb);
                for node in then_ {
                    self.run_on_node(node);
                }
                if !self.builder.is_last_inst_terminator() {
                    self.builder.build_br(merge_bb);
                }
                if let Some(else_) = else_ {
                    self.builder.set_insert_point(else_bb);
                    for node in else_ {
                        self.run_on_node(node);
                    }
                    if !self.builder.is_last_inst_terminator() {
                        self.builder.build_br(merge_bb);
                    }
                }
                self.builder.set_insert_point(merge_bb);
                (sericum::value::Value::None, parser::Type::Void)
            }
            Node::WhileLoop(cond, body) => {
                let header_bb = self.builder.append_basic_block();
                let body_bb = self.builder.append_basic_block();
                let post_bb = self.builder.append_basic_block();
                self.builder.build_br(header_bb);
                self.builder.set_insert_point(header_bb);
                let (cond, _) = self.run_on_node(cond);
                self.builder.build_cond_br(cond, body_bb, post_bb);
                self.builder.set_insert_point(body_bb);
                for node in body {
                    self.run_on_node(node);
                }
                if !self.builder.is_last_inst_terminator() {
                    self.builder.build_br(header_bb);
                }
                self.builder.set_insert_point(post_bb);
                (sericum::value::Value::None, parser::Type::Void)
            }
            Node::Return(e) => {
                let (e, _) = self.run_on_node(e);
                (self.builder.build_ret(e), parser::Type::Void)
            }
            Node::Eq(lhs, rhs)
            | Node::Lt(lhs, rhs)
            | Node::Le(lhs, rhs)
            | Node::Ne(lhs, rhs)
            | Node::Gt(lhs, rhs)
            | Node::Ge(lhs, rhs) => {
                let (lhs, ty) = self.run_on_node(lhs);
                let (rhs, _) = self.run_on_node(rhs);
                if ty.is_integer() {
                    let cmp = match node {
                        Node::Eq(_, _) => sericum::opcode::ICmpKind::Eq,
                        Node::Ne(_, _) => sericum::opcode::ICmpKind::Ne,
                        Node::Lt(_, _) => sericum::opcode::ICmpKind::Lt,
                        Node::Le(_, _) => sericum::opcode::ICmpKind::Le,
                        Node::Gt(_, _) => sericum::opcode::ICmpKind::Gt,
                        Node::Ge(_, _) => sericum::opcode::ICmpKind::Ge,
                        _ => unreachable!(),
                    };
                    (self.builder.build_icmp(cmp, lhs, rhs), parser::Type::Int1)
                } else {
                    let cmp = match node {
                        Node::Eq(_, _) => sericum::opcode::FCmpKind::UEq,
                        Node::Ne(_, _) => sericum::opcode::FCmpKind::UNe,
                        Node::Lt(_, _) => sericum::opcode::FCmpKind::ULt,
                        Node::Le(_, _) => sericum::opcode::FCmpKind::ULe,
                        Node::Gt(_, _) => sericum::opcode::FCmpKind::UGt,
                        Node::Ge(_, _) => sericum::opcode::FCmpKind::UGe,
                        _ => unreachable!(),
                    };
                    (self.builder.build_fcmp(cmp, lhs, rhs), parser::Type::Int1)
                }
            }
            Node::Add(lhs, rhs)
            | Node::Sub(lhs, rhs)
            | Node::Mul(lhs, rhs)
            | Node::Div(lhs, rhs)
            | Node::Rem(lhs, rhs) => {
                let (lhs, ty) = self.run_on_node(lhs);
                let (rhs, _) = self.run_on_node(rhs);
                match node {
                    Node::Add(_, _) => (self.builder.build_add(lhs, rhs), ty),
                    Node::Sub(_, _) => (self.builder.build_sub(lhs, rhs), ty),
                    Node::Mul(_, _) => (self.builder.build_mul(lhs, rhs), ty),
                    Node::Div(_, _) => (self.builder.build_div(lhs, rhs), ty),
                    Node::Rem(_, _) => (self.builder.build_rem(lhs, rhs), ty),
                    _ => unreachable!(),
                }
            }
            Node::Call(name, args) => {
                let name = match name.as_str() {
                    "println_i32" => "sericum.println.i32",
                    "print_i32" => "sericum.print.i32",
                    "printch_i32" => "sericum.printch.i32",
                    "println_f64" => "sericum.println.f64",
                    "print_f64" => "sericum.print.f64",
                    "sin" => "sericum.sin.f64",
                    "cos" => "sericum.cos.f64",
                    "sqrt" => "sericum.sqrt.f64",
                    "floor" => "sericum.floor.f64",
                    "fabs" => "sericum.fabs.f64",
                    "i32_to_f64" => "sericum.i32_to_f64.i32",
                    "f64_to_i32" => "sericum.f64_to_i32.f64",
                    "malloc" => "sericum.malloc.i32",
                    name => name,
                };
                let func_id = self.builder.module().unwrap().find_function(name).unwrap();
                let args: Vec<sericum::value::Value> =
                    args.iter().map(|i| self.run_on_node(i).0).collect();
                let ret_ty = self.types.functions.get(&func_id).unwrap().0.clone();
                (
                    self.builder
                        .build_call(sericum::value::Value::new_func(func_id), args),
                    ret_ty,
                )
            }
            Node::Identifier(name) => {
                let (_is_arg, ty, v) = self
                    .lookup_var(name.as_str())
                    .expect("variable not found")
                    .clone();
                (v, ty)
            }
            Node::Load(from) => match &**from {
                Node::Identifier(name) => {
                    let (is_arg, ty, v) = self
                        .lookup_var(name.as_str())
                        .expect("variable not found")
                        .clone();
                    if is_arg {
                        (v, ty)
                    } else {
                        (self.builder.build_load(v), ty.get_elem_ty())
                    }
                }
                Node::Addr(e) => self.run_on_node(e),
                from => {
                    let (from, ty) = self.run_on_node(&from);
                    // if matches!(ty, parser::Type::Struct(_)) {
                    //     (from, ty)
                    // } else {
                    (self.builder.build_load(from), ty.get_elem_ty())
                    // }
                }
            },
            Node::Addr(e) => self.run_on_node(e),
            Node::TypeCast(val, to) => {
                let (val, ty) = self.run_on_node(val);
                let to_ = to.into_sericum_type(&self.types, &mut self.builder.func_ref_mut().types);
                if ty.is_integer() {
                    return (self.builder.build_sitofp(val, to_), to.clone());
                }
                if ty.is_float() {
                    return (self.builder.build_fptosi(val, to_), to.clone());
                }
                unimplemented!()
            }

            Node::Index(base, idx) => {
                let (base, ty) = self.run_on_node(base);
                let indices = vec![
                    sericum::value::Value::new_imm_int32(0),
                    self.run_on_node(idx).0,
                ];
                let gep = self.builder.build_gep(base, indices);
                (
                    gep,
                    parser::Type::Pointer(Box::new(ty.get_elem_ty().get_elem_ty())),
                )
            }
            Node::Dot(base, field) => {
                let (base, base_ty) = self.run_on_node(base);
                let (idx, ty) = base_ty
                    .get_elem_ty()
                    .get_record_member(
                        {
                            let name = match &**field {
                                Node::Identifier(name) => name,
                                _ => unreachable!(),
                            };
                            name.as_str()
                        },
                        &self.types,
                    )
                    .clone();
                let indices = vec![
                    sericum::value::Value::new_imm_int32(0),
                    sericum::value::Value::new_imm_int32(idx as i32),
                ];
                let gep = self.builder.build_gep(base, indices);
                (gep, parser::Type::Pointer(Box::new(ty)))
            }
            Node::Number(i) => (
                sericum::value::Value::new_imm_int32(*i),
                parser::Type::Int32,
            ),
            Node::FPNumber(f) => (sericum::value::Value::new_imm_f64(*f), parser::Type::F64),
        }
    }

    fn create_var(
        &mut self,
        name: String,
        is_arg: bool,
        ty: parser::Type,
        value: sericum::value::Value,
    ) {
        self.vars
            .last_mut()
            .unwrap()
            .insert(name, (is_arg, ty, value));
    }

    fn lookup_var(&self, name: &str) -> Option<&(bool, parser::Type, sericum::value::Value)> {
        self.vars.last().unwrap().get(name)
    }
}

impl parser::Type {
    pub fn into_sericum_type(
        &self,
        types1: &Types,
        types2: &mut sericum::types::Types,
    ) -> sericum::types::Type {
        match self {
            parser::Type::Void => sericum::types::Type::Void,
            parser::Type::Int1 => sericum::types::Type::i1,
            parser::Type::Int32 => sericum::types::Type::i32,
            parser::Type::Int64 => sericum::types::Type::i64,
            parser::Type::F64 => sericum::types::Type::f64,
            parser::Type::Struct(name) => types1.records.get(name.as_str()).unwrap().0,
            parser::Type::Pointer(inner) => {
                let inner = inner.into_sericum_type(types1, types2);
                types2.new_pointer_ty(inner)
            }
            parser::Type::Array(len, inner) => {
                let inner = inner.into_sericum_type(types1, types2);
                types2.new_array_ty(inner, *len)
            }
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Int32 | Self::Int64 | Self::Int1)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::F64)
    }

    pub fn get_elem_ty(&self) -> parser::Type {
        match self {
            parser::Type::Array(_, e) | parser::Type::Pointer(e) => (**e).clone(),
            _ => panic!(),
        }
    }

    pub fn get_record_member<'a>(&self, name: &str, types: &'a Types) -> &'a (usize, parser::Type) {
        match self {
            parser::Type::Struct(s_name) => types
                .records
                .get(s_name.as_str())
                .unwrap()
                .1
                .get(name)
                .unwrap(),
            _ => panic!(),
        }
    }
}
