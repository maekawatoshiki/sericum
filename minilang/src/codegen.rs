use crate::parser::{self, Node};
use cilk;
use std::collections::HashMap;

pub struct CodeGenerator {
    pub module: cilk::module::Module,
    types: Types,
}

pub struct CodeGeneratorForFunction<'a> {
    builder: cilk::builder::Builder<'a>,
    types: &'a Types,
    func: &'a parser::Function,
    vars: Vec<HashMap<String, (bool, parser::Type, cilk::value::Value)>>, // name, (is_arg, ty, val)
}

pub struct Types {
    records: HashMap<String, (cilk::types::Type, HashMap<String, (usize, parser::Type)>)>,
    functions: HashMap<cilk::function::FunctionId, (parser::Type, Vec<parser::Type>)>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            module: cilk::module::Module::new("minilang"),
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
            "cilk.println.i32",
            cilk::types::Type::Void,
            vec![cilk::types::Type::Int32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "cilk.print.i32",
            cilk::types::Type::Void,
            vec![cilk::types::Type::Int32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "cilk.printch.i32",
            cilk::types::Type::Void,
            vec![cilk::types::Type::Int32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "cilk.println.f64",
            cilk::types::Type::Void,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.print.f64",
            cilk::types::Type::Void,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Void, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.sin.f64",
            cilk::types::Type::F64,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.cos.f64",
            cilk::types::Type::F64,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.sqrt.f64",
            cilk::types::Type::F64,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.floor.f64",
            cilk::types::Type::F64,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.fabs.f64",
            cilk::types::Type::F64,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::F64]));
        let id = self.module.create_function(
            "cilk.i32_to_f64.i32",
            cilk::types::Type::F64,
            vec![cilk::types::Type::Int32],
        );
        self.types
            .functions
            .insert(id, (parser::Type::F64, vec![parser::Type::Int32]));
        let id = self.module.create_function(
            "cilk.f64_to_i32.f64",
            cilk::types::Type::Int32,
            vec![cilk::types::Type::F64],
        );
        self.types
            .functions
            .insert(id, (parser::Type::Int32, vec![parser::Type::F64]));
        let ptr_i64 = self.module.types.new_pointer_ty(cilk::types::Type::Int64);
        let id =
            self.module
                .create_function("cilk.malloc.i32", ptr_i64, vec![cilk::types::Type::Int32]);
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
        println!("Parsed: {:?}", module);

        // Declare struct
        for (name, decls) in &module.structs {
            let decls_ = decls
                .iter()
                .map(|(_, ty)| ty.into_cilk_type(&self.types, &mut self.module.types))
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
                .into_cilk_type(&self.types, &mut self.module.types);
            let params_ty = func
                .params
                .iter()
                .map(|(_name, ty)| ty.into_cilk_type(&self.types, &mut self.module.types))
                .collect::<Vec<cilk::types::Type>>();
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
            CodeGeneratorForFunction::new(&mut self.module, &self.types, func_id, func).run()
        }
    }
}

impl<'a> CodeGeneratorForFunction<'a> {
    pub fn new(
        module: &'a mut cilk::module::Module,
        types: &'a Types,
        func_id: cilk::function::FunctionId,
        func: &'a parser::Function,
    ) -> Self {
        Self {
            builder: cilk::builder::Builder::new(module, func_id),
            types,
            func,
            vars: vec![HashMap::new()],
        }
    }

    pub fn run(&mut self) {
        for (i, (name, ty)) in self.func.params.iter().enumerate() {
            let param = self.builder.get_param(i).unwrap();
            self.create_var(name.clone(), true, ty.clone(), param);
        }

        let entry = self.builder.append_basic_block();
        self.builder.set_insert_point(entry);

        for stmt in &self.func.body {
            self.run_on_node(stmt);
        }
    }

    pub fn run_on_node(&mut self, node: &Node) -> (cilk::value::Value, parser::Type) {
        match node {
            Node::VarDecl(name, ty) => {
                let ty_ = ty.into_cilk_type(&self.types, &mut self.builder.module.types);
                let alloca = self.builder.build_alloca(ty_);
                self.create_var(
                    name.clone(),
                    false,
                    parser::Type::Pointer(Box::new(ty.clone())),
                    alloca,
                );
                (cilk::value::Value::None, parser::Type::Void)
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
                (cilk::value::Value::None, parser::Type::Void)
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
                (cilk::value::Value::None, parser::Type::Void)
            }
            Node::Return(e) => {
                let (e, _) = self.run_on_node(e);
                (self.builder.build_ret(e), parser::Type::Void)
            }
            Node::Eq(lhs, rhs) | Node::Lt(lhs, rhs) | Node::Le(lhs, rhs) => {
                let (lhs, ty) = self.run_on_node(lhs);
                let (rhs, _) = self.run_on_node(rhs);
                if ty.is_integer() {
                    let cmp = match node {
                        Node::Eq(_, _) => cilk::opcode::ICmpKind::Eq,
                        Node::Lt(_, _) => cilk::opcode::ICmpKind::Lt,
                        Node::Le(_, _) => cilk::opcode::ICmpKind::Le,
                        _ => unreachable!(),
                    };
                    (self.builder.build_icmp(cmp, lhs, rhs), parser::Type::Int1)
                } else {
                    let cmp = match node {
                        Node::Eq(_, _) => cilk::opcode::FCmpKind::UEq,
                        Node::Lt(_, _) => cilk::opcode::FCmpKind::ULt,
                        Node::Le(_, _) => cilk::opcode::FCmpKind::ULe,
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
                    "println_i32" => "cilk.println.i32",
                    "print_i32" => "cilk.print.i32",
                    "printch_i32" => "cilk.printch.i32",
                    "println_f64" => "cilk.println.f64",
                    "print_f64" => "cilk.print.f64",
                    "sin" => "cilk.sin.f64",
                    "cos" => "cilk.cos.f64",
                    "sqrt" => "cilk.sqrt.f64",
                    "floor" => "cilk.floor.f64",
                    "fabs" => "cilk.fabs.f64",
                    "i32_to_f64" => "cilk.i32_to_f64.i32",
                    "f64_to_i32" => "cilk.f64_to_i32.f64",
                    "malloc" => "cilk.malloc.i32",
                    name => name,
                };
                let func_id = self.builder.module.find_function(name).unwrap();
                let args: Vec<cilk::value::Value> =
                    args.iter().map(|i| self.run_on_node(i).0).collect();
                let ret_ty = self.types.functions.get(&func_id).unwrap().0.clone();
                (
                    self.builder.build_call(
                        cilk::value::Value::new_func(cilk::value::FunctionValue { func_id }),
                        args,
                    ),
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
                    (self.builder.build_load(from), ty.get_elem_ty())
                }
            },
            Node::Addr(e) => self.run_on_node(e),
            Node::Index(base, idx) => {
                let (base, ty) = self.run_on_node(base);
                let indices = vec![
                    cilk::value::Value::new_imm_int32(0),
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
                    cilk::value::Value::new_imm_int32(0),
                    cilk::value::Value::new_imm_int32(idx as i32),
                ];
                let gep = self.builder.build_gep(base, indices);
                (gep, parser::Type::Pointer(Box::new(ty)))
            }
            Node::Number(i) => (cilk::value::Value::new_imm_int32(*i), parser::Type::Int32),
            Node::FPNumber(f) => (cilk::value::Value::new_imm_f64(*f), parser::Type::F64),
        }
    }

    fn create_var(
        &mut self,
        name: String,
        is_arg: bool,
        ty: parser::Type,
        value: cilk::value::Value,
    ) {
        self.vars
            .last_mut()
            .unwrap()
            .insert(name, (is_arg, ty, value));
    }

    fn lookup_var(&self, name: &str) -> Option<&(bool, parser::Type, cilk::value::Value)> {
        self.vars.last().unwrap().get(name)
    }
}

impl parser::Type {
    pub fn into_cilk_type(
        &self,
        types1: &Types,
        types2: &mut cilk::types::Types,
    ) -> cilk::types::Type {
        match self {
            parser::Type::Void => cilk::types::Type::Void,
            parser::Type::Int1 => cilk::types::Type::Int1,
            parser::Type::Int32 => cilk::types::Type::Int32,
            parser::Type::Int64 => cilk::types::Type::Int64,
            parser::Type::F64 => cilk::types::Type::F64,
            parser::Type::Struct(name) => types1.records.get(name.as_str()).unwrap().0,
            parser::Type::Pointer(inner) => {
                let inner = inner.into_cilk_type(types1, types2);
                types2.new_pointer_ty(inner)
            }
            parser::Type::Array(len, inner) => {
                let inner = inner.into_cilk_type(types1, types2);
                types2.new_array_ty(inner, *len)
            }
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Int32 | Self::Int64 | Self::Int1)
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
