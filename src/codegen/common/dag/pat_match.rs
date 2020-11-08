use super::{function::DAGFunction, node::*};
use crate::codegen::common::types::MVType;
use crate::ir::types::Type;
use id_arena::{Arena, Id};
use rustc_hash::FxHashMap;
use std::ops::BitOr;

pub struct InstPatternMatcherOnFunction<'a> {
    func: &'a mut DAGFunction,
}

enum Pat {
    IR(IRPat),
    MI,
    Operand(OperandPat),
    Compound(CompoundPat),
    Invalid,
}

type GenFn = fn(&FxHashMap<&'static str, NodeId>, &mut Arena<Node>) -> NodeId;

struct IRPat {
    pub name: &'static str,
    pub opcode: Option<IROpcode>,
    pub operands: Vec<Pat>,
    pub ty: Type,
    pub generate: Option<Box<GenFn>>,
}

struct MIPat {}

struct OperandPat {
    pub name: &'static str,
    pub kind: OperandKind,
    pub not: bool,
    pub generate: Option<Box<GenFn>>,
}

struct CompoundPat {
    pub name: &'static str,
    pub pats: Vec<Pat>,
}

#[derive(PartialEq, Eq)]
enum OperandKind {
    Immediate(Immediate),
    Reg,
    Invalid,
}

#[derive(PartialEq, Eq)]
enum Immediate {
    AnyInt32,
    Int32(i32),
    Any,
}

const fn ir_node() -> IRPat {
    IRPat {
        name: "",
        opcode: None,
        operands: vec![],
        ty: Type::Void,
        generate: None,
    }
}

const fn ir(opcode: IROpcode) -> IRPat {
    IRPat {
        name: "",
        opcode: Some(opcode),
        operands: vec![],
        ty: Type::Void,
        generate: None,
    }
}

const fn mi_node() -> MIPat {
    MIPat {}
}

impl IRPat {
    fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    fn opcode(mut self, opcode: IROpcode) -> Self {
        self.opcode = Some(opcode);
        self
    }

    fn args(mut self, operands: Vec<Pat>) -> Self {
        self.operands = operands;
        self
    }

    fn generate(mut self, f: GenFn) -> Self {
        self.generate = Some(Box::new(f));
        self
    }
}

impl OperandPat {
    fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn immediate(mut self) -> Self {
        self.kind = OperandKind::Immediate(Immediate::Any);
        self
    }

    pub fn any_i32_immediate(mut self) -> Self {
        self.kind = OperandKind::Immediate(Immediate::AnyInt32);
        self
    }

    fn generate(mut self, f: GenFn) -> Self {
        self.generate = Some(Box::new(f));
        self
    }
}

impl CompoundPat {
    fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }
}

const fn not() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Invalid,
        not: true,
        generate: None,
    }
}

const fn immediate() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Immediate(Immediate::Any),
        not: false,
        generate: None,
    }
}

const fn i32_immediate(i: i32) -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Immediate(Immediate::Int32(i)),
        not: false,
        generate: None,
    }
}

const fn any_i32_immediate() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Immediate(Immediate::AnyInt32),
        not: false,
        generate: None,
    }
}

impl Into<Pat> for IRPat {
    fn into(self) -> Pat {
        Pat::IR(self)
    }
}

impl Into<Pat> for OperandPat {
    fn into(self) -> Pat {
        Pat::Operand(self)
    }
}

impl Into<Pat> for CompoundPat {
    fn into(self) -> Pat {
        Pat::Compound(self)
    }
}

impl BitOr for IRPat {
    type Output = CompoundPat;

    fn bitor(self, rhs: Self) -> Self::Output {
        CompoundPat {
            name: "",
            pats: vec![self.into(), rhs.into()],
        }
    }
}

impl BitOr for OperandPat {
    type Output = CompoundPat;

    fn bitor(self, rhs: Self) -> Self::Output {
        CompoundPat {
            name: "",
            pats: vec![self.into(), rhs.into()],
        }
    }
}

// impl BitOr for IRPat {}

// if node.operand[0].is_operation()
//     && node.operand[0].kind == NodeKind::IR(IRNodeKind::Add)
//     && !node.operand[0].operand[0].is_constant()
//     && node.operand[0].operand[1].is_constant()
//     && node.operand[1].is_constant()
// {

#[test]
fn xxx() {
    let mut arena: Arena<Node> = Arena::new();
    let reg = arena.alloc(Node::Operand(OperandNode::Reg));
    let imm1 = arena.alloc(Node::Operand(OperandNode::Immediate(ImmediateKind::Int32(
        2,
    ))));
    let imm2 = arena.alloc(Node::Operand(OperandNode::Immediate(ImmediateKind::Int32(
        5,
    ))));
    let add1 = arena.alloc(Node::IR(IRNode {
        opcode: IROpcode::Add,
        args: vec![reg, imm1],
        ty: Type::i32,
        mvty: MVType::i32,
        next: None,
        chain: None,
    }));
    let add2 = arena.alloc(Node::IR(IRNode {
        opcode: IROpcode::Add,
        args: vec![add1, imm2],
        ty: Type::i32,
        mvty: MVType::i32,
        next: None,
        chain: None,
    }));
    let node = add2;

    // ((X + C) + (3|5)) => (X + (C+3|C+5))
    let pat: Pat = ir(IROpcode::Add)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    not().any_i32_immediate().named("c").into(),
                    any_i32_immediate().named("d").into(),
                ])
                .into(),
            (i32_immediate(3) | i32_immediate(5)).named("e").into(),
        ])
        .generate(|m, a| {
            let lhs = m["c"];
            let rhs = a[m["d"]].as_i32() + a[m["e"]].as_i32();
            let rhs = a.alloc(rhs.into());
            a.alloc(
                IRNode::new(IROpcode::Add)
                    .args(vec![lhs, rhs])
                    .ty(Type::i32)
                    .into(),
            )
        })
        .into();

    // (C + X) => (X + C)
    // let _: Pat = ir(IROpcode::Add)
    //     .named("a")
    //     .args(vec![
    //         any_i32_immediate().into(),
    //         not().any_i32_immediate().into(),
    //     ])
    //     .generate(|m, a| {
    //         a.get_mut(m["a"]).unwrap().as_ir_mut().args.swap(0, 1);
    //         m["a"]
    //     })
    //     .into();

    let pats = vec![
        pat,
        i32_immediate(7)
            .named("E")
            .generate(|m, a| a.alloc(OperandNode::i32(11).into()))
            .into(),
    ];

    let new_node = inst_select(&mut ReplacedNodeMap::default(), &mut arena, node, &pats);

    println!("{:#?}", arena);
    println!("{:?}: {:?}", new_node, arena[new_node]);

    // let mut map = NameMap::default();
    // if let Some(gen) = try_match(&arena, node, &pat, &mut map) {
    //     for (_, &id) in &map {
    //         try_match(&arena, id, &pat, &mut NameMap::default());
    //     }
    //     let _id = gen(&map, &mut arena);
    //     return;
    // }

    // panic!()
}

impl<'a> InstPatternMatcherOnFunction<'a> {
    pub fn new(func: &'a mut DAGFunction) -> Self {
        Self { func }
    }

    pub fn try_match(&self, list: Vec<i32>) -> () {}
}

type ReplacedNodeMap = FxHashMap<NodeId, NodeId>;

fn inst_select(
    replaced: &mut ReplacedNodeMap,
    arena: &mut Arena<Node>,
    id: NodeId,
    pats: &[Pat],
) -> NodeId {
    if let Some(replaced) = replaced.get(&id) {
        return *replaced;
    }

    let mut map = NameMap::default();
    for pat in pats {
        if let Some(gen) = try_match(&arena, id, &pat, &mut map) {
            for (_, named_id) in &mut map {
                if named_id == &id {
                    continue;
                }
                *named_id = inst_select(replaced, arena, *named_id, pats);
            }
            let new_id = gen(&map, arena);
            replaced.insert(id, new_id);
            return inst_select(replaced, arena, new_id, pats);
        }
    }

    // Operand selection
    let args_id = match &arena[id] {
        Node::IR(ir) => ir.args.clone(),
        Node::MI(mi) => mi.args.clone(),
        Node::Operand(_) => vec![],
    };
    let mut replaced_args = ReplacedNodeMap::default();
    for id in args_id {
        let new_id = inst_select(replaced, arena, id, pats);
        replaced_args.insert(id, new_id);
    }
    // Actually replace args
    for id in arena[id].args_mut() {
        *id = replaced_args[id];
    }
    id
}

fn try_match(arena: &Arena<Node>, id: NodeId, pat: &Pat, m: &mut NameMap) -> Option<Box<GenFn>> {
    if !matches(arena, id, pat, m) {
        return None;
    }

    println!("{:?}", m);

    match pat {
        Pat::IR(pat) => return pat.generate.clone(),
        Pat::MI => {}
        Pat::Operand(pat) => return pat.generate.clone(),
        Pat::Compound(_) => todo!(),
        Pat::Invalid => panic!(),
    }

    None

    // arena[id]
    // None
}

pub type NameMap = FxHashMap<&'static str, NodeId>;

fn matches(arena: &Arena<Node>, id: NodeId, pat: &Pat, m: &mut NameMap) -> bool {
    match pat {
        Pat::IR(pat) => {
            let n = match &arena[id] {
                Node::IR(ir) => ir,
                _ => return false,
            };
            let same_opcode = Some(n.opcode) == pat.opcode;
            let same_operands = pat
                .operands
                .iter()
                .zip(n.args.iter())
                .all(|(pat, &id)| matches(arena, id, pat, m));
            let matches_ = same_opcode && same_operands;
            if matches_ && !pat.name.is_empty() {
                m.insert(pat.name, id);
            }
            matches_
        }
        Pat::MI => false,
        Pat::Operand(op) => {
            let n = match &arena[id] {
                Node::Operand(op) => op,
                _ => return false,
            };
            let matches_ = match op.kind {
                OperandKind::Immediate(Immediate::AnyInt32) => {
                    matches!(n, &OperandNode::Immediate(ImmediateKind::Int32(_)))
                }
                OperandKind::Immediate(Immediate::Int32(i)) => {
                    matches!(n, &OperandNode::Immediate(ImmediateKind::Int32(x)) if x == i)
                }
                OperandKind::Immediate(Immediate::Any) => matches!(n, &OperandNode::Immediate(_)),
                OperandKind::Reg => n == &OperandNode::Reg,
                OperandKind::Invalid => panic!(),
            };
            let matches_ = if op.not { !matches_ } else { matches_ };
            if matches_ && !op.name.is_empty() {
                m.insert(op.name, id);
            }
            matches_
        }
        Pat::Compound(pat) => {
            let matches_ = pat.pats.iter().any(|pat| matches(arena, id, pat, m));
            if matches_ && !pat.name.is_empty() {
                m.insert(pat.name, id);
            }
            matches_
        }
        Pat::Invalid => panic!(),
    }
}
