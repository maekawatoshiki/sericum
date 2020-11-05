use super::{function::DAGFunction, node::*};
use crate::codegen::common::types::MVType;
use crate::ir::types::Type;
use id_arena::{Arena, Id};
use rustc_hash::FxHashMap;

pub struct InstPatternMatcherOnFunction<'a> {
    func: &'a mut DAGFunction,
}

enum Pat {
    IR(IRPat),
    MI,
    Operand(OperandPat),
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

#[derive(PartialEq, Eq)]
struct OperandPat {
    pub name: &'static str,
    pub kind: OperandKind,
    pub not: bool,
}

#[derive(PartialEq, Eq)]
enum OperandKind {
    Immediate,
    Reg,
    Invalid,
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
        self.kind = OperandKind::Immediate;
        self
    }
}

const fn not() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Invalid,
        not: true,
    }
}

const fn immediate() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Immediate,
        not: false,
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
        3,
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

    let pat: Pat = ir_node()
        .opcode(IROpcode::Add)
        .args(vec![
            ir_node()
                .opcode(IROpcode::Add)
                .args(vec![
                    not().immediate().named("c").into(),
                    immediate().named("d").into(),
                ])
                .into(),
            immediate().named("e").into(),
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

    let mut map = NameMap::default();
    if let Some(gen) = try_match(&arena, node, &pat, &mut map) {
        let _id = gen(&map, &mut arena);
        return;
    }

    panic!()
}

// impl Pat {
//     fn opcode(
// }

// if node.
// let node = arena[node_id];
// if node.is_ir() {
//     let node = node.as_ir();
//     if node.args()[0].is_operation() {
//         let node2 = arena[node.args()[0].operation_id()];
//         if node2.opcode == Add
//             && !node2.args()[0].is_constant()
//             && node2.args()[1].is_constant()
//         {}
//     }
// }

// ir_node().opcode(Add).matcher(|m| {
//     m.args(
//         0,
//         ir_node()
//             .opcode(Add)
//             .args(0, not_constant)
//             .args(1, constant),
//     )
//     .into(|nodes| {
//         let n1 = nodes[0];
//         let n2 = nodes[1];
//         Node::IR(IRNode{
//             Add,
//         })
//     });
//     m.args(0, ..);

impl<'a> InstPatternMatcherOnFunction<'a> {
    pub fn new(func: &'a mut DAGFunction) -> Self {
        Self { func }
    }

    pub fn try_match(&self, list: Vec<i32>) -> () {}
}

fn try_match(arena: &Arena<Node>, id: NodeId, pat: &Pat, m: &mut NameMap) -> Option<Box<GenFn>> {
    if !matches(arena, id, pat, m) {
        return None;
    }

    println!("{:?}", m);

    match pat {
        Pat::IR(pat) => return pat.generate.clone(),
        Pat::MI => {}
        Pat::Operand(_) => {}
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
            let matches_ = if op.not {
                match op.kind {
                    OperandKind::Immediate => !matches!(n, &OperandNode::Immediate(_)),
                    OperandKind::Reg => n != &OperandNode::Reg,
                    OperandKind::Invalid => panic!(),
                }
            } else {
                match op.kind {
                    OperandKind::Immediate => matches!(n, &OperandNode::Immediate(_)),
                    OperandKind::Reg => n == &OperandNode::Reg,
                    OperandKind::Invalid => panic!(),
                }
            };
            if matches_ && !op.name.is_empty() {
                m.insert(op.name, id);
            }
            matches_
        }
        Pat::Invalid => panic!(),
    }
}
