use super::node::*;
use crate::codegen::arch::machine::register::{ty2rc, RegisterClassKind as RC};
use crate::codegen::common::{machine::register::RegistersInfo, types::MVType};
use crate::ir::types::Type;
use id_arena::Arena;
use rustc_hash::FxHashMap;
use std::ops::BitOr;

pub type GenFn = fn(&FxHashMap<&'static str, NodeId>, &mut MatchContext) -> NodeId;
pub type ReplacedNodeMap = FxHashMap<NodeId, NodeId>;
pub type NameMap = FxHashMap<&'static str, NodeId>;

pub struct MatchContext<'a> {
    pub arena: &'a mut Arena<Node>,
    pub regs: &'a RegistersInfo,
}

pub enum Pat {
    IR(IRPat),
    MI,
    Operand(OperandPat),
    Compound(CompoundPat),
    Invalid,
}

pub struct IRPat {
    pub name: &'static str,
    pub opcode: Option<IROpcode>,
    pub operands: Vec<Pat>,
    pub ty: Option<Type>,
    pub generate: Option<Box<GenFn>>,
}

pub struct MIPat {}

pub struct OperandPat {
    pub name: &'static str,
    pub kind: OperandKind,
    pub not: bool,
    pub generate: Option<Box<GenFn>>,
}

pub struct CompoundPat {
    pub name: &'static str,
    pub pats: Vec<Pat>,
    pub generate: Option<Box<GenFn>>,
}

pub enum OperandKind {
    Imm(Immediate),
    Slot(Slot),
    Reg(Register),
    Block(Block),
    Invalid,
}

#[derive(PartialEq, Eq)]
pub enum Immediate {
    AnyInt32,
    AnyInt64,
    Int32(i32),
    Any,
}

pub enum Slot {
    Type(MVType),
    Any,
}

pub enum Register {
    Class(RC),
    Any,
}

pub enum Block {
    Any,
}

pub const fn ir_node() -> IRPat {
    IRPat {
        name: "",
        opcode: None,
        operands: vec![],
        ty: None,
        generate: None,
    }
}

pub const fn ir(opcode: IROpcode) -> IRPat {
    IRPat {
        name: "",
        opcode: Some(opcode),
        operands: vec![],
        ty: None,
        generate: None,
    }
}

pub const fn mi_node() -> MIPat {
    MIPat {}
}

impl IRPat {
    pub fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn ty(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }

    pub fn opcode(mut self, opcode: IROpcode) -> Self {
        self.opcode = Some(opcode);
        self
    }

    pub fn args(mut self, operands: Vec<Pat>) -> Self {
        self.operands = operands;
        self
    }

    pub fn generate(mut self, f: GenFn) -> Self {
        self.generate = Some(Box::new(f));
        self
    }
}

impl OperandPat {
    pub fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn imm(mut self) -> Self {
        self.kind = OperandKind::Imm(Immediate::Any);
        self
    }

    pub fn any_i32_imm(mut self) -> Self {
        self.kind = OperandKind::Imm(Immediate::AnyInt32);
        self
    }

    pub fn generate(mut self, f: GenFn) -> Self {
        self.generate = Some(Box::new(f));
        self
    }
}

impl CompoundPat {
    pub fn named(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn generate(mut self, f: GenFn) -> Self {
        self.generate = Some(Box::new(f));
        self
    }
}

pub const fn not() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Invalid,
        not: true,
        generate: None,
    }
}

pub const fn imm() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Imm(Immediate::Any),
        not: false,
        generate: None,
    }
}

pub const fn i32_imm(i: i32) -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Imm(Immediate::Int32(i)),
        not: false,
        generate: None,
    }
}

pub const fn any_i32_imm() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Imm(Immediate::AnyInt32),
        not: false,
        generate: None,
    }
}

pub const fn any_i64_imm() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Imm(Immediate::AnyInt64),
        not: false,
        generate: None,
    }
}

pub const fn any_slot() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Slot(Slot::Any),
        not: false,
        generate: None,
    }
}

pub const fn slot(ty: MVType) -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Slot(Slot::Type(ty)),
        not: false,
        generate: None,
    }
}

pub const fn reg_class(rc: RC) -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Reg(Register::Class(rc)),
        not: false,
        generate: None,
    }
}

pub const fn any_reg() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Reg(Register::Any),
        not: false,
        generate: None,
    }
}

pub const fn any_block() -> OperandPat {
    OperandPat {
        name: "",
        kind: OperandKind::Block(Block::Any),
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

impl Into<CompoundPat> for OperandPat {
    fn into(self) -> CompoundPat {
        CompoundPat {
            name: "",
            pats: vec![self.into()],
            generate: None,
        }
    }
}

impl Into<CompoundPat> for IRPat {
    fn into(self) -> CompoundPat {
        CompoundPat {
            name: "",
            pats: vec![self.into()],
            generate: None,
        }
    }
}

impl Into<Pat> for CompoundPat {
    fn into(self) -> Pat {
        Pat::Compound(self)
    }
}

impl Into<Pat> for RC {
    fn into(self) -> Pat {
        Pat::Operand(OperandPat {
            name: "",
            kind: OperandKind::Reg(Register::Class(self)),
            not: false,
            generate: None,
        })
    }
}

impl BitOr for IRPat {
    type Output = CompoundPat;

    fn bitor(self, rhs: Self) -> Self::Output {
        CompoundPat {
            name: "",
            pats: vec![self.into(), rhs.into()],
            generate: None,
        }
    }
}

impl BitOr for OperandPat {
    type Output = CompoundPat;

    fn bitor(self, rhs: Self) -> Self::Output {
        CompoundPat {
            name: "",
            pats: vec![self.into(), rhs.into()],
            generate: None,
        }
    }
}

impl BitOr for CompoundPat {
    type Output = CompoundPat;

    fn bitor(self, rhs: Self) -> Self::Output {
        CompoundPat {
            name: "",
            pats: vec![self.pats, rhs.pats]
                .into_iter()
                .flatten()
                .collect::<Vec<Pat>>(),
            generate: None,
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
    let regs = RegistersInfo::new();

    // let reg = arena.alloc(Node::Operand(OperandNode::Reg(regs.new_virt_reg(RC::GR32))));
    let imm1 = arena.alloc(Node::Operand(OperandNode::Imm(ImmediateKind::Int32(2))));
    let imm2 = arena.alloc(Node::Operand(OperandNode::Imm(ImmediateKind::Int32(5))));
    let node = arena.alloc(
        IRNode::new(IROpcode::Sub)
            .args(vec![imm1, imm2])
            .ty(Type::i32)
            .into(),
    );
    // let add1 = arena.alloc(Node::IR(IRNode {
    //     opcode: IROpcode::Add,
    //     args: vec![reg, imm1],
    //     ty: Type::i32,
    //     mvty: MVType::i32,
    //     next: None,
    //     chain: None,
    // }));
    // let add2 = arena.alloc(Node::IR(IRNode {
    //     opcode: IROpcode::Add,
    //     args: vec![add1, imm2],
    //     ty: Type::i32,
    //     mvty: MVType::i32,
    //     next: None,
    //     chain: None,
    // }));
    // let node = add2;

    // ((X + C) + (3|5)) => (X + (C+3|C+5))
    let pat: Pat = ir(IROpcode::Add)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    reg_class(RC::GR32).named("c").into(),
                    // not().any_i32_imm().named("c").into(),
                    any_i32_imm().named("d").into(),
                ])
                .into(),
            (i32_imm(3) | i32_imm(5)).named("e").into(),
        ])
        .generate(|m, c| {
            let lhs = m["c"];
            let rhs = c.arena[m["d"]].as_i32() + c.arena[m["e"]].as_i32();
            let rhs = c.arena.alloc(rhs.into());
            c.arena.alloc(
                IRNode::new(IROpcode::Add)
                    .args(vec![lhs, rhs])
                    .ty(Type::i32)
                    .into(),
            )
        })
        .into();

    let p: Pat = (ir(IROpcode::Add)
        .args(vec![any_i32_imm().named("i").into(), any_i32_imm().into()])
        | ir(IROpcode::Sub).args(vec![any_i32_imm().named("i").into(), any_i32_imm().into()]))
    .named("x")
    .generate(|m, c| {
        let id = m["x"];
        match c.arena[id].as_ir().opcode {
            IROpcode::Add => m["i"],
            IROpcode::Sub => m["i"],
            _ => unreachable!(),
        }
    })
    .into();

    let pats = vec![
        pat,
        p,
        i32_imm(7)
            .named("E")
            .generate(|_, c| c.arena.alloc(OperandNode::i32(11).into()))
            .into(),
    ];

    let new_node = inst_select(
        &mut ReplacedNodeMap::default(),
        &mut MatchContext {
            arena: &mut arena,
            regs: &regs,
        },
        node,
        &pats,
    );

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

pub fn inst_select(
    replaced: &mut ReplacedNodeMap,
    ctx: &mut MatchContext,
    id: NodeId,
    pats: &[Pat],
) -> NodeId {
    if let Some(replaced) = replaced.get(&id) {
        return *replaced;
    }

    println!("{:?}", ctx.arena[id]);

    let mut map = NameMap::default();
    for pat in pats {
        if let Some(gen) = try_match(ctx, id, &pat, &mut map) {
            for (_, named_id) in &mut map {
                // If current node (`id`) is named, `inst_select` infinitely recurses.
                // To avoid it, do not `inst_select` current node.
                if named_id == &id {
                    continue;
                }
                *named_id = inst_select(replaced, ctx, *named_id, pats);
            }
            let new_id = gen(&map, ctx);
            replaced.insert(id, new_id);
            return inst_select(replaced, ctx, new_id, pats);
        }
    }

    operand_select(replaced, ctx, id, pats);

    id
}

fn operand_select(
    replaced: &mut ReplacedNodeMap,
    ctx: &mut MatchContext,
    inst_id: NodeId,
    pats: &[Pat],
) {
    let args_id = match &ctx.arena[inst_id] {
        Node::IR(ir) => ir.args.clone(),
        Node::MI(mi) => mi.args.clone(),
        Node::Operand(_) => vec![],
        Node::None => vec![],
    };
    let mut replaced_args = ReplacedNodeMap::default();
    for id in args_id {
        let new_id = inst_select(replaced, ctx, id, pats);
        replaced_args.insert(id, new_id);
    }
    // Actually replace args
    for id in ctx.arena[inst_id].args_mut() {
        *id = replaced_args[id];
    }
}

fn try_match(ctx: &mut MatchContext, id: NodeId, pat: &Pat, m: &mut NameMap) -> Option<Box<GenFn>> {
    if !matches(ctx, id, pat, m) {
        return None;
    }

    println!("{:?}", m);

    match pat {
        Pat::IR(pat) => return pat.generate.clone(),
        Pat::MI => panic!(),
        Pat::Operand(pat) => return pat.generate.clone(),
        Pat::Compound(pat) => return pat.generate.clone(),
        Pat::Invalid => panic!(),
    }
}

fn matches(ctx: &MatchContext, id: NodeId, pat: &Pat, m: &mut NameMap) -> bool {
    match pat {
        Pat::IR(pat) => {
            let n = match &ctx.arena[id] {
                Node::IR(ir) => ir,
                _ => return false,
            };
            let same_opcode = Some(n.opcode) == pat.opcode;
            let same_operands = pat
                .operands
                .iter()
                .zip(n.args.iter())
                .all(|(pat, &id)| matches(ctx, id, pat, m));
            let same_ty = pat.ty.map_or(true, |ty| n.ty == ty);
            let matches_ = same_opcode && same_operands && same_ty;
            if matches_ && !pat.name.is_empty() {
                m.insert(pat.name, id);
            }
            matches_
        }
        Pat::MI => false,
        Pat::Operand(op) => {
            let matches_ = match &ctx.arena[id] {
                Node::Operand(n) => {
                    let matches_ = match &op.kind {
                        OperandKind::Imm(Immediate::AnyInt32) => {
                            matches!(n, &OperandNode::Imm(ImmediateKind::Int32(_)))
                        }
                        OperandKind::Imm(Immediate::AnyInt64) => {
                            matches!(n, &OperandNode::Imm(ImmediateKind::Int64(_)))
                        }
                        OperandKind::Imm(Immediate::Int32(i)) => {
                            matches!(n, &OperandNode::Imm(ImmediateKind::Int32(x)) if x == *i)
                        }
                        OperandKind::Imm(Immediate::Any) => matches!(n, &OperandNode::Imm(_)),
                        OperandKind::Reg(Register::Class(reg_class)) => {
                            matches!(n, OperandNode::Reg(id)
                                                if ctx.regs.arena_ref()[*id].reg_class == *reg_class)
                        }
                        OperandKind::Reg(Register::Any) => matches!(n, OperandNode::Reg(_)),
                        OperandKind::Slot(Slot::Any) => matches!(n, &OperandNode::Slot(_)),
                        OperandKind::Slot(Slot::Type(ty)) => {
                            matches!(n, &OperandNode::Slot(slot) if ty == &slot.ty.into())
                        }
                        OperandKind::Block(_) => matches!(n, &OperandNode::Block(_)),
                        OperandKind::Invalid => panic!(),
                    };
                    if op.not {
                        !matches_
                    } else {
                        matches_
                    }
                }
                Node::IR(IRNode { ty, .. }) => match &op.kind {
                    OperandKind::Reg(Register::Class(reg_class))
                        if ty2rc(ty).unwrap() == *reg_class =>
                    {
                        true
                    }
                    OperandKind::Reg(Register::Any) if !matches!(ty, Type::Void) => true,
                    _ => false,
                },
                Node::MI(MINode { reg_class: rc, .. }) => match &op.kind {
                    OperandKind::Reg(Register::Class(reg_class)) if rc == &Some(*reg_class) => true,
                    OperandKind::Reg(Register::Any) if rc.is_some() => true,
                    _ => false,
                },
                Node::None => false,
            };
            if matches_ && !op.name.is_empty() {
                m.insert(op.name, id);
            }
            matches_
        }
        Pat::Compound(pat) => {
            let matches_ = pat.pats.iter().any(|pat| matches(ctx, id, pat, m));
            if matches_ && !pat.name.is_empty() {
                m.insert(pat.name, id);
            }
            matches_
        }
        Pat::Invalid => panic!(),
    }
}
