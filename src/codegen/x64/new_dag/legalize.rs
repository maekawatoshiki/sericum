use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::new_dag::{
    function::DAGFunction,
    module::DAGModule,
    node::{IRNode, IROpcode, MINode, NodeId, OperandNode},
    pat_match::{
        any, any_f64_imm, any_i32_imm, any_reg, any_slot, inst_select, ir, null_imm, reg_class,
        slot, MatchContext, Pat, ReplacedNodeMap,
    },
};
use crate::ir::types::Type;

pub fn run(module: &mut DAGModule) {
    for (_, func) in &mut module.functions {
        if func.is_internal {
            continue;
        }
        run_on_function(func);
    }
}

fn run_on_function(func: &mut DAGFunction) {
    let load: Pat = ir(IROpcode::Load)
        .ty(Type::i32)
        .args(vec![ir(IROpcode::Add)
            .args(vec![
                reg_class(RC::GR64).named("base").into(),
                any_i32_imm().named("off").into(),
            ])
            .into()])
        .generate(|m, c| {
            let mem = c
                .arena
                .alloc(MemKind::BaseOff(vec![m["base"], m["off"]]).into());
            c.arena.alloc(
                MINode::new(MO::MOVrm32)
                    .args(vec![mem])
                    .reg_class(RC::GR32)
                    .into(),
            )
        })
        .into();
    let load2: Pat = ir(IROpcode::Load)
        .ty(Type::i32)
        .args(vec![ir(IROpcode::Add)
            .args(vec![
                reg_class(RC::GR64).named("base").into(),
                ir(IROpcode::Mul)
                    .args(vec![
                        any().named("off").into(),
                        any_i32_imm().named("align").into(),
                    ])
                    .into(),
            ])
            .into()])
        .generate(|m, c| {
            let off = c.arena.alloc(
                IRNode::new(IROpcode::RegClass)
                    .args(vec![m["off"]])
                    .ty(Type::i64)
                    .into(),
            );
            let mem = c
                .arena
                .alloc(MemKind::BaseAlignOff([m["base"], m["align"], off]).into());
            c.arena.alloc(
                MINode::new(MO::MOVrm32)
                    .args(vec![mem])
                    .reg_class(RC::GR32)
                    .into(),
            )
        })
        .into();
    let load3: Pat = ir(IROpcode::Load)
        .ty(Type::f64)
        .args(vec![ir(IROpcode::Add)
            .args(vec![
                ir(IROpcode::FIAddr)
                    .args(vec![any_slot().named("slot").into()])
                    .into(),
                any_i32_imm().named("off").into(),
            ])
            .into()])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(MemKind::BaseFiOff([rbp, m["slot"], m["off"]]).into());
            c.arena.alloc(
                MINode::new(MO::MOVSDrm)
                    .args(vec![mem])
                    .reg_class(RC::XMM)
                    .into(),
            )
        })
        .into();
    let store: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    reg_class(RC::GR64).named("base").into(),
                    ir(IROpcode::Mul)
                        .args(vec![
                            any().named("off").into(),
                            any_i32_imm().named("align").into(),
                        ])
                        .into(),
                ])
                .into(),
            any_i32_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let off = c.arena.alloc(
                IRNode::new(IROpcode::RegClass)
                    .args(vec![m["off"]])
                    .ty(Type::i64)
                    .into(),
            );
            let mem = c
                .arena
                .alloc(MemKind::BaseAlignOff([m["base"], m["align"], off]).into());
            c.arena
                .alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into())
        })
        .into();
    let store2: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::FIAddr)
                        .args(vec![any_slot().named("slot").into()])
                        .into(),
                    any_i32_imm().named("off").into(),
                ])
                .into(),
            any_f64_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(MemKind::BaseFiOff([rbp, m["slot"], m["off"]]).into());
            let src = c.arena.alloc(
                MINode::new(MO::MOVSDrm64)
                    .args(vec![m["src"]])
                    .reg_class(RC::XMM)
                    .into(),
            );
            c.arena
                .alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into())
        })
        .into();
    let sext: Pat = ir(IROpcode::Sext)
        .ty(Type::i64)
        .args(vec![reg_class(RC::GR32).named("arg").into()])
        .generate(|m, c| {
            c.arena.alloc(
                IRNode::new(IROpcode::RegClass)
                    .args(vec![m["arg"]])
                    .ty(Type::i64)
                    .into(),
            )
        })
        .into();

    let pats = vec![load, load2, load3, store, store2, sext];

    let mut replaced = ReplacedNodeMap::default();
    for &id in &func.dag_basic_blocks {
        let block = &func.dag_basic_block_arena[id];
        let a = select_node(
            &mut MatchContext {
                arena: &mut func.node_arena,
                regs: &func.regs,
            },
            &mut replaced,
            &pats,
            block.entry.unwrap(),
        );
        assert_eq!(block.entry.unwrap(), a);
    }
}

fn select_node<'a>(
    ctx: &mut MatchContext<'a>,
    replaced: &mut ReplacedNodeMap,
    pats: &[Pat],
    id: NodeId,
) -> NodeId {
    println!("here");
    let new = inst_select(replaced, ctx, id, pats);

    if let Some(next) = ctx.arena[id].next() {
        let next = select_node(ctx, replaced, pats, next);
        *ctx.arena[new].next_mut() = Some(next);
    }

    new
}
