use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::new_dag::{
    function::DAGFunction,
    module::DAGModule,
    node::{IRNode, IROpcode, MINode, NodeId, OperandNode},
    pat_match::{
        any, any_cc, any_i32_imm, any_imm, any_reg, inst_select, ir, null_imm, reg_class, slot,
        MatchContext, Pat, ReplacedNodeMap,
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
    let pats = vec![load];

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
