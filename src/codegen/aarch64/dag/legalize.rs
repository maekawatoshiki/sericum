use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
        pat_match::{
            add, any, any_block, any_cc, any_f64_imm, any_i32_imm, any_imm, any_imm32,
            any_imm32_power_of_2, any_imm_f64, any_reg, any_slot, fiaddr, gbladdr, inst_select, ir,
            mul, not, reg_, reg_class, reorder_patterns, store, CompoundPat, MatchContext, Pat,
            ReplacedNodeMap,
        },
    },
    types::MVType,
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
    let sext: Pat = ir(IROpcode::Sext)
        .named("sext")
        // .ty(Type::i64)
        .args(vec![any_reg().named("arg").into()])
        .generate(|m, c| {
            let ty = c.arena[m["sext"]].as_ir().ty;
            c.arena.alloc(
                IRNode::new(IROpcode::RegClass)
                    .args(vec![m["arg"]])
                    .ty(ty)
                    .into(),
            )
        })
        .into();

    let pats = vec![
        sext,
        // sext, load4, load5, store, load, load2, load3, store2, brcc, fpbrcc, bitcast, load6, store3,
        // store4,
    ];
    let pats = reorder_patterns(pats);

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
    let new = inst_select(replaced, ctx, id, pats);

    if let Some(next) = ctx.arena[id].next() {
        let next = select_node(ctx, replaced, pats, next);
        *ctx.arena[new].next_mut() = Some(next);
    }

    new
}
