use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode};
use crate::codegen::common::new_dag::{
    function::DAGFunction,
    module::DAGModule,
    node::{IRNode, IROpcode, MINode, NodeId, OperandNode},
    pat_match::{
        any, any_block, any_cc, any_imm, any_reg, inst_select, ir, null_imm, reg_class, slot,
        MatchContext, Pat, ReplacedNodeMap,
    },
};
// use crate::ir::types::Type;

pub fn run(module: &mut DAGModule) {
    for (_, func) in &mut module.functions {
        if func.is_internal {
            continue;
        }
        run_on_function(func);
    }
}

fn run_on_function(func: &mut DAGFunction) {
    let args = vec![
        any_cc().named("cc").into(),
        (any_imm() | any_reg()).named("lhs").into(),
        (any_imm() | any_reg()).named("rhs").into(),
    ];
    let brcond: Pat = ir(IROpcode::BrCond)
        .args(vec![
            (ir(IROpcode::Setcc).args(args.clone()) | ir(IROpcode::FCmp).args(args))
                .named("setcc")
                .into(),
            any_block().named("dst").into(),
        ])
        .generate(|m, c| {
            c.arena.alloc(
                IRNode::new(match c.arena[m["setcc"]].as_ir().opcode {
                    IROpcode::Setcc => IROpcode::Brcc,
                    IROpcode::FCmp => IROpcode::FPBrcc,
                    _ => unreachable!(),
                })
                .args(vec![m["cc"], m["lhs"], m["rhs"], m["dst"]])
                .into(),
            )
        })
        .into();

    let add: Pat = (
        // (IMM + any) -> (any + IMM)
        ir(IROpcode::Add)
            .named("n")
            .args(vec![any_imm().into(), any().into()])
            .generate(|m, c| {
                c.arena[m["n"]].as_ir_mut().args.swap(0, 1);
                m["n"]
            })
        // (A + 0) -> A
        | ir(IROpcode::Add)
            .args(vec![any().named("l").into(), null_imm().into()])
            .generate(|m, _| m["l"])
    )
    .into();

    let mul: Pat = (ir(IROpcode::Mul)
        .named("n")
        .args(vec![any_imm().into(), any().into()]))
    .generate(|m, c| {
        c.arena[m["n"]].as_ir_mut().args.swap(0, 1);
        m["n"]
    })
    .into();

    let pats = vec![brcond, add, mul];

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
