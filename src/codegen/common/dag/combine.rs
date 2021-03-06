use crate::codegen::common::dag::{
    function::DAGFunction,
    module::DAGModule,
    node::{IRNode, IROpcode, NodeId},
    pat_match::{
        any, any_block, any_cc, any_i32_imm, any_imm, any_reg, i32_imm, inst_select, ir, not,
        null_imm, MatchContext, Pat, ReplacedNodeMap,
    },
};
use defs::node_gen;

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
            (ir(IROpcode::Setcc).args(args.clone()) | ir(IROpcode::FCmp).args(args.clone()))
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
    let setcc: Pat = ir(IROpcode::Setcc)
        .named("setcc")
        .args(vec![
            any_cc().named("cc").into(),
            any_imm().into(),
            any_reg().into(),
        ])
        .generate(|m, c| {
            let cc = c.arena[m["cc"]].as_operand_mut().as_cc_mut();
            *cc = cc.flip();
            c.arena[m["setcc"]].as_ir_mut().args.swap(1, 2);
            m["setcc"]
        })
        .into();

    let add: Pat = (
        // (IMM + any) -> (any + IMM)
        ir(IROpcode::Add)
            .named("n")
            .args(vec![any_imm().into(), any()])
            .generate(|m, c| {
                c.arena[m["n"]].as_ir_mut().args.swap(0, 1);
                m["n"]
            })
        // (A + 0) -> A
        | ir(IROpcode::Add)
            .args(vec![any().named("l"), null_imm().into()])
            .generate(|m, _| m["l"])
        // ((node + C1) + C2) -> (node + C3)
        | ir(IROpcode::Add).named("add").args(vec![
            ir(IROpcode::Add).args(vec![
                not().any_i32_imm().named("n").into(),
                any_i32_imm().named("c1").into(),
            ]).into(),
            any_i32_imm().named("c2").into()
        ]).generate(|m, c|{
            let c1 = c.arena[m["c1"]].as_operand().as_imm().as_i32();
            let c2 = c.arena[m["c2"]].as_operand().as_imm().as_i32();
            let c3 = c.arena.alloc((c1 + c2).into());
            let ty = c.arena[m["add"]].as_ir().ty;
            node_gen!((IR.Add.(ty) m["n"], c3))
        }).into()
    )
    .into();

    let mul: Pat = (ir(IROpcode::Mul)
        .named("n")
        .args(vec![any_imm().into(), any()])
        .generate(|m, c| {
            c.arena[m["n"]].as_ir_mut().args.swap(0, 1);
            m["n"]
        })
        | ir(IROpcode::Mul)
            .args(vec![any(), i32_imm(0).named("0").into()])
            .generate(|m, _c| m["0"])
        | ir(IROpcode::Mul)
            .args(vec![any().named("n"), i32_imm(1).into()])
            .generate(|m, _c| m["n"])
            .into())
    .into();

    let pats = vec![brcond, setcc, add, mul];

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
