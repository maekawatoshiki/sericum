use crate::codegen::arch::machine::register::GR64;
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, MINode, Node, NodeId, OperandNode},
        pat_match::{
            any_i32_imm, any_slot, inst_select, ir, not, slot, MatchContext, Pat, ReplacedNodeMap,
        },
    },
    types::MVType,
};

pub fn run(module: &mut DAGModule) {
    for (_, func) in &mut module.functions {
        if func.is_internal {
            continue;
        }
        run_on_function(func);
    }
}

fn run_on_function(func: &mut DAGFunction) {
    let pat: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::FIAddr)
                .args(vec![slot(MVType::i32).named("dst").into()])
                .into(),
            any_i32_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(OperandNode::Mem(MemKind::BaseFi(rbp, m["dst"])).into());
            c.arena.alloc(
                MINode::new(MachineOpcode::MOVmi32)
                    .args(vec![mem, m["src"]])
                    .into(),
            )
        })
        .into();
    let pats = vec![pat];
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
