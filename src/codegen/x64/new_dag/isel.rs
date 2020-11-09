use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IROpcode, MINode, NodeId, OperandNode},
        pat_match::{
            any_i32_imm, any_i64_imm, any_slot, inst_select, ir, reg_class, slot, MatchContext,
            Pat, ReplacedNodeMap,
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
    let store: Pat = ir(IROpcode::Store)
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

    // TODO: Support GlobalAddr
    let load: Pat = ir(IROpcode::Load)
        .named("load")
        .args(vec![ir(IROpcode::FIAddr)
            .args(vec![(reg_class(RC::GR64) | any_slot()).named("src").into()])
            .into()])
        .ty(Type::i32)
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let src = m["src"];
            let mem = c.arena.alloc(
                match c.arena[src].as_operand() {
                    OperandNode::Slot(_) => MemKind::BaseFi(rbp, src),
                    OperandNode::Reg(_) => MemKind::Base(src),
                    _ => unreachable!(),
                }
                .into(),
            );
            let opcode = match c.arena[m["load"]].as_ir().mvty {
                MVType::Void | MVType::Invalid => panic!(),
                MVType::i1 | MVType::i8 => MachineOpcode::MOVrm8,
                MVType::i16 => panic!(),
                MVType::i32 => MachineOpcode::MOVrm32,
                MVType::i64 => MachineOpcode::MOVrm64,
                MVType::f64 => MachineOpcode::MOVSDrm,
                MVType::f32 => panic!(),
            };
            c.arena.alloc(
                MINode::new(opcode)
                    .args(vec![mem])
                    .reg_class(opcode.inst_def().unwrap().def_reg_class())
                    .into(),
            )
        })
        .into();

    // let bin: Pat = ir(IROpcode::Add).args(vec![any_reg().into(),
    #[rustfmt::skip]
    let bin: Pat = {
        let addi32 = ir(IROpcode::Add).named("bin").ty(Type::i32).args(vec![reg_class(RC::GR32).named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let addi64 = ir(IROpcode::Add).named("bin").ty(Type::i64).args(vec![reg_class(RC::GR64).named("lhs").into(), (reg_class(RC::GR64) | any_i64_imm()).named("rhs").into()]);
        (addi32 | addi64).generate(|m, c| {
            let ty = c.arena[m["bin"]].as_ir().mvty;
            let op = c.arena[m["rhs"]].as_operand();
            let opcode = match c.arena[m["bin"]].as_ir().opcode {
                IROpcode::Add if matches!(ty, MVType::i32) && matches!(op, OperandNode::Imm(_)) => MachineOpcode::ADDri32,
                IROpcode::Add if matches!(ty, MVType::i32) && matches!(op, OperandNode::Reg(_)) => MachineOpcode::ADDrr32,
                IROpcode::Add if matches!(ty, MVType::i64) && matches!(op, OperandNode::Imm(_)) => MachineOpcode::ADDr64i32,
                IROpcode::Add if matches!(ty, MVType::i64) && matches!(op, OperandNode::Reg(_)) => MachineOpcode::ADDr64i32,
                _ => panic!() 
            };
            c.arena.alloc(MINode::new(opcode).args(vec![
                m["lhs"],
                m["rhs"]
            ]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into())
        }).into()
    };
    // let bin: Pat = addi32.

    let pats = vec![store, load, bin];

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
