use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IROpcode, MINode, NodeId, OperandNode},
        pat_match::{
            any_block, any_i32_imm, any_slot, inst_select, ir, reg_class, slot, MatchContext, Pat,
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

    #[rustfmt::skip]
    let bin: Pat = {
        let add32 = ir(IROpcode::Add).named("bin").ty(Type::i32).args(vec![                 reg_class(RC::GR32) .named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let add64 = ir(IROpcode::Add).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(), (reg_class(RC::GR64) | any_i32_imm()).named("rhs").into()]);
        let sub32 = ir(IROpcode::Sub).named("bin").ty(Type::i32).args(vec![(any_i32_imm() | reg_class(RC::GR32)).named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let sub64 = ir(IROpcode::Sub).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(), (reg_class(RC::GR64) | any_i32_imm()).named("rhs").into()]);
        let mul32 = ir(IROpcode::Mul).named("bin").ty(Type::i32).args(vec![                 reg_class(RC::GR32) .named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let mul64 = ir(IROpcode::Mul).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(),                        any_i32_imm() .named("rhs").into()]);
                // GR32 a {
                //     GR32  b => (mi.IMULrr32  a, b)
                //     imm32 b => (mi.IMULrri32 a, b) }
                // GR64 a {
                //     imm32 b => (mi.IMULrr64i32 a, b) }
        ((add32 | add64) | (sub32 | sub64) | (mul32 | mul64)).generate(|m, c| {
            let ty = c.arena[m["bin"]].as_ir().mvty;
            let lhs = if matches!(c.arena[m["lhs"]].as_operand(), OperandNode::Imm(_)) {
                c.arena.alloc(MINode::new(MachineOpcode::MOVri32).args(vec![m["lhs"]]).into()) } else { m["lhs"] };
            let rhs = c.arena[m["rhs"]].as_operand();
            let opcode = match c.arena[m["bin"]].as_ir().opcode {
                IROpcode::Add if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::ADDri32,
                IROpcode::Add if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Reg(_)) => MachineOpcode::ADDrr32,
                IROpcode::Add if matches!(ty, MVType::i64) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::ADDr64i32,
                IROpcode::Add if matches!(ty, MVType::i64) && matches!(rhs, OperandNode::Reg(_)) => MachineOpcode::ADDrr64,
                IROpcode::Sub if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::SUBri32,
                IROpcode::Sub if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Reg(_)) => MachineOpcode::SUBrr32,
                IROpcode::Sub if matches!(ty, MVType::i64) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::SUBr64i32,
                // IROpcode::Sub if matches!(ty, MVType::i64) && matches!(op, OperandNode::Reg(_)) => MachineOpcode::SUBrr64,
                IROpcode::Mul if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::IMULrri32,
                IROpcode::Mul if matches!(ty, MVType::i32) && matches!(rhs, OperandNode::Reg(_)) => MachineOpcode::IMULrr32,
                IROpcode::Mul if matches!(ty, MVType::i64) && matches!(rhs, OperandNode::Imm(_)) => MachineOpcode::IMULrr64i32,
                _ => panic!() 
            };
            c.arena.alloc(MINode::new(opcode).args(vec![
                lhs, m["rhs"]
            ]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into())
        }).into()
    };

    let br: Pat = ir(IROpcode::Br)
        .args(vec![any_block().named("dst").into()])
        .generate(|m, c| {
            c.arena
                .alloc(MINode::new(MachineOpcode::JMP).args(vec![m["dst"]]).into())
        })
        .into();

    let pats = vec![store, load, bin, br];

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
