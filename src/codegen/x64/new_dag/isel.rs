use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
        pat_match::{
            any_block, any_f64_imm, any_i32_imm, any_i8_imm, any_slot, inst_select, ir, reg_class,
            slot, MatchContext, Pat, ReplacedNodeMap,
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
    #[rustfmt::skip]
    let store: Pat = (
                        ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![slot(MVType::i32).named("dst").into()]).into(), any_i32_imm().named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into()) })
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![slot(MVType::i32).named("dst").into()]).into(), reg_class(RC::GR32).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["src"]]).into()) })
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![slot(MVType::i64).named("dst").into()]).into(), reg_class(RC::GR64).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr64).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), any_i32_imm().named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), reg_class(RC::GR32).named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), any_f64_imm().named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["dst"]])).into());
                                                let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["src"]]).reg_class(RC::XMM).into());
                                                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into()) }).into()
                       // (Store (FIAddr slot:f64) src:XMM) -> (MOVSDmr BaseFi(rbp, slot) src)
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), reg_class(RC::XMM).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, m["src"]]).into()) }).into()
                    ).into();

    // TODO: Support GlobalAddr
    let load: Pat = ir(IROpcode::Load)
        .named("load")
        .args(vec![ir(IROpcode::FIAddr)
            .args(vec![(reg_class(RC::GR64) | any_slot()).named("src").into()])
            .into()])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let src = m["src"];
            let mem = c.arena.alloc(
                match c.arena[src].as_operand() {
                    OperandNode::Slot(_) => MemKind::BaseFi(vec![rbp, src]),
                    OperandNode::Reg(_) => MemKind::Base(src),
                    _ => unreachable!(),
                }
                .into(),
            );
            let opcode = match c.arena[m["load"]].as_ir().mvty {
                MVType::Void | MVType::Invalid => panic!(),
                MVType::i1 | MVType::i8 => MO::MOVrm8,
                MVType::i16 => panic!(),
                MVType::i32 => MO::MOVrm32,
                MVType::i64 => MO::MOVrm64,
                MVType::f64 => MO::MOVSDrm,
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
    let mul8: Pat = ir(IROpcode::Mul)
        .named("bin")
        .ty(Type::i8)
        .args(vec![
            reg_class(RC::GR8).named("lhs").into(),
            (reg_class(RC::GR8) | any_i8_imm()).named("rhs").into(),
        ])
        .generate(|m, c| match c.arena[m["rhs"]] {
            Node::Operand(OperandNode::Reg(_)) => {
                let lhs = c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![m["lhs"]]).ty(Type::i32).into());
                let rhs = c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![m["rhs"]]).ty(Type::i32).into());
                let mul = c.arena.alloc(MINode::new(MO::IMULrr32).args(vec![lhs, rhs]).reg_class(RC::GR32).into());
                c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![mul]).ty(Type::i8).into())
            },
            Node::Operand(OperandNode::Imm(_)) => c.arena.alloc(
                MINode::new(MO::IMULrri8)
                    .args(vec![m["lhs"], m["rhs"]])
                    .reg_class(RC::GR8)
                    .into(),
            ),
            _ => panic!(),
        })
        .into();
    #[rustfmt::skip]
    let bin: Pat = {
        let add8  = ir(IROpcode::Add).named("bin").ty(Type::i8) .args(vec![                 reg_class(RC::GR8)  .named("lhs").into(), (reg_class(RC::GR8)  | any_i8_imm() ).named("rhs").into()]);
        let add32 = ir(IROpcode::Add).named("bin").ty(Type::i32).args(vec![                 reg_class(RC::GR32) .named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let add64 = ir(IROpcode::Add).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(), (reg_class(RC::GR64) | any_i32_imm()).named("rhs").into()]);
        let sub8  = ir(IROpcode::Sub).named("bin").ty(Type::i8) .args(vec![(any_i8_imm()  | reg_class(RC::GR8)) .named("lhs").into(), (reg_class(RC::GR8)  | any_i8_imm()) .named("rhs").into()]);
        let sub32 = ir(IROpcode::Sub).named("bin").ty(Type::i32).args(vec![(any_i32_imm() | reg_class(RC::GR32)).named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let sub64 = ir(IROpcode::Sub).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(), (reg_class(RC::GR64) | any_i32_imm()).named("rhs").into()]);
        let mul32 = ir(IROpcode::Mul).named("bin").ty(Type::i32).args(vec![                 reg_class(RC::GR32) .named("lhs").into(), (reg_class(RC::GR32) | any_i32_imm()).named("rhs").into()]);
        let mul64 = ir(IROpcode::Mul).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(),                        any_i32_imm() .named("rhs").into()]);
        let shl32 = ir(IROpcode::Shl).named("bin").ty(Type::i32).args(vec![                 reg_class(RC::GR32) .named("lhs").into(),                        any_i8_imm()  .named("rhs").into()]);
        let shl64 = ir(IROpcode::Shl).named("bin").ty(Type::i64).args(vec![                 reg_class(RC::GR64) .named("lhs").into(),                        any_i8_imm()  .named("rhs").into()]);
                // GR32 a {
                //     GR32  b => (mi.IMULrr32  a, b)
                //     imm32 b => (mi.IMULrri32 a, b) }
                // GR64 a {
                //     imm32 b => (mi.IMULrr64i32 a, b) }
        (((add8 | add32 | add64.into())) | (sub8 | sub32 | sub64.into()) | (mul32 | mul64) | (shl32 | shl64)).generate(|m, c| {
            let ty = c.arena[m["bin"]].as_ir().mvty;
            let lhs = match c.arena[m["lhs"]] {
                Node::Operand(OperandNode::Imm(ImmediateKind::Int8(_))) => 
                    c.arena.alloc(MINode::new(MO::MOVri8).args(vec![m["lhs"]]).reg_class(RC::GR8).into()),
                Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => 
                    c.arena.alloc(MINode::new(MO::MOVri32).args(vec![m["lhs"]]).reg_class(RC::GR32).into()),
                _ => m["lhs"]
            };
            let rhs = &c.arena[m["rhs"]];
            let opcode = match c.arena[m["bin"]].as_ir().opcode {
                IROpcode::Add if matches!(ty, MVType::i8)  && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::ADDri8,
                IROpcode::Add if matches!(ty, MVType::i8)                                                       => MO::ADDrr8,
                IROpcode::Add if matches!(ty, MVType::i32) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::ADDri32,
                IROpcode::Add if matches!(ty, MVType::i32)                                                      => MO::ADDrr32,
                IROpcode::Add if matches!(ty, MVType::i64) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::ADDr64i32,
                IROpcode::Add if matches!(ty, MVType::i64)                                                      => MO::ADDrr64,
                IROpcode::Sub if matches!(ty, MVType::i8)  && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::SUBri8,
                IROpcode::Sub if matches!(ty, MVType::i8)                                                       => MO::SUBrr8,
                IROpcode::Sub if matches!(ty, MVType::i32) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::SUBri32,
                IROpcode::Sub if matches!(ty, MVType::i32)                                                      => MO::SUBrr32,
                IROpcode::Sub if matches!(ty, MVType::i64) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::SUBr64i32,
                IROpcode::Mul if matches!(ty, MVType::i32) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::IMULrri32,
                IROpcode::Mul if matches!(ty, MVType::i32)                                                      => MO::IMULrr32,
                IROpcode::Mul if matches!(ty, MVType::i64) && matches!(rhs, Node::Operand(OperandNode::Imm(_))) => MO::IMULrr64i32,
                IROpcode::Shl if matches!(ty, MVType::i32)                                                      => MO::SHLr32i8,
                IROpcode::Shl if matches!(ty, MVType::i64)                                                      => MO::SHLr64i8,
                _ => panic!() 
            };
            c.arena.alloc(MINode::new(opcode).args(vec![
                lhs, m["rhs"]
            ]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into())
        })
    }.into();
    #[rustfmt::skip]
    let fbin: Pat = {
        let add64 = ir(IROpcode::Add).named("bin").ty(Type::f64).args(vec![reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let sub64 = ir(IROpcode::Sub).named("bin").ty(Type::f64).args(vec![reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let mul64 = ir(IROpcode::Mul).named("bin").ty(Type::f64).args(vec![reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let div64 = ir(IROpcode::Div).named("bin").ty(Type::f64).args(vec![reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        (add64 | sub64 | mul64.into() | div64.into()).generate(|m, c| {
            let lhs = if matches!(c.arena[m["lhs"]], Node::Operand(OperandNode::Imm(_))) {
                c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["lhs"]]).reg_class(RC::XMM).into()) } else { m["lhs"] };
            let rhs = if matches!(c.arena[m["rhs"]], Node::Operand(OperandNode::Imm(_))) {
                c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["rhs"]]).reg_class(RC::XMM).into()) } else { m["rhs"] };
            let opcode = match c.arena[m["bin"]].as_ir().opcode {
                IROpcode::Add => MO::ADDSDrr,
                IROpcode::Sub => MO::SUBSDrr,
                IROpcode::Mul => MO::MULSDrr,
                IROpcode::Div => MO::DIVSDrr,
                _ => panic!() 
            };
            c.arena.alloc(MINode::new(opcode).args(vec![lhs, rhs]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into())
        }).into()
    };

    let br: Pat = ir(IROpcode::Br)
        .args(vec![any_block().named("dst").into()])
        .generate(|m, c| {
            c.arena
                .alloc(MINode::new(MO::JMP).args(vec![m["dst"]]).into())
        })
        .into();

    // (ir.FIAddr a) { mem a => (mi.LEAr64m [BaseFi %rbp, a]) }
    #[rustfmt::skip]
    let fiaddr: Pat = ir(IROpcode::FIAddr).args(vec![any_slot().named("slot").into()])
                                          .generate(|m, c| {
                                              let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                              let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi(vec![rbp, m["slot"]])).into());
                                              c.arena.alloc(MINode::new(MO::LEAr64m).args(vec![mem]).reg_class(RC::GR64).into()) }).into();

    let pats = vec![store, load, mul8, bin, fbin, br, fiaddr];

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
