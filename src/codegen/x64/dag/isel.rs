use crate::codegen::arch::machine::register::{str2reg, RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
        pat_match::{
            any, any_block, any_f64_imm, any_i32_imm, any_i8_imm, any_slot, inst_select, ir,
            reg_class, CompoundPat, MatchContext, Pat, ReplacedNodeMap,
        },
    },
    types::MVType,
};
use crate::ir::types::Type;
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
    #[rustfmt::skip]
    let store: Pat = (
                        ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), any_i32_imm().named("src").into()])
                                           .generate(|m, c| node_gen!((MI.MOVmi32 [BaseFi %rbp, m["dst"]], m["src"])))
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), reg_class(RC::GR32).named("src").into()])
                                           .generate(|m, c| node_gen!((MI.MOVmr32 [BaseFi %rbp, m["dst"]], m["src"])))
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), any_i8_imm().named("src").into()])
                                           .generate(|m, c| node_gen!((MI.MOVmr8  [BaseFi %rbp, m["dst"]], m["src"]))).into()
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), reg_class(RC::GR8).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi([rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr8).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst").into()]).into(), reg_class(RC::GR64).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi([rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr64).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst")]).into(), any_f64_imm().named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi([rbp, m["dst"]])).into());
                                                let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["src"]]).reg_class(RC::XMM).into());
                                                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into()) }).into()
                       // (Store (FIAddr slot:f64) src:XMM) -> (MOVSDmr BaseFi(rbp, slot) src)
                      | ir(IROpcode::Store).args(vec![ir(IROpcode::FIAddr).args(vec![any_slot().named("dst")]).into(), reg_class(RC::XMM).named("src").into()])
                                           .generate(|m, c| {
                                                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::BaseFi([rbp, m["dst"]])).into());
                                                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), any_i32_imm().named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), reg_class(RC::GR32).named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), reg_class(RC::XMM).named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, m["src"]]).into()) }).into()
                      | ir(IROpcode::Store).args(vec![reg_class(RC::GR64).named("dst").into(), reg_class(RC::GR64).named("src").into()])
                                           .generate(|m, c| {
                                                let mem = c.arena.alloc(OperandNode::Mem(MemKind::Base(m["dst"])).into());
                                                c.arena.alloc(MINode::new(MO::MOVmr64).args(vec![mem, m["src"]]).into()) }).into()
                    ).into();

    // TODO: Support GlobalAddr
    let load: Pat = ir(IROpcode::Load)
        .named("load")
        .args(vec![(ir(IROpcode::FIAddr)
            .args(vec![any_slot().named("src")])
            .into(): CompoundPat
            | reg_class(RC::GR64).named("src").into())
        .into()])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let src = m["src"];
            let mem = c.arena.alloc(
                match c.arena[src] {
                    Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => {
                        MemKind::Base(src)
                    }
                    Node::Operand(OperandNode::Slot(_)) => MemKind::BaseFi([rbp, src]),
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

    use crate::codegen::common::dag::pat_match::{any_imm8, mul, reg_};

    let mul8: Pat = mul(
        reg_(RC::GR8).named("lhs"),
        (reg_(RC::GR8) | any_imm8()).named("rhs"),
    )
    .ty(Type::i8)
    .generate(|m, c| match c.arena[m["rhs"]] {
        Node::Operand(OperandNode::Reg(_)) => node_gen!(
                                                 (IR.RegClass.(Type::i8)
                                                     (MI.IMULrr32
                                                         (IR.RegClass.(Type::i32) m["lhs"]),
                                                         (IR.RegClass.(Type::i32) m["rhs"])))),
        Node::Operand(OperandNode::Imm(_)) => node_gen!((MI.IMULrri8 m["lhs"], m["rhs"])),
        _ => panic!(),
    });

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
        let add64 = ir(IROpcode::Add).named("bin").ty(Type::f64).args(vec![                 reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let sub64 = ir(IROpcode::Sub).named("bin").ty(Type::f64).args(vec![(any_f64_imm() | reg_class(RC::XMM)).named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let mul64 = ir(IROpcode::Mul).named("bin").ty(Type::f64).args(vec![                 reg_class(RC::XMM) .named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
        let div64 = ir(IROpcode::Div).named("bin").ty(Type::f64).args(vec![(any_f64_imm() | reg_class(RC::XMM)).named("lhs").into(), (reg_class(RC::XMM) | any_f64_imm()).named("rhs").into()]);
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
        .generate(|m, c| node_gen!((MI.JMP m["dst"])))
        .into();

    let fiaddr: Pat = ir(IROpcode::FIAddr)
        .args(vec![any_slot().named("slot").into()])
        .generate(|m, c| node_gen!((MI.LEAr64m [BaseFi %rbp, m["slot"]])))
        .into();
    let constaddr: Pat = ir(IROpcode::ConstAddr)
        .args(vec![any().named("a")])
        .generate(|m, c| node_gen!((MI.MOVrm64 [Address m["a"]])))
        .into();
    let fptosi: Pat = ir(IROpcode::FPToSI)
        .ty(Type::i32)
        .args(vec![reg_class(RC::XMM).named("x").into()])
        .generate(|m, c| node_gen!((MI.CVTTSD2SIr32r m["x"])))
        .into();
    let sitofp: Pat = ir(IROpcode::SIToFP)
        .ty(Type::f64)
        .args(vec![reg_class(RC::GR32).named("x").into()])
        .generate(|m, c| node_gen!((MI.CVTSI2SDrr32 m["x"])))
        .into();

    let pats = vec![
        store, load, mul8, bin, fbin, br, fiaddr, constaddr, fptosi, sitofp,
    ];

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
