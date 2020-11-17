use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
        pat_match::{
            any, any_block, any_cc, any_f64_imm, any_i32_imm, any_imm, any_reg, any_slot,
            inst_select, ir, reg_class, CompoundPat, MatchContext, Pat, ReplacedNodeMap,
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
    let add_gr64_off: Pat = ir(IROpcode::Add)
        .args(vec![
            reg_class(RC::GR64).named("base").into(),
            any_i32_imm().named("off").into(),
        ])
        .into();

    #[rustfmt::skip]
    // (Load (Add (base:GR64 off:imm32))) -> (MOVrm32 BaseOff(base off))
    let load: Pat = ir(IROpcode::Load)
        .ty(Type::i32)
        .args(vec![add_gr64_off.clone()])
        .generate(|m, c| {
            let mem = c.arena.alloc(MemKind::BaseOff([m["base"], m["off"]]).into());
            c.arena.alloc(MINode::new(MO::MOVrm32).args(vec![mem]).reg_class(RC::GR32).into())}).into();
    // (Load (Add (base:GR64 (Mul (off align:imm32))))) -> (MOVrm32 BaseAlignOff(base align off:GR64))
    #[rustfmt::skip]
    let load2: Pat = ir(IROpcode::Load).named("load")
        // .ty(Type::i32)
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
            let off = c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![m["off"]]).ty(Type::i64).into());
            let mem = c.arena.alloc(MemKind::BaseAlignOff([m["base"], m["align"], off]).into());
            let opcode = match c.arena[m["load"]].as_ir().mvty {
                MVType::i8 => MO::MOVrm8,
                MVType::i32 => MO::MOVrm32,
                _ => todo!()
            };
            c.arena.alloc(MINode::new(opcode).args(vec![mem]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into()) }).into();
    #[rustfmt::skip]
    let load4: Pat = ir(IROpcode::Load).named("load")
        // .ty(Type::i32)
        .args(vec![ir(IROpcode::Add)
            .args(vec![
                ir(IROpcode::GlobalAddr).args(vec![any().named("g").into()]).into(),
                ir(IROpcode::Mul)
                    .args(vec![
                        any().named("off").into(),
                        any_i32_imm().named("align").into(),
                    ])
                    .into(),
            ])
            .into()])
        .generate(|m, c| {
            let off = c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![m["off"]]).ty(Type::i64).into());
            let mem = c.arena.alloc(MemKind::AddressAlignOff([m["g"], m["align"], off]).into());
            let opcode = match c.arena[m["load"]].as_ir().mvty {
                MVType::i8 => MO::MOVrm8,
                MVType::i32 => MO::MOVrm32,
                _ => todo!()
            };
            c.arena.alloc(MINode::new(opcode).args(vec![mem]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into()) }).into();
    #[rustfmt::skip]
    let load5: Pat = ir(IROpcode::Load).named("load")
        .args(vec![ir(IROpcode::Add)
            .args(vec![
                ir(IROpcode::GlobalAddr).args(vec![any().named("g").into()]).into(),
                any_i32_imm().named("off").into(),
            ])
            .into()])
        .generate(|m, c| {
            let mem = c.arena.alloc(MemKind::AddressOff([m["g"], m["off"]]).into());
            let opcode = match c.arena[m["load"]].as_ir().mvty {
                MVType::i8 => MO::MOVrm8,
                MVType::i32 => MO::MOVrm32,
                _ => todo!()
            };
            c.arena.alloc(MINode::new(opcode).args(vec![mem]).reg_class(opcode.inst_def().unwrap().def_reg_class()).into()) }).into();
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

    // {
    //     fn store(src: Pat, dst: Pat) -> Pat {
    //         ir(IROpcode::Store).args(vec![src, dst]).into()
    //     }
    //     fn add(lhs: Pat, rhs: Pat) -> Pat {
    //         ir(IROpcode::Add).args(vec![lhs, rhs]).into()
    //     }
    //     fn fiaddr(slot: Pat) -> Pat {
    //         ir(IROpcode::FIAddr).args(vec![slot]).into()
    //     }
    //     fn mul(lhs: Pat, rhs: Pat) -> Pat {
    //         ir(IROpcode::Mul).args(vec![lhs, rhs]).into()
    //     }
    //     fn sext(arg: Pat) -> Pat {
    //         ir(IROpcode::Sext).args(vec![arg]).into()
    //     }
    //     fn imm32() -> Pat {
    //         any_i32_imm().into()
    //     }
    //     fn reg() -> Pat {
    //         any_reg().into()
    //     }
    //     fn reg_(rc: RC) -> Pat {
    //         reg_class(rc).into()
    //     }
    //     fn slot() -> Pat {
    //         any_slot().into()
    //     }
    //     let _ = store(
    //         add(
    //             fiaddr(slot()),
    //             mul(sext(add(reg().into(), imm32())), imm32()),
    //         ),
    //         imm32(),
    //     );
    //     // let store_add_slot_mul_off_align_gr32: Pat = ir(IROpcode::Store)
    //     let _ = store(
    //         add(fiaddr(slot()), mul(reg_(RC::GR32), imm32())),
    //         reg_(RC::GR32),
    //     );
    // }

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
    let brcc: Pat = ir(IROpcode::Brcc)
        .named("brcc")
        .args(vec![
            any_cc().named("cc").into(),
            any_imm().into(),
            any_reg().into(),
            any_block().into(),
        ])
        .generate(|m, c| {
            let cc = c.arena[m["cc"]].as_operand_mut().as_cc_mut();
            *cc = cc.flip();
            c.arena[m["brcc"]].as_ir_mut().args.swap(1, 2);
            m["brcc"]
        })
        .into();
    #[rustfmt::skip]
    let fpbrcc: Pat = (ir(IROpcode::FPBrcc)
        .named("brcc")
        .args(vec![
            any_cc().named("cc").into(),
            any_imm().into(),
            any_reg().into(),
            any_block().into(),
        ])
        .generate(|m, c| {
            let cc = c.arena[m["cc"]].as_operand_mut().as_cc_mut();
            *cc = cc.flip();
            c.arena[m["brcc"]].as_ir_mut().args.swap(1, 2); // swap imm and reg
            m["brcc"]
        })
        | ir(IROpcode::FPBrcc)
            .args(vec![
                any_cc().named("cc").into(),
                any_reg().named("l").into(),
                any_imm().named("r").into(),
                any_block().named("dst").into(),
            ])
            .generate(|m, c| {
                let r = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["r"]]).reg_class(RC::XMM).into());
                c.arena.alloc(IRNode::new(IROpcode::FPBrcc).args(vec![m["cc"], m["l"], r, m["dst"]]).into())
            }))
    .into();
    #[rustfmt::skip]
    let store: Pat = ir(IROpcode::Store).args(vec![
        ir(IROpcode::Add).args(vec![
            (( ir(IROpcode::FIAddr).args(vec![any_slot().named("slot").into()]).into(): CompoundPat
             | ir(IROpcode::GlobalAddr).args(vec![any().named("g").into()]).into()
             | reg_class(RC::GR64).named("based").into())).named("lhs").into(),
            ( ir(IROpcode::Mul).args(vec![
                any().named("off").into(),
                any_i32_imm().named("align").into()]).into(): CompoundPat
            | any_i32_imm().named("off").into()).named("rhs").into()
        ]).into(),
        (  any_i32_imm().named("imm").into(): CompoundPat 
         | any_f64_imm().named("imm").into()
         | reg_class(RC::GR32).named("reg").into()).named("src").into()
    ]).generate(|m, c| {
        let mem = match c.arena[m["lhs"]] {
            Node::IR(IRNode { opcode: IROpcode::FIAddr, ..}) => {
                let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
                match c.arena[m["rhs"]] {
                    Node::IR(IRNode { opcode: IROpcode::Mul, ..}) => MemKind::BaseFiAlignOff([rbp, m["slot"], m["align"], m["off"]]),
                    Node::Operand(OperandNode::Imm(_)) => MemKind::BaseFiOff([rbp, m["slot"], m["off"]]),
                    _ => unreachable!(),
                }
            }
            Node::IR(IRNode { opcode: IROpcode::GlobalAddr, ..}) => match c.arena[m["rhs"]] {
                Node::IR(IRNode { opcode: IROpcode::Mul, ..}) => MemKind::AddressAlignOff([m["g"], m["align"], m["off"]]),
                Node::Operand(OperandNode::Imm(_)) => MemKind::AddressOff([m["g"], m["off"]]),
                _ => unreachable!(),
            }
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => match c.arena[m["rhs"]] {
                Node::IR(IRNode { opcode: IROpcode::Mul, ..}) => MemKind::BaseAlignOff([m["base"], m["align"], m["off"]]),
                Node::Operand(OperandNode::Imm(_)) => MemKind::BaseOff([m["base"], m["off"]]),
                _ => unreachable!(),
            }
            _ => unreachable!(),
        };
        let mem = c.arena.alloc(mem.into());
        match c.arena[m["src"]] {
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => 
                c.arena.alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["reg"]]).into()),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => 
                c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["imm"]]).into()),
            Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
                let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into())
            }
            _ => unreachable!()
        }
    }).into();
    #[rustfmt::skip]
    let store1: Pat = ir(IROpcode::Store).args(vec![
        ir(IROpcode::Add).args(vec![
            ir(IROpcode::FIAddr).args(vec![any_slot().named("slot").into()]).into(),
            ir(IROpcode::Mul).args(vec![
                (  ir(IROpcode::Sext).args(vec![
                    ir(IROpcode::Add).args(vec![
                        any().named("w").into(),
                        any_i32_imm().named("e").into()]).into()]).into(): CompoundPat
                 | any_i32_imm().named("y").into()
                 | reg_class(RC::GR32).named("y").into()).named("x").into(),
                any_i32_imm().named("m2").into()
            ]).into()]).into(),
        (  any_i32_imm().named("imm").into(): CompoundPat
         | any_f64_imm().named("imm").into()
         | reg_class(RC::GR32).named("reg").into()).named("src").into()]).generate(|m, c| {
        let i = c.arena[m["m2"]].as_operand().as_imm().as_i32();
        let off = c.arena[m["e"]].as_operand().as_imm().as_i32() * i;
        let off = c.arena.alloc(off.into());
        let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
        let mem = if (i as usize).is_power_of_two() && i <= 8 {
            c.arena.alloc(MemKind::BaseFiAlignOffOff([rbp, m["slot"], m["m2"], m["w"], off]).into())
        } else {
            let one = c.arena.alloc(1.into());
            let off2 = c.arena.alloc(MINode::new(MO::IMULrr64i32)
                .args(vec![m["w"], m["m2"]]).reg_class(RC::GR64).into());
            c.arena.alloc(MemKind::BaseFiAlignOffOff([rbp, m["slot"], one, off2, off]).into())
        };
        let mi = match c.arena[m["src"]] {
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => MINode::new(MO::MOVmr32),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => MINode::new(MO::MOVmi32),
            // Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
            //     let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
            //     MINode::new(MO::MOVSDmr)
            // }
            _ => unreachable!()
        };
        c.arena.alloc(mi.args(vec![mem, m["src"]]).into())
    }).into();
    #[rustfmt::skip]
    let store2: Pat = ir(IROpcode::Store).args(vec![
        ir(IROpcode::Add).args(vec![
            ir(IROpcode::FIAddr).args(vec![any_slot().named("slot").into()]).into(),
            ir(IROpcode::Mul).args(vec![
                any().named("m1").into(),
                any_i32_imm().named("m2").into()
            ]).into()]).into(),
        (  any_i32_imm().named("imm").into(): CompoundPat 
         | any_f64_imm().named("imm").into()
         | reg_class(RC::GR32).named("reg").into()).named("src").into()]).generate(|m, c| {
        let i = c.arena[m["m2"]].as_operand().as_imm().as_i32() as usize;
        let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
        let mem = if i.is_power_of_two() && i <= 8 {
            c.arena.alloc(MemKind::BaseFiAlignOff([rbp, m["slot"], m["m2"], m["m1"]]).into())
        } else {
            let one = c.arena.alloc(1.into());
            let mul = c.arena.alloc(MINode::new(MO::IMULrr64i32).args(vec![m["m1"], m["m2"]]).reg_class(RC::GR64).into());
            c.arena.alloc(MemKind::BaseFiAlignOff([rbp, m["slot"], one, mul]).into())
        };
        let mi = match c.arena[m["src"]] {
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => MINode::new(MO::MOVmr32),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => MINode::new(MO::MOVmi32),
            // Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
            //     let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
            //     MINode::new(MO::MOVSDmr)
            // }
            _ => unreachable!()
        };
        c.arena.alloc(mi.args(vec![mem, m["src"]]).into())
    }).into();
    let bitcast: Pat = ir(IROpcode::Bitcast)
        .args(vec![any().named("arg").into()])
        .generate(|m, _| m["arg"])
        .into();

    let pats = vec![
        load4, load5, load, load2, load3, store1, store2, store, sext, brcc, fpbrcc, bitcast,
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
