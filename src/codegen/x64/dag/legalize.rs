use crate::codegen::arch::machine::register::{str2reg, RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
        pat_match::{
            add, any, any_block, any_cc, any_f64_imm, any_i32_imm, any_imm, any_imm32,
            any_imm32_power_of_2, any_imm_f64, any_reg, any_slot, fiaddr, gbladdr, inst_select, ir,
            load, mul, not, reg_, reg_class, reorder_patterns, sext, store, CompoundPat,
            MatchContext, Pat, ReplacedNodeMap,
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
    let add_gr64_off: Pat = ir(IROpcode::Add)
        .args(vec![
            reg_class(RC::GR64).named("base").into(),
            any_i32_imm().named("off").into(),
        ])
        .into();

    // (Load (Add (base:GR64 off:imm32))) -> (MOVrm32 BaseOff(base off))

    let load1 = load(add_gr64_off)
        .ty(Type::i32)
        .generate(|m, c| node_gen!((MI.MOVrm32 [BaseOff m["base"], m["off"]])));
    let load2 = load(add(
        reg_(RC::GR64).named("base"),
        mul(any().named("off"), any_imm32().named("align")),
    ))
    .named("load")
    .generate(|m, c| {
        let opcode = match c.arena[m["load"]].as_ir().mvty {
            MVType::i8 => MO::MOVrm8,
            MVType::i32 => MO::MOVrm32,
            _ => todo!(),
        };
        node_gen!((MI.(opcode) [BaseAlignOff m["base"], m["align"], (IR.RegClass.(Type::i64) m["off"])]))
    });
    let load4 = load(add(
        gbladdr(any().named("g")),
        mul(any().named("off"), any_imm32().named("align")),
    ))
    .named("load")
    .generate(|m, c| {
        let opcode = match c.arena[m["load"]].as_ir().mvty {
            MVType::i8 => MO::MOVrm8,
            MVType::i32 => MO::MOVrm32,
            _ => todo!(),
        };
        node_gen!((MI.(opcode) [AddressAlignOff m["g"], m["align"], (IR.RegClass.(Type::i64) m["off"])]))
    })
    .into();
    let load5 = load(add(gbladdr(any().named("g")), any_imm32().named("off")))
        .named("load")
        .generate(|m, c| {
            node_gen!((MI.(match c.arena[m["load"]].as_ir().mvty {
                MVType::i8 => MO::MOVrm8,
                MVType::i32 => MO::MOVrm32,
                _ => todo!(),
            }) [AddressOff m["g"], m["off"]]))
        });
    let load6 = load(add(
        not().any_i32_imm().named("base").into(),
        any_imm32().named("off"),
    ))
    .named("load")
    .generate(|m, c| {
        node_gen!((MI.(match c.arena[m["load"]].as_ir().mvty {
            MVType::i8 => MO::MOVrm8,
            MVType::i32 => MO::MOVrm32,
            MVType::i64 => MO::MOVrm64,
            MVType::f64 => MO::MOVSDrm,
            _ => todo!(),
        }) [BaseOff m["base"], m["off"]]))
    });
    let load3 = load(add(
        fiaddr(any_slot().named("slot")),
        any_imm32().named("off"),
    ))
    .ty(Type::f64)
    .generate(|m, c| node_gen!((MI.MOVSDrm [BaseFiOff %rbp, m["slot"], m["off"]])));

    let sext1 = sext(any_reg().named("arg").into())
        .named("sext")
        .generate(|m, c| node_gen!((IR.RegClass.(c.arena[m["sext"]].as_ir().ty) m["arg"])));
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
            .generate(
                |m, c| node_gen!((IR.FPBrcc m["cc"], m["l"], (MI.MOVSDrm64 m["r"]), m["dst"])),
            ))
    .into();

    #[rustfmt::skip]
    let store1: Pat = store(
        add(
            (fiaddr(any_slot().named("slot")) | gbladdr(any().named("g")) | reg_(RC::GR64).named("base")).named("lhs"),
            (mul(any().named("off"), any_imm32_power_of_2().named("align")) | any_imm32().named("off")).named("rhs"),
        ),
        (any_imm32().named("imm") | any_imm_f64().named("imm") | reg_(RC::GR32).named("reg")).named("src"),
    ).generate(|m, c| {
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
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => c.arena.alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["reg"]]).into()),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["imm"]]).into()),
            Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
                let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
                c.arena.alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into())
            }
            _ => unreachable!()
        }
    }).into();
    #[rustfmt::skip]
    let store2: Pat = ir(IROpcode::Store).args(vec![
        ir(IROpcode::Add).args(vec![
            ir(IROpcode::FIAddr).args(vec![any_slot().named("slot").into()]).into(),
            ir(IROpcode::Mul).args(vec![
                any().named("m1"),
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
    let store3: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::FIAddr)
                        .args(vec![any_slot().named("slot").into()])
                        .into(),
                    any_i32_imm().named("off").into(),
                ])
                .into(),
            (any_i32_imm().named("imm").into(): CompoundPat
                | any_f64_imm().named("imm").into()
                | reg_class(RC::GR32).named("reg").into())
            .named("src")
            .into(),
        ])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(MemKind::BaseFiOff([rbp, m["slot"], m["off"]]).into())
                .into();
            let mi = match c.arena[m["src"]] {
                Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => {
                    MINode::new(MO::MOVmr32)
                }
                Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => {
                    MINode::new(MO::MOVmi32)
                }
                // Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
                //     let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
                //     MINode::new(MO::MOVSDmr)
                // }
                _ => unreachable!(),
            };
            c.arena.alloc(mi.args(vec![mem, m["src"]]).into())
        })
        .into();
    let store4 = store(
        add(reg_(RC::GR64).named("base"), any_imm32().named("off")),
        (any_imm32().named("imm")
            | any_imm_f64().named("imm")
            | reg_(RC::GR32).named("reg")
            | reg_(RC::GR64).named("reg")
            | reg_(RC::XMM).named("reg"))
        .named("src"),
    )
    .generate(|m, c| {
        let mem = c
            .arena
            .alloc(MemKind::BaseOff([m["base"], m["off"]]).into())
            .into();
        let mi = match c.arena[m["src"]] {
            Node::IR(IRNode {
                mvty: MVType::i64, ..
            })
            | Node::MI(MINode {
                reg_class: Some(RC::GR64),
                ..
            }) => MINode::new(MO::MOVmr64),
            Node::Operand(OperandNode::Reg(r)) if c.regs.arena_ref()[r].reg_class == RC::GR64 => {
                MINode::new(MO::MOVmr64)
            }
            Node::IR(IRNode {
                mvty: MVType::f64, ..
            })
            | Node::MI(MINode {
                reg_class: Some(RC::XMM),
                ..
            }) => MINode::new(MO::MOVSDmr),
            Node::Operand(OperandNode::Reg(r)) if c.regs.arena_ref()[r].reg_class == RC::XMM => {
                MINode::new(MO::MOVSDmr)
            }
            Node::IR(_) | Node::MI(_) | Node::Operand(OperandNode::Reg(_)) => {
                MINode::new(MO::MOVmr32)
            }
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => MINode::new(MO::MOVmi32),
            // Node::Operand(OperandNode::Imm(ImmediateKind::F64(_))) => {
            //     let src = c.arena.alloc(MINode::new(MO::MOVSDrm64).args(vec![m["imm"]]).reg_class(RC::XMM).into());
            //     MINode::new(MO::MOVSDmr)
            // }
            _ => unreachable!(),
        };
        c.arena.alloc(mi.args(vec![mem, m["src"]]).into())
    })
    .into();
    let bitcast: Pat = ir(IROpcode::Bitcast)
        .args(vec![any().named("arg")])
        .generate(|m, _| m["arg"])
        .into();

    let pats = vec![
        load4, load5, load6, load1, load2, load3, store2, store1, sext1, brcc, fpbrcc, store3,
        store4,
        bitcast,
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
