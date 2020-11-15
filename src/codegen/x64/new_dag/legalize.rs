use crate::codegen::arch::machine::register::{RegisterClassKind as RC, GR64};
use crate::codegen::arch::{dag::node::MemKind, machine::inst::MachineOpcode as MO};
use crate::codegen::common::{
    new_dag::{
        function::DAGFunction,
        module::DAGModule,
        node::{IRNode, IROpcode, MINode, NodeId, OperandNode},
        pat_match::{
            any, any_block, any_cc, any_f64_imm, any_i32_imm, any_imm, any_reg, any_slot,
            inst_select, ir, reg_class, MatchContext, Pat, ReplacedNodeMap,
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
        .args(vec![add_gr64_off])
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
    let store: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    reg_class(RC::GR64).named("base").into(),
                    ir(IROpcode::Mul)
                        .args(vec![
                            any().named("off").into(),
                            any_i32_imm().named("align").into(),
                        ])
                        .into(),
                ])
                .into(),
            any_i32_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let off = c.arena.alloc(
                IRNode::new(IROpcode::RegClass)
                    .args(vec![m["off"]])
                    .ty(Type::i64)
                    .into(),
            );
            let mem = c
                .arena
                .alloc(MemKind::BaseAlignOff([m["base"], m["align"], off]).into());
            c.arena
                .alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into())
        })
        .into();
    let store2: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::FIAddr)
                        .args(vec![any_slot().named("slot").into()])
                        .into(),
                    any_i32_imm().named("off").into(),
                ])
                .into(),
            any_f64_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(MemKind::BaseFiOff([rbp, m["slot"], m["off"]]).into());
            let src = c.arena.alloc(
                MINode::new(MO::MOVSDrm64)
                    .args(vec![m["src"]])
                    .reg_class(RC::XMM)
                    .into(),
            );
            c.arena
                .alloc(MINode::new(MO::MOVSDmr).args(vec![mem, src]).into())
        })
        .into();
    let store3: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::FIAddr)
                        .args(vec![any_slot().named("slot").into()])
                        .into(),
                    ir(IROpcode::Mul)
                        .args(vec![
                            any().named("off").into(),
                            any_i32_imm().named("align").into(),
                        ])
                        .into(),
                ])
                .into(),
            reg_class(RC::GR32).named("src").into(),
        ])
        .generate(|m, c| {
            let rbp = c.arena.alloc(c.regs.get_phys_reg(GR64::RBP).into());
            let mem = c
                .arena
                .alloc(MemKind::BaseFiAlignOff([rbp, m["slot"], m["align"], m["off"]]).into());
            c.arena
                .alloc(MINode::new(MO::MOVmr32).args(vec![mem, m["src"]]).into())
        })
        .into();
    #[rustfmt::skip]
    let store4: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::GlobalAddr).args(vec![any().named("g").into()]).into(),
                    ir(IROpcode::Mul)
                        .args(vec![
                            any().named("off").into(),
                            any_i32_imm().named("align").into(),
                        ])
                        .into(),
                ])
                .into(),
            any_i32_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let off = c.arena.alloc(IRNode::new(IROpcode::RegClass).args(vec![m["off"]]).ty(Type::i64).into());
            let mem = c
                .arena
                .alloc(MemKind::AddressAlignOff([m["g"], m["align"], off]).into());
            c.arena
                .alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into())
        })
        .into();
    #[rustfmt::skip]
    let store5: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    ir(IROpcode::GlobalAddr).args(vec![any().named("g").into()]).into(),
                    any_i32_imm().named("off").into(),
                ])
                .into(),
            any_i32_imm().named("src").into(),
        ])
        .generate(|m, c| {
            let mem = c
                .arena
                .alloc(MemKind::AddressOff([m["g"], m["off"]]).into());
            c.arena.alloc(MINode::new(MO::MOVmi32).args(vec![mem, m["src"]]).into())
        })
        .into();
    #[rustfmt::skip]
    let store6: Pat = ir(IROpcode::Store)
        .args(vec![
            ir(IROpcode::Add)
                .args(vec![
                    reg_class(RC::GR64).named("base").into(),
                    any_i32_imm().named("off").into(),
                ])
                .into(),
            reg_class(RC::GR64).named("src").into(),
        ])
        .generate(|m, c| {
            let mem = c
                .arena
                .alloc(MemKind::BaseOff([m["base"], m["off"]]). into());
            c.arena.alloc(MINode::new(MO::MOVmi64).args(vec![mem, m["src"]]).into())
        })
        .into();
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

    let pats = vec![
        load4, load5, load, load2, load3, store4, store5, store, store2, store3, store6, sext,
        brcc, fpbrcc,
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
