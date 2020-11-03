use crate::codegen::arch::machine::register::*;
use crate::codegen::common::machine::inst_def::*;

#[allow(non_upper_case_globals)]
mod inst {
    use super::*;

    // TODO: need macro to describe the followings
    lazy_static! {
        pub static ref MOVSDrm64: TargetInstDef = {
            TargetInstDef::new("movsd", TargetOpcode::MOVSDrm64)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        };
        pub static ref MOVSDrr: TargetInstDef = {
            TargetInstDef::new("movsd", TargetOpcode::MOVSDrr)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                    RegisterClassKind::XMM,
                ))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        };
        pub static ref MOVSDrm: TargetInstDef = {
            TargetInstDef::new("movsd",TargetOpcode::MOVSDrm)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        };
        pub static ref MOVSDmr: TargetInstDef = {
            TargetInstDef::new("movsd",TargetOpcode::MOVSDmr).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
            ])
        };
        pub static ref MOVSXr32r8: TargetInstDef = {
            TargetInstDef::new("movsx", TargetOpcode::MOVSXr32r8)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref MOVSXDr64m32: TargetInstDef = {
            TargetInstDef::new("movsxd", TargetOpcode::MOVSXDr64m32)
                .set_uses(vec![TargetOperand::FrameIndex])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref MOVSXDr64r32: TargetInstDef = {
            TargetInstDef::new("movsxd", TargetOpcode::MOVSXDr64r32)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref LEAr64m: TargetInstDef = {
            TargetInstDef::new("lea", TargetOpcode::LEAr64m)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref ADDrr8: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDrr8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDri8: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDri8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                    TargetOperand::Immediate(TargetImmediate::I8)
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDrr32: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDrr32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDmr32: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDmr32)
                .set_uses(vec![
                    TargetOperand::Mem,
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref ADDmi32: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDmi32)
                .set_uses(vec![
                    TargetOperand::Mem,
                    TargetOperand::Immediate(TargetImmediate::I32)
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref ADDrr64: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDrr64)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDri32: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDri32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDr64i32: TargetInstDef = {
            TargetInstDef::new("add", TargetOpcode::ADDr64i32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDSDrr: TargetInstDef = {
            TargetInstDef::new("addsd", TargetOpcode::ADDSDrr)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref ADDSDrm: TargetInstDef = {
            TargetInstDef::new("addsd", TargetOpcode::ADDSDrm)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Mem,
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBrr8: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBrr8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBri8: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBri8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                    TargetOperand::Immediate(TargetImmediate::I8),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBrr32: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBrr32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBri32: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBri32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBr64i32: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBr64i32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBmr32: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBmr32)
                .set_uses(vec![
                    TargetOperand::Mem,
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref SUBmi32: TargetInstDef = {
            TargetInstDef::new("sub", TargetOpcode::SUBmi32)
                .set_uses(vec![
                    TargetOperand::Mem,
                    TargetOperand::Immediate(TargetImmediate::I32)
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref SUBSDrr: TargetInstDef = {
            TargetInstDef::new("subsd", TargetOpcode::SUBSDrr)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SUBSDrm: TargetInstDef = {
            TargetInstDef::new("subsd", TargetOpcode::SUBSDrm)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Mem,
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        // pub static ref IMULrr8: TargetInstDef = {
        //     TargetInstDef::new("imul", TargetOpcode::IMULrr8)
        //         .set_uses(vec![
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
        //             TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
        //         ])
        //         .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
        //         .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        // };
        pub static ref IMULrri8: TargetInstDef = {
            TargetInstDef::new("imul", TargetOpcode::IMULrri8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
                    TargetOperand::Immediate(TargetImmediate::I8),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
        };
        pub static ref IMULrr32: TargetInstDef = {
            TargetInstDef::new("imul", TargetOpcode::IMULrr32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref IMULrri32: TargetInstDef = {
            TargetInstDef::new("imul", TargetOpcode::IMULrri32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref IMULrr64i32: TargetInstDef = {
            TargetInstDef::new("imul", TargetOpcode::IMULrr64i32)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                    TargetOperand::Immediate(TargetImmediate::I32),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref MULSDrr: TargetInstDef = {
            TargetInstDef::new("mulsd", TargetOpcode::MULSDrr)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref MULSDrm: TargetInstDef = {
            TargetInstDef::new("mulsd", TargetOpcode::MULSDrm)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Mem,
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref DIVSDrr: TargetInstDef = {
            TargetInstDef::new("divsd", TargetOpcode::DIVSDrr)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref DIVSDrm: TargetInstDef = {
            TargetInstDef::new("divsd", TargetOpcode::DIVSDrm)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Mem,
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SHLr32i8: TargetInstDef = {
            TargetInstDef::new("shl", TargetOpcode::SHLr32i8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
                    TargetOperand::Immediate(TargetImmediate::I8),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SHLr64i8: TargetInstDef = {
            TargetInstDef::new("shl", TargetOpcode::SHLr64i8)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
                    TargetOperand::Immediate(TargetImmediate::I8),
                ])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
                .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref SQRTSDrr: TargetInstDef = {
            TargetInstDef::new("sqrtsd", TargetOpcode::SQRTSDrr)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                    RegisterClassKind::XMM,
                ))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
                // .add_tie(DefOrUseReg::Def(0), DefOrUseReg::Use(0))
        };
        pub static ref CDQ: TargetInstDef = {
            TargetInstDef::new("cdq", TargetOpcode::CDQ)
                .set_imp_def(vec![TargetRegister::Specific(GR32::EDX.as_phys_reg())])
                .set_imp_use(vec![TargetRegister::Specific(GR32::EAX.as_phys_reg())])
        };
        pub static ref CVTSI2SDrr32: TargetInstDef = {
            TargetInstDef::new("cvtsi2sd", TargetOpcode::CVTSI2SDrr32)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::XMM)])
        };
        pub static ref CVTTSD2SIr32r: TargetInstDef = {
            TargetInstDef::new("cvttsd2si", TargetOpcode::CVTTSD2SIr32r)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref MOVrr8: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrr8)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
        };
        pub static ref MOVrr32: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrr32)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                    RegisterClassKind::GR32,
                ))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref MOVri32: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVri32)
                .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I32)])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref MOVri8: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVri8)
                .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I8)])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
        };
        pub static ref MOVrm8: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrm8)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR8)])
        };
        pub static ref MOVmi8: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmi8).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Immediate(TargetImmediate::I8),
            ])
        };
        pub static ref MOVmr8: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmr8).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR8)),
            ])
        };
        pub static ref MOVrm32: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrm32)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR32)])
        };
        pub static ref MOVmr32: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmr32).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR32)),
            ])
        };
        pub static ref MOVmi32: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmi32).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Immediate(TargetImmediate::I32),
            ])
        };
        pub static ref MOVmr64: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmr64).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::GR64)),
            ])
        };
        pub static ref MOVmi64: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVmi64).set_uses(vec![
                TargetOperand::Mem,
                TargetOperand::Immediate(TargetImmediate::I64),
            ])
        };
        pub static ref MOVrr64: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrr64)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                    RegisterClassKind::GR64,
                ))])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref MOVri64: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVri64)
                .set_uses(vec![TargetOperand::Immediate(TargetImmediate::I64)])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref MOVrm64: TargetInstDef = {
            TargetInstDef::new("mov", TargetOpcode::MOVrm64)
                .set_uses(vec![TargetOperand::Mem])
                .set_defs(vec![TargetRegister::RegClass(RegisterClassKind::GR64)])
        };
        pub static ref IDIV: TargetInstDef = {
            TargetInstDef::new("idiv", TargetOpcode::IDIV)
                .set_uses(vec![TargetOperand::Register(TargetRegister::RegClass(
                    RegisterClassKind::GR32,
                ))])
                .set_imp_def(vec![
                    TargetRegister::Specific(GR32::EAX.as_phys_reg()),
                    TargetRegister::Specific(GR32::EDX.as_phys_reg()),
                ])
                .set_imp_use(vec![
                    TargetRegister::Specific(GR32::EAX.as_phys_reg()),
                    TargetRegister::Specific(GR32::EDX.as_phys_reg()),
                ])
        };
        pub static ref PUSH64: TargetInstDef = {
            TargetInstDef::new("push", TargetOpcode::PUSH64).set_uses(vec![TargetOperand::Register(
                TargetRegister::RegClass(RegisterClassKind::GR64),
            )])
        };
        pub static ref POP64: TargetInstDef = {
            TargetInstDef::new("pop", TargetOpcode::POP64).set_uses(vec![TargetOperand::Register(
                TargetRegister::RegClass(RegisterClassKind::GR64),
            )])
        };
        pub static ref JG: TargetInstDef = {
            TargetInstDef::new("jg", TargetOpcode::JG).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JGE: TargetInstDef = {
            TargetInstDef::new("jge", TargetOpcode::JGE).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JE: TargetInstDef = {
            TargetInstDef::new("je", TargetOpcode::JE).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JNE: TargetInstDef = {
            TargetInstDef::new("jne", TargetOpcode::JNE).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JLE: TargetInstDef = {
            TargetInstDef::new("jle", TargetOpcode::JLE).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JL: TargetInstDef = {
            TargetInstDef::new("jl", TargetOpcode::JL).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JB: TargetInstDef = {
            TargetInstDef::new("jb", TargetOpcode::JB).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JBE: TargetInstDef = {
            TargetInstDef::new("jbe", TargetOpcode::JB).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JA: TargetInstDef = {
            TargetInstDef::new("ja", TargetOpcode::JA).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JAE: TargetInstDef = {
            TargetInstDef::new("jae", TargetOpcode::JAE).set_uses(vec![TargetOperand::Block])
        };
        pub static ref JMP: TargetInstDef = {
            TargetInstDef::new("jmp", TargetOpcode::JMP).set_uses(vec![TargetOperand::Block])
        };
        pub static ref CMPri: TargetInstDef = {
            TargetInstDef::new("cmp", TargetOpcode::CMPri)
                .set_uses(vec![TargetOperand::Any, TargetOperand::Any])
        };
        pub static ref CMPrr: TargetInstDef = {
            TargetInstDef::new("cmp", TargetOpcode::CMPrr)
                .set_uses(vec![TargetOperand::Any, TargetOperand::Any])
        };
        pub static ref UCOMISDrr: TargetInstDef = {
            TargetInstDef::new("ucomisd", TargetOpcode::UCOMISDrr)
                .set_uses(vec![
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM)),
                    TargetOperand::Register(TargetRegister::RegClass(RegisterClassKind::XMM))
                ])
        };
        pub static ref CALL: TargetInstDef = {
            TargetInstDef::new("call", TargetOpcode::CALL) // TODO: defs and uses info
                // .set_uses(vec![TargetOperand::Any, TargetOperand::Any])
                // .set_imp_use(vec![]
        };
        pub static ref RET: TargetInstDef = TargetInstDef::new("ret", TargetOpcode::RET);
    }
}

// r => register
// i => constant integer
// m => TODO: [memory] or [rbp - fi.off]
// p => [register]
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TargetOpcode {
    MOVSDrm64, // out(xmm) = movsd [memory64] TODO
    MOVSDmr,   // movsd MEM, r
    MOVSDrm,   // movsd r, MEM
    MOVSDrr,

    MOVrm32,
    MOVrm8,
    MOVri8,

    MOVmr8,
    MOVmi8,
    MOVmr32,
    MOVmi32,
    MOVmr64,
    MOVmi64,

    MOVSXr32r8,
    MOVSXDr64m32, // out = movsxd [rbp - fi.off]
    MOVSXDr64r32, // r64 = movsxd r32

    LEAr64m,

    ADDrr8,
    ADDri8,
    ADDrr32,
    ADDmr32,
    ADDmi32,
    ADDrr64,
    ADDri32,
    ADDr64i32,
    ADDSDrr,
    ADDSDrm,
    SUBrr8,
    SUBri8,
    SUBrr32,
    SUBmr32,
    SUBmi32,
    SUBri32,
    SUBr64i32,
    SUBSDrr,
    SUBSDrm,
    // IMULrr8,
    IMULrri8,
    IMULrr32,
    IMULrri32,
    IMULrr64i32,
    MULSDrr,
    MULSDrm,
    CDQ,
    IDIV,
    DIVSDrr,
    DIVSDrm,
    SHLr64i8,
    SHLr32i8,
    CVTTSD2SIr32r,
    CVTSI2SDrr32,
    SQRTSDrr,
    MOVrr8,
    MOVrr32,
    MOVri32,
    MOVrr64,
    MOVri64,
    MOVrm64,
    PUSH64,
    POP64,
    RET,

    CALL,

    // Comparison
    Seteq,
    Setle,
    Setlt,

    // BrccEq,
    // BrccLe,
    // BrccLt,
    CMPrr,
    CMPri,
    UCOMISDrr,
    JE,
    JNE,
    JBE,
    JB,
    JLE,
    JL,
    JA,
    JAE,
    JG,
    JGE,
    JMP,

    Phi,
    Ret,
    Copy,
    ExtractSubreg,
    BrCond,
    AdjStackDown,
    AdjStackUp,
}

impl TargetOpcode {
    pub fn inst_def(&self) -> Option<&TargetInstDef> {
        match self {
            Self::MOVSDrm64 => Some(&*inst::MOVSDrm64),
            Self::MOVSDmr => Some(&*inst::MOVSDmr),
            Self::MOVSDrm => Some(&*inst::MOVSDrm),
            Self::MOVSDrr => Some(&*inst::MOVSDrr),
            Self::MOVSXr32r8 => Some(&*inst::MOVSXr32r8),
            Self::MOVSXDr64m32 => Some(&*inst::MOVSXDr64m32),
            Self::MOVSXDr64r32 => Some(&*inst::MOVSXDr64r32),
            Self::LEAr64m => Some(&*inst::LEAr64m),
            Self::ADDrr8 => Some(&*inst::ADDrr8),
            Self::ADDri8 => Some(&*inst::ADDri8),
            Self::ADDrr32 => Some(&*inst::ADDrr32),
            Self::ADDmr32 => Some(&*inst::ADDmr32),
            Self::ADDmi32 => Some(&*inst::ADDmi32),
            Self::ADDrr64 => Some(&*inst::ADDrr64),
            Self::ADDri32 => Some(&*inst::ADDri32),
            Self::ADDr64i32 => Some(&*inst::ADDr64i32),
            Self::ADDSDrr => Some(&*inst::ADDSDrr),
            Self::ADDSDrm => Some(&*inst::ADDSDrm),
            Self::SUBrr8 => Some(&*inst::SUBrr8),
            Self::SUBri8 => Some(&*inst::SUBri8),
            Self::SUBrr32 => Some(&*inst::SUBrr32),
            Self::SUBri32 => Some(&*inst::SUBri32),
            Self::SUBmr32 => Some(&*inst::SUBmr32),
            Self::SUBmi32 => Some(&*inst::SUBmi32),
            Self::SUBr64i32 => Some(&*inst::SUBr64i32),
            Self::SUBSDrr => Some(&*inst::SUBSDrr),
            Self::SUBSDrm => Some(&*inst::SUBSDrm),
            // Self::IMULrr8 => Some(&*inst::IMULrr8),
            Self::IMULrri8 => Some(&*inst::IMULrri8),
            Self::IMULrr32 => Some(&*inst::IMULrr32),
            Self::IMULrri32 => Some(&*inst::IMULrri32),
            Self::IMULrr64i32 => Some(&*inst::IMULrr64i32),
            Self::MULSDrr => Some(&*inst::MULSDrr),
            Self::MULSDrm => Some(&*inst::MULSDrm),
            Self::CDQ => Some(&*inst::CDQ),
            Self::DIVSDrr => Some(&*inst::DIVSDrr),
            Self::DIVSDrm => Some(&*inst::DIVSDrm),
            Self::SHLr64i8 => Some(&*inst::SHLr64i8),
            Self::SHLr32i8 => Some(&*inst::SHLr32i8),
            Self::CVTSI2SDrr32 => Some(&*inst::CVTSI2SDrr32),
            Self::CVTTSD2SIr32r => Some(&*inst::CVTTSD2SIr32r),
            Self::SQRTSDrr => Some(&*inst::SQRTSDrr),
            Self::MOVrr8 => Some(&*inst::MOVrr8),
            Self::MOVri8 => Some(&*inst::MOVri8),
            Self::MOVrm8 => Some(&*inst::MOVrm8),
            Self::MOVmr8 => Some(&*inst::MOVmr8),
            Self::MOVmi8 => Some(&*inst::MOVmi8),
            Self::MOVrr32 => Some(&*inst::MOVrr32),
            Self::MOVri32 => Some(&*inst::MOVri32),
            Self::MOVrm32 => Some(&*inst::MOVrm32),
            Self::MOVmr32 => Some(&*inst::MOVmr32),
            Self::MOVmi32 => Some(&*inst::MOVmi32),
            Self::MOVmr64 => Some(&*inst::MOVmr64),
            Self::MOVmi64 => Some(&*inst::MOVmi64),
            Self::MOVrr64 => Some(&*inst::MOVrr64),
            Self::MOVri64 => Some(&*inst::MOVri64),
            Self::MOVrm64 => Some(&*inst::MOVrm64),
            Self::IDIV => Some(&*inst::IDIV),
            Self::PUSH64 => Some(&*inst::PUSH64),
            Self::POP64 => Some(&*inst::POP64),
            Self::JMP => Some(&*inst::JMP),
            Self::JG => Some(&*inst::JG),
            Self::JGE => Some(&*inst::JGE),
            Self::JE => Some(&*inst::JE),
            Self::JNE => Some(&*inst::JNE),
            Self::JLE => Some(&*inst::JLE),
            Self::JL => Some(&*inst::JL),
            Self::JB => Some(&*inst::JB),
            Self::JBE => Some(&*inst::JBE),
            Self::JA => Some(&*inst::JA),
            Self::JAE => Some(&*inst::JAE),
            Self::CMPri => Some(&*inst::CMPri),
            Self::CMPrr => Some(&*inst::CMPrr),
            Self::CALL => Some(&*inst::CALL),
            Self::RET => Some(&*inst::RET),
            Self::UCOMISDrr => Some(&*inst::UCOMISDrr),
            _ => None,
        }
    }
}
