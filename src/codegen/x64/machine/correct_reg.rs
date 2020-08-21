use crate::codegen::arch::machine::register::{
    RegisterClassKind, RegisterId, GR32, GR64, GR8, XMM,
};
use crate::codegen::common::machine::inst_def::*;
use crate::codegen::common::machine::register::{PhysReg, TargetRegisterTrait, VirtOrPhys};
use crate::codegen::common::machine::{function::MachineFunction, module::MachineModule};
use crate::traits::pass::ModulePassTrait;
use num::FromPrimitive;

pub struct CorrectOperandRegisterClass {}

impl ModulePassTrait for CorrectOperandRegisterClass {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "CorrectOperandRegisterClass"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl CorrectOperandRegisterClass {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        fn get_reg(rc: &RegisterClassKind, idx: usize) -> PhysReg {
            match rc {
                RegisterClassKind::GR8 => GR8::from_usize(idx).unwrap().as_phys_reg(),
                RegisterClassKind::GR32 => GR32::from_usize(idx).unwrap().as_phys_reg(),
                RegisterClassKind::GR64 => GR64::from_usize(idx).unwrap().as_phys_reg(),
                RegisterClassKind::XMM => XMM::from_usize(idx).unwrap().as_phys_reg(),
            }
        }
        fn reg_idx(r: &RegisterId) -> usize {
            r.as_phys_reg().retrieve() - r.as_phys_reg().reg_class() as usize
        }

        if f.is_internal {
            return;
        }

        for (_, bb) in f.body.basic_blocks.id_and_block() {
            for inst_id in &*bb.iseq_ref() {
                let inst = &mut f.body.inst_arena[*inst_id];

                let inst_def = inst.opcode.inst_def();
                let inst_def = if let Some(def) = inst_def {
                    def
                } else {
                    continue;
                };

                for (correct_r, r) in inst_def.defs.iter().zip(inst.def.iter_mut()) {
                    match correct_r {
                        TargetRegister::RegClass(rc) if rc != &r.as_phys_reg().reg_class() => {
                            r.kind = VirtOrPhys::Phys(get_reg(rc, reg_idx(&r)));
                        }
                        _ => {}
                    }
                }

                for (correct_r, operand) in inst_def.uses.iter().zip(inst.operand.iter_mut()) {
                    match correct_r {
                        TargetOperand::Register(TargetRegister::RegClass(rc)) => {
                            let mut regs = operand.registers_mut();

                            // `operand` is Mem operand, so skip it
                            if regs.len() > 1 {
                                continue;
                            }

                            let r = &mut regs[0];
                            if rc != &r.as_phys_reg().reg_class() {
                                r.kind = VirtOrPhys::Phys(get_reg(rc, reg_idx(&r)));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}
