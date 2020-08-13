use crate::codegen::arch::{
    frame_object::FrameIndexInfo,
    machine::inst::{MachineMemOperand, MachineOpcode},
    machine::register::*,
};
pub use crate::codegen::common::machine::regalloc::*;
use crate::codegen::common::machine::{
    basic_block::MachineBasicBlockId,
    function::MachineFunction,
    inst::{MachineInst, MachineInstId, MachineOperand},
};

impl RegisterAllocator {
    pub fn store_and_load_for_reg_preservation(
        &mut self,
        f: &mut MachineFunction,
        reg: RegisterId,
        frinfo: FrameIndexInfo,
        parent: MachineBasicBlockId,
    ) -> (MachineInstId, MachineInstId) {
        let src = MachineOperand::Register(reg);
        let s0 = f.regs_info.get_phys_reg(GPR::S0);
        let store_inst_id = f.alloc_inst(MachineInst::new(
            &f.regs_info,
            MachineOpcode::SD,
            vec![
                src,
                MachineOperand::Mem(MachineMemOperand::FiReg(frinfo, s0)),
            ],
            None,
            parent,
        ));

        let load_inst_id = f.alloc_inst(
            MachineInst::new_simple(
                MachineOpcode::LD,
                vec![MachineOperand::Mem(MachineMemOperand::FiReg(frinfo, s0))],
                parent,
            )
            .with_def(vec![reg]),
        );

        (store_inst_id, load_inst_id)
    }
}
