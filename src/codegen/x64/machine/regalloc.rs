use crate::codegen::arch::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::FrameIndexInfo,
    machine::inst::MachineMemOperand,
    machine::register::*,
};
pub use crate::codegen::common::machine::regalloc::*;
use crate::codegen::common::machine::{
    basic_block::MachineBasicBlockId,
    function::MachineFunction,
    inst::{MachineInst, MachineInstId, MachineOperand, RegisterOperand},
};

impl RegisterAllocator {
    pub fn store_and_load_for_reg_preservation(
        &mut self,
        f: &mut MachineFunction,
        reg: RegisterId,
        frinfo: FrameIndexInfo,
        parent: MachineBasicBlockId,
    ) -> (MachineInstId, MachineInstId) {
        let dst = MachineOperand::FrameIndex(frinfo.clone());
        let src = MachineOperand::Register(RegisterOperand::new(reg));
        let rbp = RegisterOperand::new(f.regs_info.get_phys_reg(GR64::RBP));
        let store_inst_id = f.alloc_inst(MachineInst::new(
            &f.regs_info,
            mov_mx(&f.regs_info, &src).unwrap(),
            vec![
                MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *dst.as_frame_index())),
                src,
            ],
            None,
            parent,
        ));

        let src = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, frinfo));
        let opcode = mov_rx(f.regs_info.arena_ref()[reg].reg_class, &src).unwrap();
        let load_inst_id = f.alloc_inst(
            MachineInst::new_simple(opcode, vec![src], parent)
                .with_def(vec![RegisterOperand::new(reg)]),
        );

        (store_inst_id, load_inst_id)
    }

    pub fn get_regs_used_to_preserve(
        &mut self,
        func: &mut MachineFunction,
        store_id: MachineInstId,
        load_id: MachineInstId,
    ) -> Vec<(RegisterId, RegisterId)> {
        let store = &func.body.inst_arena[store_id];
        let r1 = store.operand[1].as_register().id;

        let load = &func.body.inst_arena[load_id];
        let r2 = load.def[0].id;

        vec![(r1, r2)]
    }
}
