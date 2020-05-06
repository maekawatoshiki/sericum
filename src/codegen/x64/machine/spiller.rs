use super::super::dag::mc_convert::{mov_mx, mov_rx};
use super::super::{
    frame_object::FrameIndexInfo,
    register::{rc2ty, RegisterId, VirtReg, GR64},
};
use super::{
    builder::{BuilderTrait, BuilderWithLiveInfoEdit},
    function::MachineFunction,
    inst::{MachineInst, MachineMemOperand, MachineOperand, MachineRegister},
    liveness::LiveRegMatrix,
};
use crate::ir::types::Types;

pub struct Spiller<'a> {
    func: &'a mut MachineFunction,
    matrix: &'a mut LiveRegMatrix,
}

impl<'a> Spiller<'a> {
    pub fn new(func: &'a mut MachineFunction, matrix: &'a mut LiveRegMatrix) -> Self {
        Self { func, matrix }
    }

    pub fn insert_evict(&mut self, r: RegisterId, slot: &FrameIndexInfo) {
        let r_defs = self.func.regs_info.arena_ref()[r.id].defs.clone();
        // If r is defined more than once, choose the later defined one.
        // e.g.
        //   %v1 = COPY %v2
        //   %v1 = ADD %v1, 2 <- def_id
        let def_id = *r_defs
            .iter()
            .max_by(|x, y| {
                let x = &self.matrix.get_program_point(**x).unwrap();
                let y = &self.matrix.get_program_point(**y).unwrap();
                x.cmp(&y)
            })
            .unwrap();

        let bb = self.func.body.inst_arena[def_id].parent;
        let dst = MachineOperand::FrameIndex(slot.clone());
        let src = MachineOperand::Register(r);
        let rbp = self.func.regs_info.get_phys_reg(GR64::RBP);
        let store = MachineInst::new_simple(
            mov_mx(&self.func.regs_info, &src).unwrap(),
            vec![
                MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *dst.as_frame_index())),
                src,
            ],
            bb,
        );

        let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
        builder.set_insert_point_after_inst(def_id).unwrap();
        builder.insert(store);
    }

    pub fn insert_reload(
        &mut self,
        tys: &Types,
        reg_id: RegisterId,
        slot: &FrameIndexInfo,
    ) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let rc = self.func.regs_info.arena_ref()[reg_id.id].reg_class;

        let uses = self.func.regs_info.arena_ref()[reg_id.id].uses.clone();
        for use_id in uses {
            let new_r = self.func.regs_info.new_virt_reg(rc);
            new_regs.push(new_r.as_virt_reg());
            self.matrix.add_vreg_entity(new_r);

            // TODO: inst arena should have replace_operand_register
            let use_inst = &mut self.func.body.inst_arena[use_id];
            use_inst.replace_operand_register(&self.func.regs_info, reg_id, new_r);

            let fi = MachineOperand::FrameIndex(slot.clone());
            let rbp = self.func.regs_info.get_phys_reg(GR64::RBP);
            let load = MachineInst::new_simple(
                mov_rx(tys, &self.func.regs_info, &fi).unwrap(),
                vec![MachineOperand::Mem(MachineMemOperand::BaseFi(
                    rbp,
                    *fi.as_frame_index(),
                ))],
                use_inst.parent,
            )
            .with_def(vec![new_r]);

            let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
            builder.set_insert_point_before_inst(use_id);
            builder.insert(load);
        }

        self.func.regs_info.arena_ref_mut()[reg_id.id].uses.clear();

        new_regs
    }

    pub fn spill(&mut self, tys: &Types, vreg: VirtReg) -> Vec<VirtReg> {
        let reg_id = *self.matrix.get_entity_by_vreg(vreg).unwrap();
        let slot = self
            .func
            .local_mgr
            .alloc(&rc2ty(self.func.regs_info.arena_ref()[reg_id.id].reg_class)); // TODO

        self.matrix
            .virt_reg_interval
            .get_mut(&vreg)
            .unwrap()
            .range
            .adjust_end_to_start();

        let new_regs = self.insert_reload(tys, reg_id, &slot);
        self.insert_evict(reg_id, &slot);

        new_regs
    }
}
