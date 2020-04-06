use super::super::dag::mc_convert::{mov_mx, mov_rx};
use super::super::{
    frame_object::FrameIndexInfo,
    register::{rc2ty, VirtReg, GR64},
};
use super::{
    builder::{BuilderTrait, BuilderWithLiveInfoEdit},
    function::MachineFunction,
    inst::{MachineInst, MachineOperand, MachineRegister},
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

    pub fn insert_evict(&mut self, r: MachineRegister, slot: &FrameIndexInfo) {
        let r_def_list = r.info_ref().def_list.clone();
        // If r is defined more than once, choose the later defined one.
        // e.g.
        //   %v1 = COPY %v2
        //   %v1 = ADD %v1, 2 <- def_id
        let def_id = *r_def_list
            .iter()
            .max_by(|x, y| {
                let x = &self.matrix.get_program_point(**x).unwrap();
                let y = &self.matrix.get_program_point(**y).unwrap();
                x.cmp(&y)
            })
            .unwrap();

        let bb = self.func.body.inst_arena[def_id].parent;
        let dst = MachineOperand::FrameIndex(slot.clone());
        let src = MachineOperand::Register(r.clone());
        let rbp = MachineOperand::phys_reg(GR64::RBP);
        let store = MachineInst::new_simple(
            mov_mx(&src).unwrap(),
            vec![rbp, dst, MachineOperand::None, MachineOperand::None, src],
            bb,
        );

        let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
        builder.set_insert_point_after_inst(def_id).unwrap();
        builder.insert(store);
    }

    pub fn insert_reload(
        &mut self,
        tys: &Types,
        r: MachineRegister,
        slot: &FrameIndexInfo,
    ) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let rc = r.info_ref().reg_class;

        for use_id in &r.info_ref().use_list {
            let use_id = *use_id;
            let new_r = self.func.vreg_gen.gen_vreg(rc).into_machine_register();
            new_regs.push(new_r.get_vreg());
            self.matrix.add_vreg_entity(new_r.clone());

            let use_inst = &mut self.func.body.inst_arena[use_id];
            use_inst.replace_operand_reg(&r, &new_r);
            new_r.add_use(use_id);

            let src = MachineOperand::FrameIndex(slot.clone());
            let rbp = MachineOperand::phys_reg(GR64::RBP);
            let load = MachineInst::new_simple(
                mov_rx(tys, &src).unwrap(),
                vec![rbp, src, MachineOperand::None, MachineOperand::None],
                use_inst.parent,
            )
            .with_def(vec![new_r.clone()]);

            let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
            builder.set_insert_point_before_inst(use_id);
            builder.insert(load);
        }

        r.info_ref_mut().use_list.clear();

        new_regs
    }

    pub fn spill(&mut self, tys: &Types, vreg: VirtReg) -> Vec<VirtReg> {
        let r = self.matrix.get_entity_by_vreg(vreg).unwrap().clone();
        let slot = self.func.local_mgr.alloc(&rc2ty(r.info_ref().reg_class)); // TODO

        self.matrix
            .get_vreg_interval_mut(vreg)
            .unwrap()
            .range
            .adjust_end_to_start();

        let new_regs = self.insert_reload(tys, r.clone(), &slot);
        self.insert_evict(r, &slot);

        new_regs
    }
}
