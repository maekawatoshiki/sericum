use super::super::{
    frame_object::FrameIndexInfo,
    register::{rc2ty, VirtReg},
};
use super::{
    builder::{BuilderTrait, BuilderWithLiveInfoEdit},
    function::MachineFunction,
    instr::{MachineInstr, MachineOpcode, MachineOperand, MachineRegister},
    liveness::LiveRegMatrix,
};

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
        // If r is defined more than once (no longer SSA) and r_def_list.len() > 1,
        // choose the later defined one.
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

        let def_instr = &mut self.func.instr_arena[def_id];
        let store = MachineInstr::new_simple(
            MachineOpcode::Store,
            vec![
                MachineOperand::FrameIndex(slot.clone()),
                MachineOperand::Register(r.clone()),
            ],
            def_instr.parent,
        );

        let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
        builder.set_insert_point_after_instr(def_id).unwrap();
        builder.insert(store);
    }

    pub fn insert_reload(&mut self, r: MachineRegister, slot: &FrameIndexInfo) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let rc = r.info_ref().reg_class;

        for use_id in &r.info_ref().use_list {
            let use_id = *use_id;
            let new_r = self.func.vreg_gen.gen_vreg(rc).into_machine_register();
            new_regs.push(new_r.get_vreg());
            self.matrix.add_vreg_entity(new_r.clone());

            let use_instr = &mut self.func.instr_arena[use_id];
            use_instr.replace_operand_reg(&r, &new_r);
            new_r.add_use(use_id);

            let load = MachineInstr::new_simple(
                MachineOpcode::Load,
                vec![MachineOperand::FrameIndex(slot.clone())],
                use_instr.parent,
            )
            .with_def(vec![new_r.clone()]);

            let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
            builder.set_insert_point_before_instr(use_id);
            builder.insert(load);
        }

        r.info_ref_mut().use_list.clear();

        new_regs
    }

    pub fn spill(&mut self, vreg: VirtReg) -> Vec<VirtReg> {
        let r = self.matrix.get_entity_by_vreg(vreg).unwrap().clone();
        let slot = self.func.local_mgr.alloc(&rc2ty(r.info_ref().reg_class)); // TODO

        self.matrix
            .get_vreg_interval_mut(vreg)
            .unwrap()
            .range
            .adjust_end_to_start();

        let new_regs = self.insert_reload(r.clone(), &slot);
        self.insert_evict(r, &slot);

        new_regs
    }
}
