use super::super::dag::mc_convert::{mov_mx, mov_rx};
use super::super::{
    frame_object::FrameIndexInfo,
    register::{rc2ty, RegisterId, VirtReg, GR64},
};
use super::{
    builder::{BuilderTrait, BuilderWithLiveInfoEdit},
    function::MachineFunction,
    inst::{MachineInst, MachineMemOperand, MachineOpcode, MachineOperand},
    liveness::{LiveRange, LiveRegMatrix, LiveSegment},
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

    pub fn insert_evict(&mut self, reg_id: RegisterId, slot: &FrameIndexInfo) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let mut defs = self.func.regs_info.arena_ref()[reg_id]
            .defs
            .clone()
            .into_iter()
            .collect::<Vec<_>>();

        defs.sort_by(|x, y| {
            let x = self.matrix.get_program_point(*x).unwrap();
            let y = self.matrix.get_program_point(*y).unwrap();
            x.cmp(&y)
        });

        // a = add a, b
        //
        // new_reg = add a, b
        // mov [mem], new_reg
        //
        // new_reg = mov a
        // new_reg = add new_reg, b
        // mov [mem], new_reg

        for def_id in defs {
            let reg_class = self.func.regs_info.arena_ref()[reg_id].reg_class;
            let new_reg = self.func.regs_info.new_virt_reg(reg_class);
            new_regs.push(new_reg.as_virt_reg());
            self.matrix.add_vreg_entity(new_reg);
            self.matrix
                .virt_reg_interval
                .add(new_reg.as_virt_reg(), LiveRange::new_empty())
                .is_spillable = false;

            let def_inst = &mut self.func.body.inst_arena[def_id];
            def_inst.set_def(&self.func.regs_info, new_reg);

            let mut old_src = None;
            if let Some(inst_def) = def_inst.opcode.inst_def() {
                assert!(inst_def.tie.len() <= 1); // TODO: support for more than one tied register
                if inst_def.tie.len() == 1 {
                    let pos = def_inst
                        .operand
                        .iter()
                        .position(|o| o.is_register() && o.as_register() == &reg_id)
                        .unwrap();
                    if inst_def.tie.iter().next().unwrap().1.as_use() == pos {
                        old_src = Some(reg_id);
                        def_inst.replace_operand_register(&self.func.regs_info, reg_id, new_reg);
                    }
                }
            }

            let parent = self.func.body.inst_arena[def_id].parent;
            let dst = MachineOperand::FrameIndex(slot.clone());
            let src = MachineOperand::Register(new_reg);
            let rbp = self.func.regs_info.get_phys_reg(GR64::RBP);
            let store = MachineInst::new_simple(
                mov_mx(&self.func.regs_info, &src).unwrap(),
                vec![
                    MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *dst.as_frame_index())),
                    src.clone(),
                ],
                parent,
            );
            let copy = old_src.map_or(None, |dst| {
                let copy = MachineInst::new_simple(MachineOpcode::Copy, vec![src], parent)
                    .with_def(vec![dst]);
                Some(copy)
            });

            let start = self.matrix.get_program_point(def_id).unwrap();
            self.matrix
                .virt_reg_interval
                .get_mut(&new_reg.as_virt_reg())
                .unwrap()
                .range
                .add_segment(LiveSegment::new(start, start));

            let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
            builder.set_insert_point_after_inst(def_id).unwrap();
            if let Some(copy) = copy {
                builder.insert(copy);
            }
            builder.insert(store);
        }

        self.func.regs_info.arena_ref_mut()[reg_id].defs.clear();

        new_regs
    }

    pub fn insert_reload(
        &mut self,
        tys: &Types,
        reg_id: RegisterId,
        slot: &FrameIndexInfo,
    ) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let reg_class = self.func.regs_info.arena_ref()[reg_id].reg_class;

        let mut uses = self.func.regs_info.arena_ref()[reg_id]
            .uses
            .clone()
            .into_iter()
            .collect::<Vec<_>>();

        // TODO: Need sort because BuilderWithLiveInfoEdit is stupid
        uses.sort_by(|x, y| {
            let x = self.matrix.get_program_point(*x).unwrap();
            let y = self.matrix.get_program_point(*y).unwrap();
            x.cmp(&y)
        });

        for use_id in uses {
            let new_reg = self.func.regs_info.new_virt_reg(reg_class);
            new_regs.push(new_reg.as_virt_reg());
            self.matrix.add_vreg_entity(new_reg);

            let use_inst = &mut self.func.body.inst_arena[use_id];
            let parent = use_inst.parent;
            let replaced_idx =
                use_inst.replace_operand_register(&self.func.regs_info, reg_id, new_reg);
            let mut old_dst = None;
            if let Some(inst_def) = use_inst.opcode.inst_def() {
                // TODO: Support for more than one tied register
                if inst_def.tie.len() == 1
                    && replaced_idx.len() == 1
                    && replaced_idx[0] == 0
                    && use_inst.def[0] == reg_id
                {
                    old_dst = Some(use_inst.def[0]);
                    use_inst.set_def(&self.func.regs_info, new_reg);
                }
            }

            let fi = MachineOperand::FrameIndex(slot.clone());
            let rbp = self.func.regs_info.get_phys_reg(GR64::RBP);
            let load = MachineInst::new_simple(
                mov_rx(tys, &self.func.regs_info, &fi).unwrap(),
                vec![MachineOperand::Mem(MachineMemOperand::BaseFi(
                    rbp,
                    *fi.as_frame_index(),
                ))],
                parent,
            )
            .with_def(vec![new_reg]);

            let copy = old_dst.map_or(None, |dst| {
                let copy = MachineInst::new_simple(
                    MachineOpcode::Copy,
                    vec![MachineOperand::Register(new_reg)],
                    parent,
                )
                .with_def(vec![dst]);
                Some(copy)
            });
            let new_reg_needs_live_range_update = copy.is_none();

            {
                let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
                builder.set_insert_point_before_inst(use_id).unwrap();
                builder.insert(load);
                builder.set_insert_point_after_inst(use_id).unwrap();
                if let Some(copy) = copy {
                    builder.insert(copy);
                }
            }

            let use_pp = self.matrix.get_program_point(use_id).unwrap();
            let interval = self
                .matrix
                .virt_reg_interval
                .get_mut(&new_reg.as_virt_reg())
                .unwrap();
            if new_reg_needs_live_range_update {
                *interval.end_point_mut().unwrap() = use_pp;
            }
            interval.is_spillable = false;
        }

        self.func.regs_info.arena_ref_mut()[reg_id].uses.clear();

        new_regs
    }

    pub fn spill(&mut self, tys: &Types, vreg: VirtReg) -> Vec<VirtReg> {
        let reg_id = *self.matrix.get_entity_by_vreg(vreg).unwrap();
        let slot = self
            .func
            .local_mgr
            .alloc(&rc2ty(self.func.regs_info.arena_ref()[reg_id].reg_class)); // TODO

        let mut new_regs = self.insert_reload(tys, reg_id, &slot);
        new_regs.append(&mut self.insert_evict(reg_id, &slot));

        self.matrix.virt_reg_interval.inner_mut().remove(&vreg);
        self.matrix.vreg2entity.remove(&vreg);

        new_regs
    }
}
