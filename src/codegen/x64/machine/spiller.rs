use super::super::dag::mc_convert::{mov_mx, mov_rx};
use super::super::{
    frame_object::FrameIndexInfo,
    machine::register::{rc2ty, RegisterId, VirtReg, GR64},
};
use super::inst::{MachineInst, MachineMemOperand, MachineOperand, RegisterOperand};
use crate::codegen::common::machine::{
    builder::{BuilderTrait, BuilderWithLiveInfoEdit},
    function::MachineFunction,
    liveness::{LiveRange, LiveRegMatrix, LiveSegment},
};
use rustc_hash::FxHashMap;

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

        // a = c
        // a = add a, b
        //
        // a = c
        // new_reg = add a, b
        // mov [mem], new_reg
        //
        // new_reg = mov c
        // new_reg = add new_reg, b
        // mov [mem], new_reg

        let mut two_addrs = FxHashMap::default();

        for &def_id in &defs {
            let def_inst = &mut self.func.body.inst_arena[def_id];
            let new_reg = if let Some(r) = two_addrs.remove(&def_id) {
                def_inst.set_def(&self.func.regs_info, RegisterOperand::new(r));
                r
            } else {
                let new_reg = self.func.regs_info.new_virt_reg_from(&reg_id);
                new_regs.push(new_reg.as_virt_reg());
                self.matrix.add_virt_reg(new_reg);
                self.matrix
                    .virt_reg_interval
                    .add(new_reg.as_virt_reg(), LiveRange::new_empty())
                    .is_spillable = false;
                def_inst.set_def(&self.func.regs_info, RegisterOperand::new(new_reg));
                self.func
                    .body
                    .basic_blocks
                    .liveness
                    .get_mut(&def_inst.parent)
                    .unwrap()
                    .add_def(new_reg);

                let start = self.matrix.get_program_point(def_id).unwrap();
                self.matrix
                    .virt_reg_interval
                    .get_mut(&new_reg.as_virt_reg())
                    .unwrap()
                    .range
                    .add_segment(LiveSegment::new(start, start));

                if let Some(i) = def_inst.copy_for_two_addr {
                    two_addrs.insert(i, new_reg);
                }

                new_reg
            };

            if let Some(inst_def) = def_inst.opcode.inst_def() {
                assert!(inst_def.tie.len() <= 1); // TODO: support more than one tied register
                if inst_def.tie.len() == 1 {
                    def_inst.replace_operand_register(&self.func.regs_info, reg_id, new_reg);
                }
            }

            let dst = MachineOperand::FrameIndex(slot.clone());
            let src = MachineOperand::Register(RegisterOperand::new(new_reg));
            let rbp = RegisterOperand::new(self.func.regs_info.get_phys_reg(GR64::RBP));
            let store = MachineInst::new_simple(
                mov_mx(&self.func.regs_info, &src).unwrap(),
                vec![
                    MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *dst.as_frame_index())),
                    src.clone(),
                ],
                def_inst.parent,
            );

            let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
            builder.set_insert_point_after_inst(def_id).unwrap();
            builder.insert(store);
        }

        self.func.regs_info.arena_ref_mut()[reg_id].defs.clear();

        new_regs
    }

    pub fn insert_reload(&mut self, reg_id: RegisterId, slot: &FrameIndexInfo) -> Vec<VirtReg> {
        let mut new_regs = vec![];
        let uses = self.func.regs_info.arena_ref()[reg_id]
            .uses
            .clone()
            .into_iter()
            .collect::<Vec<_>>();

        for use_id in uses {
            let new_reg = self.func.regs_info.new_virt_reg_from(&reg_id);
            new_regs.push(new_reg.as_virt_reg());
            self.matrix.add_virt_reg(new_reg);

            let use_inst = &mut self.func.body.inst_arena[use_id];
            let parent = use_inst.parent;
            use_inst.replace_operand_register(&self.func.regs_info, reg_id, new_reg);

            let rbp = RegisterOperand::new(self.func.regs_info.get_phys_reg(GR64::RBP));
            let src = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, slot.clone()));
            let load = MachineInst::new_simple(
                mov_rx(self.func.regs_info.arena_ref()[new_reg].reg_class, &src).unwrap(),
                vec![src],
                parent,
            )
            .with_def(vec![RegisterOperand::new(new_reg)]);

            self.func
                .body
                .basic_blocks
                .liveness
                .get_mut(&parent)
                .unwrap()
                .add_def(new_reg);

            {
                let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);
                builder.set_insert_point_before_inst(use_id).unwrap();
                builder.insert(load);
            }

            let use_pp = self.matrix.get_program_point(use_id).unwrap();
            let interval = self
                .matrix
                .virt_reg_interval
                .get_mut(&new_reg.as_virt_reg())
                .unwrap();
            interval.is_spillable = false;
            if let Some(s) = interval.range.find_nearest_starting_segment_mut(&use_pp) {
                assert!(s.end < use_pp);
                s.end = use_pp;
            }
        }

        self.func.regs_info.arena_ref_mut()[reg_id].uses.clear();

        new_regs
    }

    pub fn spill(&mut self, vreg: VirtReg) -> Vec<VirtReg> {
        let reg_id = *self.matrix.get_entity_by_vreg(vreg).unwrap();
        let slot = self
            .func
            .local_mgr
            .alloc(&rc2ty(self.func.regs_info.arena_ref()[reg_id].reg_class)); // TODO: May allocate redundant stack slot

        let mut new_regs = self.insert_evict(reg_id, &slot);
        new_regs.append(&mut self.insert_reload(reg_id, &slot));

        let r = self.matrix.virt_regs.remove(&vreg).unwrap();
        self.func.body.basic_blocks.remove_reg_from_live_info(&r);
        self.matrix.virt_reg_interval.inner_mut().remove(&vreg);

        new_regs
    }
}
