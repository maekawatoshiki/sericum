use crate::codegen::arch::machine::inst::*;
use crate::codegen::common::machine::{
    basic_block::*,
    function::*,
    liveness::{LiveRange, LiveRegMatrix, LiveSegment, ProgramPoint},
};

pub struct BuilderWithLiveInfoEdit<'a> {
    pub matrix: &'a mut LiveRegMatrix,
    pub function: &'a mut MachineFunction,
    cur_bb_id: Option<MachineBasicBlockId>,
    insert_point: usize,
}

pub struct Builder<'a> {
    pub function: &'a mut MachineFunction,
    cur_bb_id: Option<MachineBasicBlockId>,
    insert_point: usize,
}

pub trait MachineInstTrait {
    fn into_id(self, f: &mut MachineFunction) -> MachineInstId;
}

pub trait BuilderTrait {
    fn set_insert_point_at_entry_block(&mut self);
    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId);
    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId);
    fn set_insert_point_before_inst(&mut self, inst_id: MachineInstId) -> Option<()>;
    fn set_insert_point_after_inst(&mut self, inst_id: MachineInstId) -> Option<()>;
    fn insert<T: MachineInstTrait>(&mut self, inst: T);
    fn back_insert_point_while<F: Fn(&MachineInst) -> bool>(&mut self, f: F);
}

impl MachineInstTrait for MachineInstId {
    fn into_id(self, _: &mut MachineFunction) -> MachineInstId {
        self
    }
}

impl MachineInstTrait for MachineInst {
    fn into_id(self, f: &mut MachineFunction) -> MachineInstId {
        f.alloc_inst(self)
    }
}

impl<'a> BuilderWithLiveInfoEdit<'a> {
    pub fn new(matrix: &'a mut LiveRegMatrix, function: &'a mut MachineFunction) -> Self {
        Self {
            matrix,
            function,
            cur_bb_id: None,
            insert_point: 0,
        }
    }

    fn calc_program_point(&mut self, insert_pt: usize) -> ProgramPoint {
        // calculate program point for given insert point
        let bb = &self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()];
        let iseq = bb.iseq_ref();
        let pp = self.matrix.get_program_point(iseq[insert_pt]).unwrap();
        self.matrix.program_points.prev_of(pp)
    }
}

impl<'a> BuilderTrait for BuilderWithLiveInfoEdit<'a> {
    fn set_insert_point_at_entry_block(&mut self) {
        let entry = *self.function.get_entry_bb().unwrap();
        self.set_insert_point_at(0, entry);
    }

    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = self.function.body.basic_blocks.arena[bb_id]
            .iseq_ref()
            .len();
    }

    fn set_insert_point_before_inst(&mut self, inst_id: MachineInstId) -> Option<()> {
        let (bb_id, inst_pos) = self.function.find_inst_pos(inst_id)?;
        self.set_insert_point_at(inst_pos, bb_id);
        Some(())
    }

    fn set_insert_point_after_inst(&mut self, inst_id: MachineInstId) -> Option<()> {
        let (bb_id, inst_pos) = self.function.find_inst_pos(inst_id)?;
        self.set_insert_point_at(inst_pos + 1, bb_id);
        Some(())
    }

    fn insert<T: MachineInstTrait>(&mut self, inst: T) {
        let insert_pt = self.insert_point;
        self.insert_point += 1;

        let inst_id = inst.into_id(&mut self.function);
        let pp = match self.matrix.id2pp.get(&inst_id) {
            Some(pp) => *pp,
            None => {
                let pp = self.calc_program_point(insert_pt);
                self.matrix.id2pp.insert(inst_id, pp);
                pp
            }
        };

        {
            // update registers' use&def list. TODO: refine code
            let inst = &self.function.body.inst_arena[inst_id];
            for use_ in inst.collect_used_regs() {
                let use_ = &self.function.regs_info.arena_ref()[*use_];

                if let Some(use_) = use_.phys_reg {
                    let range = self.matrix.phys_reg_range.get_mut(use_);
                    if range.is_none() {
                        continue;
                    }
                    if let Some(s) = range.unwrap().find_nearest_starting_segment_mut(&pp) {
                        if s.end < pp {
                            s.end = pp;
                        }
                    }
                } else {
                    if let Some(s) = self
                        .matrix
                        .virt_reg_interval
                        .get_mut(&use_.virt_reg)
                        .unwrap()
                        .range
                        .find_nearest_starting_segment_mut(&pp)
                    {
                        if s.end < pp {
                            s.end = pp;
                        }
                    }
                }
            }

            for &def in inst.collect_defined_regs() {
                let def_ = &self.function.regs_info.arena_ref()[def];

                if let Some(phys_reg) = def_.phys_reg {
                    self.matrix
                        .phys_reg_range
                        .get_or_create(phys_reg)
                        .add_segment(LiveSegment::new(pp, pp));
                } else {
                    self.matrix.add_vreg_entity(def);
                    if let Some(i) = self.matrix.virt_reg_interval.get_mut(&def_.virt_reg) {
                        i.range.add_segment(LiveSegment::new(pp, pp))
                    } else {
                        self.matrix.add_live_interval(
                            def_.virt_reg,
                            LiveRange::new(vec![LiveSegment::new(pp, pp)]),
                        );
                    }
                }
            }
        }

        self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, inst_id);
    }

    fn back_insert_point_while<F: Fn(&MachineInst) -> bool>(&mut self, f: F) {
        loop {
            if self.insert_point == 0 {
                break;
            }

            self.insert_point -= 1;

            let inst_id = self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()].iseq_ref()
                [self.insert_point];
            let inst = &self.function.body.inst_arena[inst_id];
            if !f(inst) {
                self.insert_point += 1;
                break;
            }
        }
    }
}

impl<'a> BuilderWithLiveInfoEdit<'a> {
    pub fn insert2<T: MachineInstTrait>(&mut self, inst: T, pp: ProgramPoint) {
        let insert_pt = self.insert_point;
        self.insert_point += 1;

        let inst_id = inst.into_id(&mut self.function);

        {
            // update registers' use&def list. TODO: refine code
            let inst = &self.function.body.inst_arena[inst_id];
            for use_ in inst.collect_used_regs() {
                if use_.is_phys_reg() {
                    let range = self.matrix.phys_reg_range.get_mut(use_.as_phys_reg());
                    if range.is_none() {
                        continue;
                    }
                    let range = range.unwrap();
                    let end_point = range.end_point_mut().unwrap();
                    if *end_point <= pp {
                        *end_point = pp
                    }
                } else {
                    let end_point = self
                        .matrix
                        .virt_reg_interval
                        .get_mut(&use_.as_virt_reg())
                        .unwrap()
                        .end_point_mut()
                        .unwrap();
                    if *end_point <= pp {
                        *end_point = pp
                    }
                }
            }
            for &def in inst.collect_defined_regs() {
                let def_ = &self.function.regs_info.arena_ref()[def];
                if let Some(phys_reg) = def_.phys_reg {
                    self.matrix
                        .phys_reg_range
                        .get_or_create(phys_reg)
                        .add_segment(LiveSegment::new(pp, pp));
                } else {
                    self.matrix.add_vreg_entity(def);
                    if let Some(i) = self.matrix.virt_reg_interval.get_mut(&def_.virt_reg) {
                        i.range.add_segment(LiveSegment::new(pp, pp))
                    } else {
                        self.matrix.add_live_interval(
                            def_.virt_reg,
                            LiveRange::new(vec![LiveSegment::new(pp, pp)]),
                        );
                    }
                }
            }
        }

        self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, inst_id);
    }
}

impl<'a> Builder<'a> {
    pub fn new(function: &'a mut MachineFunction) -> Self {
        Self {
            function,
            cur_bb_id: None,
            insert_point: 0,
        }
    }

    pub fn get_cur_bb(&self) -> Option<MachineBasicBlockId> {
        self.cur_bb_id
    }
}

impl<'a> BuilderTrait for Builder<'a> {
    fn set_insert_point_at_entry_block(&mut self) {
        let entry = *self.function.get_entry_bb().unwrap();
        self.set_insert_point_at(0, entry);
    }

    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = self.function.body.basic_blocks.arena[bb_id]
            .iseq_ref()
            .len();
    }

    fn set_insert_point_before_inst(&mut self, inst_id: MachineInstId) -> Option<()> {
        let (bb_id, inst_pos) = self.function.find_inst_pos(inst_id)?;
        self.set_insert_point_at(inst_pos, bb_id);
        Some(())
    }

    fn set_insert_point_after_inst(&mut self, inst_id: MachineInstId) -> Option<()> {
        let (bb_id, inst_pos) = self.function.find_inst_pos(inst_id)?;
        self.set_insert_point_at(inst_pos + 1, bb_id);
        Some(())
    }

    fn insert<T: MachineInstTrait>(&mut self, inst: T) {
        let insert_pt = self.insert_point;
        let inst_id = inst.into_id(&mut self.function);
        self.insert_point += 1;
        self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, inst_id);
    }

    fn back_insert_point_while<F: Fn(&MachineInst) -> bool>(&mut self, f: F) {
        loop {
            if self.insert_point == 0 {
                break;
            }

            self.insert_point -= 1;

            let inst_id = self.function.body.basic_blocks.arena[self.cur_bb_id.unwrap()].iseq_ref()
                [self.insert_point];
            let inst = &self.function.body.inst_arena[inst_id];
            if !f(inst) {
                self.insert_point += 1;
                break;
            }
        }
    }
}
