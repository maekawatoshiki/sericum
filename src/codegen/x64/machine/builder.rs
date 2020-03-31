use super::{
    basic_block::*,
    function::*,
    instr::*,
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
    fn into_id(self, f: &mut MachineFunction) -> MachineInstrId;
}

pub trait BuilderTrait {
    fn set_insert_point_at_entry_bb(&mut self);
    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId);
    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId);
    fn set_insert_point_before_instr(&mut self, instr_id: MachineInstrId) -> Option<()>;
    fn set_insert_point_after_instr(&mut self, instr_id: MachineInstrId) -> Option<()>;
    fn insert<T: MachineInstTrait>(&mut self, inst: T);
    fn back_insert_point(&mut self);
}

impl MachineInstTrait for MachineInstrId {
    fn into_id(self, _: &mut MachineFunction) -> MachineInstrId {
        self
    }
}

impl MachineInstTrait for MachineInstr {
    fn into_id(self, f: &mut MachineFunction) -> MachineInstrId {
        f.instr_arena.alloc(self)
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
        let bb = &self.function.basic_blocks.arena[self.cur_bb_id.unwrap()];
        let iseq = bb.iseq_ref();
        let pp = self.matrix.get_program_point(iseq[insert_pt]).unwrap();
        self.matrix.program_points.prev_of(pp)
    }
}

impl<'a> BuilderTrait for BuilderWithLiveInfoEdit<'a> {
    fn set_insert_point_at_entry_bb(&mut self) {
        let entry = *self.function.get_entry_bb().unwrap();
        self.set_insert_point_at(0, entry);
    }

    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = self.function.basic_blocks.arena[bb_id].iseq_ref().len();
    }

    fn set_insert_point_before_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos, bb_id);
        Some(())
    }

    fn set_insert_point_after_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos + 1, bb_id);
        Some(())
    }

    fn insert<T: MachineInstTrait>(&mut self, inst: T) {
        let insert_pt = self.insert_point;
        self.insert_point += 1;

        let pp = self.calc_program_point(insert_pt);

        let instr_id = inst.into_id(&mut self.function);
        self.matrix.id2pp.insert(instr_id, pp);

        {
            // update registers' use&def list. TODO: refine code
            let instr = &self.function.instr_arena[instr_id];
            for def in &instr.def {
                self.matrix.add_vreg_entity(def.clone());
                self.matrix.add_live_interval(
                    def.get_vreg(),
                    LiveRange::new(vec![LiveSegment::new(pp, pp)]),
                );
            }
            for use_ in instr.collect_used_regs() {
                let end_point = self
                    .matrix
                    .get_vreg_interval_mut(use_.get_vreg())
                    .unwrap()
                    .end_point_mut()
                    .unwrap();
                if *end_point <= pp {
                    *end_point = pp
                }
            }
        }

        self.function.basic_blocks.arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, instr_id);
    }

    fn back_insert_point(&mut self) {
        self.insert_point -= 1;
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
    fn set_insert_point_at_entry_bb(&mut self) {
        let entry = *self.function.get_entry_bb().unwrap();
        self.set_insert_point_at(0, entry);
    }

    fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    fn set_insert_point_at_end(&mut self, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = self.function.basic_blocks.arena[bb_id].iseq_ref().len();
    }

    fn set_insert_point_before_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos, bb_id);
        Some(())
    }

    fn set_insert_point_after_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos + 1, bb_id);
        Some(())
    }

    fn insert<T: MachineInstTrait>(&mut self, inst: T) {
        let insert_pt = self.insert_point;
        let instr_id = inst.into_id(&mut self.function);
        self.insert_point += 1;
        self.function.basic_blocks.arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, instr_id);
    }

    fn back_insert_point(&mut self) {
        self.insert_point -= 1;
    }
}
