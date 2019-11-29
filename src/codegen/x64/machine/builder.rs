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

// TODO: Refine code: Something like BuilderTrait is needed

impl<'a> BuilderWithLiveInfoEdit<'a> {
    pub fn new(matrix: &'a mut LiveRegMatrix, function: &'a mut MachineFunction) -> Self {
        Self {
            matrix,
            function,
            cur_bb_id: None,
            insert_point: 0,
        }
    }

    pub fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    pub fn set_insert_point_before_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos, bb_id);
        Some(())
    }

    pub fn set_insert_point_after_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos + 1, bb_id);
        Some(())
    }

    pub fn insert_instr_id(&mut self, instr_id: MachineInstrId) {
        let insert_pt = self.insert_point;
        self.insert_point += 1;

        let pp = {
            // calculate program point for given instr (=instr_id)
            let bb = &self.function.basic_block_arena[self.cur_bb_id.unwrap()];
            let iseq = bb.iseq_ref();
            let end = *self.matrix.id2pp.get(&iseq[insert_pt]).unwrap();
            let start = if insert_pt == 0 {
                end // TODO
            } else {
                *self.matrix.id2pp.get(&iseq[insert_pt - 1]).unwrap()
            };
            assert!(end.idx() - start.idx() >= 2); // TODO
            ProgramPoint::new(start.bb(), (end.idx() + start.idx()) / 2)
        };

        self.matrix.id2pp.insert(instr_id, pp);

        {
            let instr = &self.function.instr_arena[instr_id];
            for def in instr.collect_defined_regs() {
                self.matrix.add_vreg_entity(def.clone());
                self.matrix.add_live_interval(
                    def.get_vreg(),
                    LiveRange::new(vec![LiveSegment::new(pp, pp)]),
                );
            }
            let instr_pp = *self.matrix.id2pp.get(&instr_id).unwrap();
            for use_ in instr.collect_used_regs() {
                *self
                    .matrix
                    .get_vreg_interval_mut(use_.get_vreg())
                    .unwrap()
                    .end_point_mut()
                    .unwrap() = instr_pp;
            }
        }

        self.function.basic_block_arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, instr_id);
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

    pub fn set_insert_point_at(&mut self, pt: usize, bb_id: MachineBasicBlockId) {
        self.cur_bb_id = Some(bb_id);
        self.insert_point = pt;
    }

    pub fn set_insert_point_before_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos, bb_id);
        Some(())
    }

    pub fn set_insert_point_after_instr(&mut self, instr_id: MachineInstrId) -> Option<()> {
        let (bb_id, instr_pos) = self.function.find_instr_pos(instr_id)?;
        self.set_insert_point_at(instr_pos + 1, bb_id);
        Some(())
    }

    pub fn insert_instr_id(&mut self, instr_id: MachineInstrId) {
        let insert_pt = self.insert_point;
        self.insert_point += 1;
        self.function.basic_block_arena[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, instr_id);
    }
}
