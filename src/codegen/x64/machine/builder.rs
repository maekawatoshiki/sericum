use super::{basic_block::*, function::*, instr::*};

pub struct Builder<'a> {
    pub function: &'a mut MachineFunction,
    cur_bb_id: Option<MachineBasicBlockId>,
    insert_point: usize,
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
        self.function.basic_blocks[self.cur_bb_id.unwrap()]
            .iseq_ref_mut()
            .insert(insert_pt, instr_id);
    }
}
