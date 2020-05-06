use super::{function::MachineFunction, liveness::LiveRegMatrix};
use rustc_hash::FxHashSet;

struct SpillWeightCalculator<'a> {
    func: &'a MachineFunction,
    matrix: &'a mut LiveRegMatrix,
}

pub fn calc_spill_weight(func: &MachineFunction, matrix: &mut LiveRegMatrix) {
    SpillWeightCalculator { func, matrix }.run()
}

impl<'a> SpillWeightCalculator<'a> {
    // VERY simple spill weight calculation
    pub fn run(&mut self) {
        for (vreg, li) in self.matrix.virt_reg_interval.inner_mut() {
            let inst_arena = &self.func.body.inst_arena;
            let reg_id = *self.matrix.vreg2entity.get(vreg).unwrap();
            let uses = &self.func.regs_info.arena_ref()[reg_id.id].uses;
            let bbs_used_in = uses
                .iter()
                .map(|id| inst_arena[*id].parent)
                .collect::<FxHashSet<_>>()
                .len();
            let uses = uses.len();
            li.spill_weight = uses as f32 * bbs_used_in as f32;
            // println!("{:?} - {:?}", vreg, li.spill_weight);
        }
    }
}
