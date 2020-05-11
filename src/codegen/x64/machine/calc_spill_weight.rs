use super::{basic_block::MachineBasicBlock, function::MachineFunction, liveness::LiveRegMatrix};
use crate::analysis::{
    dom_tree::DominatorTreeConstructor,
    loops::{Loops, LoopsConstructor},
};
use rustc_hash::FxHashSet;

struct SpillWeightCalculator<'a> {
    func: &'a MachineFunction,
    matrix: &'a mut LiveRegMatrix,
    loops: Loops<MachineBasicBlock>,
}

pub fn calc_spill_weight(func: &MachineFunction, matrix: &mut LiveRegMatrix) {
    let dom_tree = DominatorTreeConstructor::new(func).construct();
    let loops = LoopsConstructor::new(&dom_tree, &func.body.basic_blocks).analyze();
    SpillWeightCalculator {
        func,
        matrix,
        loops,
    }
    .run()
}

impl<'a> SpillWeightCalculator<'a> {
    // VERY simple spill weight calculation
    pub fn run(&mut self) {
        for (vreg, li) in self.matrix.virt_reg_interval.inner_mut() {
            let inst_arena = &self.func.body.inst_arena;
            let reg_id = *self.matrix.vreg2entity.get(vreg).unwrap();
            let uses = &self.func.regs_info.arena_ref()[reg_id].uses;
            let bbs_used_in = uses
                .iter()
                .map(|id| inst_arena[*id].parent)
                .collect::<FxHashSet<_>>()
                .len();
            let used_in_loop = {
                let loops = &self.loops;
                uses.iter()
                    .any(|id| loops.get_loop_for(inst_arena[*id].parent).is_some())
            };
            let uses = uses.len();
            li.spill_weight = uses as f32 * bbs_used_in as f32;
            if used_in_loop {
                li.spill_weight *= 3.0;
            }
            // println!("{:?} - {:?}", vreg, li.spill_weight);
        }
    }
}
