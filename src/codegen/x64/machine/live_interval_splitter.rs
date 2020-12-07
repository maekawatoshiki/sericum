use super::super::{
    dag::mc_convert::{mov_mx, mov_rx},
    frame_object::*,
    machine::register::*,
};
use super::inst::*;
use crate::analysis::{dom_tree::DominatorTree, loops::Loops};
use crate::codegen::common::machine::{
    basic_block::*, builder::*, function::*, liveness::*, regalloc::*,
};
use rustc_hash::FxHashSet;

pub struct LiveIntervalSplitter<'a> {
    func: &'a mut MachineFunction,
    matrix: &'a mut LiveRegMatrix,
    preserve_insts: &'a mut Vec<PreserveStoreLoad>,
}

impl<'a> LiveIntervalSplitter<'a> {
    pub fn new(
        func: &'a mut MachineFunction,
        matrix: &'a mut LiveRegMatrix,
        preserve_insts: &'a mut Vec<PreserveStoreLoad>,
    ) -> Self {
        Self {
            func,
            matrix,
            preserve_insts,
        }
    }

    // TODO: REFINE CODE
    pub fn split(
        &mut self,
        dom_tree: &DominatorTree<MachineBasicBlock>,
        loops: &Loops<MachineBasicBlock>,
        reg: &RegisterId,
        slot: &FrameIndexInfo,
        after_store: &MachineInstId,
        before_load: &MachineInstId,
    ) -> Option<RegisterId> {
        let mut s = FxHashSet::default();
        fn update_live_info(
            reg: &RegisterId,
            new_reg: RegisterId,
            start: MachineBasicBlockId,
            bb_id: MachineBasicBlockId,
            bbs: &mut MachineBasicBlocks,
            ss: &mut FxHashSet<MachineBasicBlockId>,
        ) {
            let bb = &bbs.arena[bb_id];
            if !ss.insert(bb_id) {
                return;
            }
            let liveness = bbs.liveness.get_mut(&bb_id).unwrap();
            let live_in_removed = bb_id != start && liveness.live_in.remove(reg);
            let live_out_removed = liveness.live_out.remove(reg);
            if live_in_removed {
                liveness.add_live_in(new_reg);
            }
            if live_out_removed {
                liveness.add_live_out(new_reg);
            }
            let updated = live_in_removed || live_out_removed;
            if !updated {
                return;
            }
            for s in bb.succ.clone() {
                update_live_info(reg, new_reg, start, s, bbs, ss);
            }
        }

        // virtual registers only
        if reg.is_phys_reg() {
            return None;
        }

        // reg = Copy X
        // reg = INST reg, Y
        if {
            let r = &self.func.regs_info.arena_ref()[*reg];
            r.defs.len() == 2 && r.is_early_clobber()
        } {
            return None;
        }

        // no multiple defines
        if self.func.regs_info.arena_ref()[*reg].defs.len() > 1 {
            return None;
        }

        // splitting not in any loop
        {
            let p1 = self.func.body.inst_arena[*before_load].parent;
            let p2 = self.func.body.inst_arena[*after_store].parent;
            if loops.get_loop_for(p1).is_some() || loops.get_loop_for(p2).is_some() {
                return None;
            }
        }

        {
            let x = self.func.body.inst_arena[*after_store].parent;
            let r = &self.func.regs_info.arena_ref()[*reg];
            for &u in &r.uses {
                let y = self.func.body.inst_arena[u].parent;
                if !dom_tree.path_exists(x, y) && !dom_tree.dominate_bb(y, x) {
                    return None;
                }
                if dom_tree.path_exists(x, y) && !dom_tree.dominate_bb(x, y) {
                    return None;
                }
            }
        }

        debug!(println!("Split {:?} in {:?}", reg, self.func.name));

        let after_store_pp = self.matrix.get_program_point(*after_store).unwrap();
        let before_load_pp = self.matrix.get_program_point(*before_load).unwrap();

        let parent = self.func.body.inst_arena[*after_store].parent;
        let src = MachineOperand::Register(RegisterOperand::new(*reg));
        let rbp = RegisterOperand::new(self.func.regs_info.get_phys_reg(GR64::RBP));
        let mem = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *slot));
        let store_id = self.func.alloc_inst(MachineInst::new_simple(
            mov_mx(&self.func.regs_info, &src).unwrap(),
            vec![mem, src],
            parent,
        ));
        let store_pp = self.matrix.program_points.prev_of(after_store_pp);
        self.matrix.id2pp.insert(store_id, store_pp);

        let reg_intvl = self
            .matrix
            .virt_reg_interval
            .get_mut(&reg.as_virt_reg())
            .unwrap();
        assert!(reg_intvl.start_point().unwrap() <= store_pp);
        let new_reg_end_point = reg_intvl.end_point().unwrap();
        reg_intvl.range.remove_segment(&LiveSegment::new(
            before_load_pp,
            reg_intvl.end_point().unwrap(),
        ));

        let mut new_reg_uses = FxHashSet::default();
        for &id in &self.func.regs_info.arena_ref_mut()[*reg].uses {
            if self.matrix.get_program_point(id).unwrap() > store_pp {
                new_reg_uses.insert(id);
            }
        }
        assert!(self.func.regs_info.arena_ref_mut()[*reg].uses.len() > 0);
        let new_reg = self.func.regs_info.new_virt_reg_from(&reg);
        self.matrix.add_virt_reg(new_reg);
        for &id in &new_reg_uses {
            let inst = &mut self.func.body.inst_arena[id];
            inst.replace_operand_register(&self.func.regs_info, *reg, new_reg);
        }

        let parent = self.func.body.inst_arena[*before_load].parent;

        update_live_info(
            reg,
            new_reg,
            parent,
            parent,
            &mut self.func.body.basic_blocks,
            &mut s,
        );

        let src = MachineOperand::Mem(MachineMemOperand::BaseFi(rbp, *slot));
        let opcode = mov_rx(self.func.regs_info.arena_ref()[new_reg].reg_class, &src).unwrap();
        let load_id = self.func.alloc_inst(
            MachineInst::new_simple(opcode, vec![src], parent)
                .with_def(vec![RegisterOperand::new(new_reg)]),
        );
        let load_pp = self.matrix.program_points.next_of(before_load_pp);
        self.matrix.id2pp.insert(load_id, load_pp);
        self.matrix.virt_reg_interval.add(
            new_reg.as_virt_reg(),
            LiveRange::new(vec![LiveSegment::new(load_pp, new_reg_end_point)]),
        );
        {
            let liveness = self
                .func
                .body
                .basic_blocks
                .liveness
                .get_mut(&parent)
                .unwrap();
            liveness.add_def(new_reg);
        }

        let mut builder = BuilderWithLiveInfoEdit::new(self.matrix, self.func);

        builder.set_insert_point_before_inst(*after_store);
        builder.insert(store_id);

        builder.set_insert_point_after_inst(*before_load);
        builder.insert(load_id);

        self.preserve_insts
            .push(PreserveStoreLoad(store_id, load_id));

        // TODO: We need a suitable way to calculate spill weight
        let weight = self
            .matrix
            .virt_reg_interval
            .get(&reg.as_virt_reg())
            .unwrap()
            .spill_weight;
        self.matrix
            .virt_reg_interval
            .get_mut(&new_reg.as_virt_reg())
            .unwrap()
            .spill_weight = weight;

        debug!(println!("new split reg {:?}", new_reg));

        Some(new_reg)
    }
}
