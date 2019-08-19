use super::{basic_block::*, function::*, instr::*, module::*};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct LiveRegMatrix {
    pub map: FxHashMap<usize, LiveInterval>,
    pub reg_range: FxHashMap<usize, LiveRange>,
}

#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub vreg: usize,
    pub reg: Option<usize>,
    pub range: LiveRange,
}

#[derive(Debug, Clone)]
pub struct LiveRange {
    pub segments: Vec<LiveSegment>,
}

#[derive(Debug, Clone)]
pub struct LiveSegment {
    start: usize,
    end: usize,
}

impl LiveRegMatrix {
    pub fn new(
        map: FxHashMap<usize, LiveInterval>,
        reg_range: FxHashMap<usize, LiveRange>,
    ) -> Self {
        Self { map, reg_range }
    }

    /// Return true if cannot allocate reg for vreg
    pub fn interferes(&mut self, vreg: usize, reg: usize) -> bool {
        if !self.reg_range.contains_key(&reg) {
            return false;
        }

        let r1 = self.reg_range.get(&reg).unwrap();
        println!("{}-> {:?}", reg, r1);
        let r2 = &self.map.get(&vreg).unwrap().range;
        r1.interferes(r2)
    }
}

impl LiveInterval {
    pub fn new(vreg: usize, range: LiveRange) -> Self {
        Self {
            vreg,
            range,
            reg: None,
        }
    }

    pub fn interferes(&self, other: &LiveInterval) -> bool {
        self.range.interferes(&other.range)
    }
}

impl LiveRange {
    pub fn new(segments: Vec<LiveSegment>) -> Self {
        Self { segments }
    }

    pub fn new_empty() -> Self {
        Self { segments: vec![] }
    }

    pub fn add_segment(&mut self, seg: LiveSegment) {
        self.segments.push(seg)
    }

    pub fn interferes(&self, other: &LiveRange) -> bool {
        for seg1 in &self.segments {
            for seg2 in &other.segments {
                if seg1.interferes(seg2) {
                    return true;
                }
            }
        }
        false
    }
}

impl LiveSegment {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn interferes(&self, seg: &LiveSegment) -> bool {
        self.start < seg.end && self.end > seg.start
    }
}

pub struct LivenessAnalysis {}

impl LivenessAnalysis {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze_module(&mut self, module: &MachineModule) {
        for (_, func) in &module.functions {
            self.analyze_function(func);
        }
    }

    pub fn analyze_function(&mut self, cur_func: &MachineFunction) -> LiveRegMatrix {
        self.set_def(cur_func);
        self.visit(cur_func);
        self.construct_live_reg_matrix(cur_func)
    }

    fn set_def(&mut self, cur_func: &MachineFunction) {
        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            for instr_id in &*bb.iseq_ref() {
                self.set_def_on_instr(cur_func, bb, *instr_id);
            }
        }
    }

    fn set_def_on_instr(
        &mut self,
        cur_func: &MachineFunction,
        bb: &MachineBasicBlock,
        instr_id: MachineInstrId,
    ) {
        let instr = &cur_func.instr_arena[instr_id];

        if instr.def.len() > 0 {
            bb.liveness.borrow_mut().def.insert(instr.def[0].clone());
        }
    }

    fn visit(&mut self, cur_func: &MachineFunction) {
        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            for instr_id in &*bb.iseq_ref() {
                self.visit_instr(cur_func, *bb_id, *instr_id);
            }
        }
    }

    fn visit_instr(
        &mut self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        instr_id: MachineInstrId,
    ) {
        let instr = &cur_func.instr_arena[instr_id];

        for (i, operand) in instr.operand.iter().enumerate() {
            if let MachineOperand::Register(reg) = operand {
                self.propagate(
                    cur_func,
                    bb,
                    if instr.opcode == MachineOpcode::Phi {
                        Some(instr.operand[i + 1].as_basic_block())
                    } else {
                        None
                    },
                    reg,
                )
            }
        }
    }

    fn propagate(
        &self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        pred_bb: Option<MachineBasicBlockId>,
        reg: &MachineRegister,
    ) {
        let bb = &cur_func.basic_block_arena[bb];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(reg) {
                return;
            }

            if !bb_liveness.live_in.insert(reg.clone()) {
                // live_in already had the reg
                return;
            }
        }

        if let Some(pred_bb) = pred_bb {
            let pred = &cur_func.basic_block_arena[pred_bb];
            if pred.liveness.borrow_mut().live_out.insert(reg.clone()) {
                // live_out didn't have the reg
                self.propagate(cur_func, pred_bb, None, reg);
            }
            return;
        }

        for pred_id in &bb.pred {
            let pred = &cur_func.basic_block_arena[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(reg.clone()) {
                // live_out didn't have the reg
                self.propagate(cur_func, *pred_id, None, reg);
            }
        }
    }

    pub fn construct_live_reg_matrix(&self, cur_func: &MachineFunction) -> LiveRegMatrix {
        let mut vreg2range: FxHashMap<usize, LiveRange> = FxHashMap::default();
        let mut reg2range: FxHashMap<usize, LiveRange> = FxHashMap::default();

        let mut index = 0;

        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            let liveness = bb.liveness_ref();

            // let mut vreg2seg = FxHashMap::default();

            for livein in &liveness.live_in {
                vreg2range
                    .entry(livein.get_vreg())
                    .or_insert_with(|| LiveRange::new_empty())
                    .add_segment(LiveSegment::new(index, index))
                // vreg2seg.insert(livein.get_vreg(), LiveSegment::new(index, index));
            }

            for instr_id in &*bb.iseq_ref() {
                let instr = &cur_func.instr_arena[*instr_id];

                for operand in &instr.operand {
                    if let MachineOperand::Register(reg) = operand {
                        if reg.get_reg().is_none() {
                            vreg2range
                                .get_mut(&reg.get_vreg())
                                .unwrap()
                                .segments
                                .last_mut()
                                .unwrap()
                                .end = index;
                        }
                    }
                }

                if instr.def.len() > 0 {
                    if instr.def[0].get_reg().is_some() {
                        reg2range
                            .entry(instr.def[0].get_reg().unwrap())
                            .or_insert_with(|| LiveRange::new_empty())
                            .add_segment(LiveSegment::new(index, index));
                    } else {
                        vreg2range
                            .entry(instr.def[0].get_vreg())
                            .or_insert_with(|| LiveRange::new_empty())
                            .add_segment(LiveSegment::new(index, index));
                    }
                }

                for use_ in &instr.imp_use {
                    if let Some(range) = reg2range.get_mut(&use_.get_reg().unwrap()) {
                        range.segments.last_mut().unwrap().end = index;
                    }
                }

                for def in &instr.imp_def {
                    reg2range
                        .entry(def.get_reg().unwrap())
                        .or_insert_with(|| LiveRange::new_empty())
                        .add_segment(LiveSegment::new(index, index))
                }

                index += 1;
            }

            for liveout in &liveness.live_out {
                vreg2range
                    .get_mut(&liveout.get_vreg())
                    .unwrap()
                    .segments
                    .last_mut()
                    .unwrap()
                    .end = index;
                // vreg2seg.get_mut(&liveout.get_vreg()).unwrap().end = index;
            }

            // for (vreg, seg) in vreg2seg {
            //     vreg2range
            //         .entry(vreg)
            //         .or_insert_with(|| LiveRange::new_empty())
            //         .add_segment(seg)
            // }
        }

        when_debug!(for (vreg, range) in &vreg2range {
            println!("vreg{}: {:?}", vreg, range)
        });

        LiveRegMatrix::new(
            vreg2range
                .into_iter()
                .map(|(vreg, range)| (vreg, LiveInterval::new(vreg, range)))
                .collect(),
            reg2range,
        )
    }
}
