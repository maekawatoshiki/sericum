use super::super::register::*;
use super::{basic_block::*, function::*, instr::*, module::*};
use crate::util::allocator::{Raw, RawAllocator};
use rustc_hash::FxHashMap;
use std::cmp::Ordering;

const IDX_STEP: usize = 16;

pub struct LiveRegMatrix {
    pub vreg2entity: FxHashMap<VirtReg, MachineRegister>,
    pub id2pp: FxHashMap<MachineInstrId, ProgramPoint>,
    pub vreg_interval: FxHashMap<VirtReg, LiveInterval>,
    pub reg_range: FxHashMap<RegKey, LiveRange>,
    pub program_points: ProgramPoints,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct RegKey(pub usize);

#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub vreg: VirtReg,
    pub reg: Option<PhysReg>,
    pub range: LiveRange,
}

#[derive(Debug, Clone)]
pub struct LiveRange {
    // TODO: segments should be sorted by LiveSegment.start
    pub segments: Vec<LiveSegment>,
}

#[derive(Debug, Clone)]
pub struct LiveSegment {
    start: ProgramPoint,
    end: ProgramPoint,
}

#[derive(Debug, Clone, Copy)]
pub struct ProgramPoint {
    base: Raw<ProgramPointBase>,
}

#[derive(Debug, Clone, Copy)]
pub struct ProgramPointBase {
    prev: Option<Raw<ProgramPointBase>>,
    next: Option<Raw<ProgramPointBase>>,
    bb: usize,
    idx: usize,
}

pub struct ProgramPoints {
    allocator: RawAllocator<ProgramPointBase>,
}

impl LiveRegMatrix {
    pub fn new(
        vreg2entity: FxHashMap<VirtReg, MachineRegister>,
        id2pp: FxHashMap<MachineInstrId, ProgramPoint>,
        vreg_interval: FxHashMap<VirtReg, LiveInterval>,
        reg_range: FxHashMap<RegKey, LiveRange>,
        program_points: ProgramPoints,
    ) -> Self {
        Self {
            vreg2entity,
            id2pp,
            vreg_interval,
            reg_range,
            program_points,
        }
    }

    pub fn contains_vreg_entity(&self, vreg: &MachineRegister) -> bool {
        self.vreg2entity.contains_key(&vreg.get_vreg())
    }

    pub fn add_vreg_entity(&mut self, vreg: MachineRegister) {
        self.vreg2entity.insert(vreg.get_vreg(), vreg);
    }

    pub fn add_live_interval(&mut self, vreg: VirtReg, range: LiveRange) {
        self.vreg_interval.entry(vreg).or_insert(LiveInterval {
            vreg,
            range,
            reg: None,
        });
    }

    pub fn get_program_point(&self, id: MachineInstrId) -> Option<ProgramPoint> {
        self.id2pp.get(&id).map(|x| *x)
    }

    /// Return false if it's legal to allocate reg for vreg
    pub fn interferes(&self, vreg: VirtReg, reg: PhysReg) -> bool {
        if !self.reg_range.contains_key(&reg.into()) {
            return false;
        }

        let r1 = self.get_reg_live_range(reg).unwrap();
        let r2 = &self.vreg_interval.get(&vreg).unwrap().range;

        r1.interferes(r2)
    }

    pub fn interferes_with_range(&self, vreg: VirtReg, range: LiveRange) -> bool {
        match self.vreg_interval.get(&vreg) {
            Some(interval) => range.interferes(&interval.range),
            None => false,
        }
    }

    pub fn collect_interfering_vregs(&self, vreg: VirtReg) -> Vec<VirtReg> {
        let mut interferings = vec![];
        let vreg_interval = &self.vreg_interval.get(&vreg).unwrap();

        for (cur_vreg, interval) in &self.vreg_interval {
            if vreg == *cur_vreg {
                continue;
            }

            if interval.interferes(vreg_interval) {
                interferings.push(*cur_vreg);
            }
        }

        interferings
    }

    pub fn pick_assigned_and_longest_lived_vreg(&self, vregs: &[VirtReg]) -> Option<VirtReg> {
        let mut longest: Option<(ProgramPoint, VirtReg)> = None;

        for vreg in vregs {
            match longest {
                Some((ref mut endpp1, ref mut vreg1)) => {
                    let interval2 = self.vreg_interval.get(vreg).unwrap();
                    if interval2.reg.is_none() {
                        continue;
                    }
                    let endpp2 = interval2.end_point().unwrap();
                    if *endpp1 < endpp2 {
                        *endpp1 = endpp2;
                        *vreg1 = *vreg
                    }
                }
                None => {
                    let interval = self.vreg_interval.get(vreg).unwrap();
                    if interval.reg.is_none() {
                        continue;
                    }
                    longest = Some((interval.end_point().unwrap(), *vreg))
                }
            }
        }

        longest.and_then(|(_, vreg)| Some(vreg))
    }

    pub fn assign_reg(&mut self, vreg: VirtReg, reg: PhysReg) {
        // assign reg to vreg
        self.vreg_interval.get_mut(&vreg).unwrap().reg = Some(reg);

        let vreg_range = self.get_vreg_interval(vreg).unwrap().range.clone();
        self.get_or_create_reg_live_range(reg)
            .unite_range(vreg_range);
    }

    pub fn unassign_reg(&mut self, vreg: VirtReg) -> Option<PhysReg> {
        let maybe_reg = &mut self.vreg_interval.get_mut(&vreg).unwrap().reg;

        if maybe_reg.is_none() {
            return None;
        }

        let reg = maybe_reg.unwrap();
        // unassign physical register
        *maybe_reg = None;

        let vreg_range = self.get_vreg_interval(vreg).unwrap().range.clone();
        self.get_or_create_reg_live_range(reg)
            .remove_range(vreg_range);

        Some(reg)
    }

    pub fn collect_vregs(&self) -> Vec<VirtReg> {
        self.vreg_interval
            .iter()
            .map(|(vreg, _)| *vreg)
            .collect::<Vec<_>>()
    }

    pub fn get_entity_by_vreg(&self, vreg: VirtReg) -> Option<&MachineRegister> {
        self.vreg2entity.get(&vreg)
    }

    pub fn get_vreg_interval(&self, vreg: VirtReg) -> Option<&LiveInterval> {
        self.vreg_interval.get(&vreg)
    }

    pub fn get_vreg_interval_mut(&mut self, vreg: VirtReg) -> Option<&mut LiveInterval> {
        self.vreg_interval.get_mut(&vreg)
    }

    pub fn get_or_create_reg_live_range(&mut self, reg: PhysReg) -> &mut LiveRange {
        self.reg_range
            .entry(reg.into())
            .or_insert(LiveRange::new_empty())
    }

    pub fn get_reg_live_range(&self, reg: PhysReg) -> Option<&LiveRange> {
        self.reg_range.get(&reg.into())
    }
}

impl From<PhysReg> for RegKey {
    fn from(r: PhysReg) -> Self {
        RegKey(
            r.retrieve() - r.reg_class() as usize
                + r.reg_class().register_file_base_class() as usize,
        )
    }
}

impl LiveInterval {
    pub fn new(vreg: VirtReg, range: LiveRange) -> Self {
        Self {
            vreg,
            range,
            reg: None,
        }
    }

    pub fn interferes(&self, other: &LiveInterval) -> bool {
        self.range.interferes(&other.range)
    }

    pub fn start_point(&self) -> Option<ProgramPoint> {
        self.range.start_point()
    }

    pub fn end_point(&self) -> Option<ProgramPoint> {
        self.range.end_point()
    }

    pub fn end_point_mut(&mut self) -> Option<&mut ProgramPoint> {
        self.range.end_point_mut()
    }
}

impl LiveRange {
    pub fn new(segments: Vec<LiveSegment>) -> Self {
        Self { segments }
    }

    pub fn new_empty() -> Self {
        Self { segments: vec![] }
    }

    pub fn adjust_end_to_start(&mut self) {
        let start = match self.segments.iter().min_by(|x, y| x.start.cmp(&y.start)) {
            Some(seg) => seg.start,
            None => return,
        };
        self.segments = vec![LiveSegment::new(start, start)];
    }

    pub fn add_segment(&mut self, seg: LiveSegment) {
        self.segments.push(seg)
    }

    pub fn unite_range(&mut self, mut range: LiveRange) {
        self.segments.append(&mut range.segments)
    }

    pub fn remove_segment(&mut self, seg: &LiveSegment) {
        self.segments.retain(|seg_| seg_.start != seg.start);
    }

    pub fn remove_range(&mut self, range: LiveRange) {
        for seg in &range.segments {
            self.remove_segment(seg)
        }
    }

    pub fn start_point(&self) -> Option<ProgramPoint> {
        self.segments
            .iter()
            .min_by(|x, y| x.start.cmp(&y.start))
            .and_then(|seg| Some(seg.start))
    }

    pub fn end_point(&self) -> Option<ProgramPoint> {
        self.segments
            .iter()
            .max_by(|x, y| x.end.cmp(&y.end))
            .and_then(|seg| Some(seg.end))
    }

    pub fn end_point_mut(&mut self) -> Option<&mut ProgramPoint> {
        self.segments
            .iter_mut()
            .max_by(|x, y| x.end.cmp(&y.end))
            .and_then(|seg| Some(&mut seg.end))
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
    pub fn new(start: ProgramPoint, end: ProgramPoint) -> Self {
        Self { start, end }
    }

    pub fn interferes(&self, seg: &LiveSegment) -> bool {
        self.start < seg.end && self.end > seg.start
    }
}

impl ProgramPoints {
    pub fn new() -> Self {
        Self {
            allocator: RawAllocator::new(),
        }
    }

    pub fn new_program_point(&mut self, ppb: ProgramPointBase) -> ProgramPoint {
        ProgramPoint::new(self.allocator.alloc(ppb))
    }

    pub fn prev_of(&mut self, pp: ProgramPoint) -> ProgramPoint {
        let mut end: Raw<_> = pp.base;
        let start = end.prev;

        if start.is_none() {
            unimplemented!()
        }

        let mut start: Raw<_> = start.unwrap();

        // need to renumber program points belonging to the same block as pp
        if end.idx() - start.idx() < 2 {
            start.renumber_in_bb();
            return self.prev_of(pp);
        }

        let new_pp = self.new_program_point(ProgramPointBase::new(
            Some(start),
            Some(end),
            start.bb(),
            (end.idx() + start.idx()) / 2,
        ));
        start.next = Some(new_pp.base);
        end.prev = Some(new_pp.base);

        new_pp
    }
}

impl ProgramPoint {
    pub fn new(base: Raw<ProgramPointBase>) -> Self {
        Self { base }
    }

    pub fn set_prev(mut self, pp: Option<ProgramPoint>) -> Self {
        some_then!(mut pp, pp, {
            pp.base.next = Some(self.base);
            self.base.prev = Some(pp.base)
        });
        self
    }

    pub fn idx(&self) -> usize {
        self.base.idx()
    }

    pub fn bb(&self) -> usize {
        self.base.bb()
    }
}

impl ProgramPointBase {
    pub fn new(
        prev: Option<Raw<ProgramPointBase>>,
        next: Option<Raw<ProgramPointBase>>,
        bb: usize,
        idx: usize,
    ) -> Self {
        Self {
            prev,
            next,
            bb,
            idx,
        }
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    pub fn bb(&self) -> usize {
        self.bb
    }

    // pub fn find_bb_start(&self) -> ProgramPointBase {
    //     let mut cur = *self;
    //     while let Some(prev) = cur.prev {
    //         if cur.bb() != prev.bb() {
    //             return cur;
    //         }
    //         cur = *prev;
    //     }
    //     cur
    // }

    pub fn renumber_in_bb(&self) {
        let mut cur = *self;
        while let Some(mut next) = cur.next {
            if cur.bb != next.bb {
                break;
            }
            (*next).idx += IDX_STEP;
            cur = *next;
        }
    }
}

impl Ord for ProgramPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.bb() < other.bb() {
            return Ordering::Less;
        }

        if self.bb() > other.bb() {
            return Ordering::Greater;
        }

        self.idx().cmp(&other.idx())
    }
}

impl Eq for ProgramPoint {}

impl PartialOrd for ProgramPoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ProgramPoint {
    fn eq(&self, other: &Self) -> bool {
        self.bb() == other.bb() && self.idx() == other.idx()
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
        self.clear_bb_liveness_info(cur_func);
        self.set_def(cur_func);
        self.visit(cur_func);
        self.construct_live_reg_matrix(cur_func)
    }

    fn clear_bb_liveness_info(&mut self, cur_func: &MachineFunction) {
        for (_, bb) in &cur_func.basic_blocks.arena {
            bb.liveness_ref_mut().clear();
        }
    }

    fn set_def(&mut self, cur_func: &MachineFunction) {
        for (_, bb) in cur_func.basic_blocks.id_and_block() {
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
        for (bb_id, bb) in cur_func.basic_blocks.id_and_block() {
            for instr_id in &*bb.iseq_ref() {
                self.visit_instr(cur_func, bb_id, *instr_id);
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

        for operand in &instr.operand {
            if let MachineOperand::Register(reg) = operand {
                // live_in and live_out should contain no assigned registers
                if reg.is_phys_reg() {
                    continue;
                }

                self.propagate(cur_func, bb, reg)
            }
        }
    }

    fn propagate(
        &self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        reg: &MachineRegister,
    ) {
        let bb = &cur_func.basic_blocks.arena[bb];

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

        for pred_id in &bb.pred {
            let pred = &cur_func.basic_blocks.arena[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(reg.clone()) {
                // live_out didn't have the reg
                self.propagate(cur_func, *pred_id, reg);
            }
        }
    }

    pub fn construct_live_reg_matrix(&self, cur_func: &MachineFunction) -> LiveRegMatrix {
        let mut vreg2range: FxHashMap<VirtReg, LiveRange> = FxHashMap::default();
        let mut reg2range: FxHashMap<RegKey, LiveRange> = FxHashMap::default();
        let mut id2pp: FxHashMap<MachineInstrId, ProgramPoint> = FxHashMap::default();
        let mut vreg2entity: FxHashMap<VirtReg, MachineRegister> = FxHashMap::default();
        let mut program_points = ProgramPoints::new();

        let mut last_pp: Option<ProgramPoint> = None;
        let mut bb_idx = 0;

        // TODO: Refine code

        for (_, bb) in cur_func.basic_blocks.id_and_block() {
            let mut index = 0;
            let liveness = bb.liveness_ref();

            #[rustfmt::skip]
            macro_rules! cur_pp { () => {{
                last_pp = Some(program_points.new_program_point(
                        ProgramPointBase::new(None, None, bb_idx, index))
                        .set_prev(last_pp));
                last_pp.unwrap()
            }};}

            for livein in &liveness.live_in {
                if livein.get_reg().is_some() {
                    continue;
                }

                vreg2range
                    .entry(livein.get_vreg())
                    .or_insert_with(|| LiveRange::new_empty())
                    .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()))
            }

            for instr_id in &*bb.iseq_ref() {
                let instr = &cur_func.instr_arena[*instr_id];

                id2pp.insert(*instr_id, cur_pp!());

                for def_reg in instr.collect_defined_regs() {
                    vreg2entity.insert(def_reg.get_vreg(), def_reg);
                }

                for operand in &instr.operand {
                    if let MachineOperand::Register(reg) = operand {
                        if let Some(phy_reg) = reg.get_reg() {
                            if let Some(range) = reg2range.get_mut(&phy_reg.into()) {
                                range.segments.last_mut().unwrap().end = cur_pp!();
                            }
                        }

                        if reg.get_reg().is_none() {
                            vreg2range
                                .get_mut(&reg.get_vreg())
                                .unwrap()
                                .segments
                                .last_mut()
                                .unwrap()
                                .end = cur_pp!();
                        }
                    }
                }

                // TODO: def.len() > 0 is no easy to understand
                if instr.def.len() > 0 {
                    if instr.def[0].get_reg().is_some() {
                        reg2range
                            .entry(instr.def[0].get_reg().unwrap().into())
                            .or_insert_with(|| LiveRange::new_empty())
                            .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()));
                    } else {
                        vreg2range
                            .entry(instr.def[0].get_vreg())
                            .or_insert_with(|| LiveRange::new_empty())
                            .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()));
                    }
                }

                for use_ in &instr.imp_use {
                    if let Some(range) = reg2range.get_mut(&use_.get_reg().unwrap().into()) {
                        range.segments.last_mut().unwrap().end = cur_pp!();
                    }
                }

                for def in &instr.imp_def {
                    reg2range
                        .entry(def.get_reg().unwrap().into())
                        .or_insert_with(|| LiveRange::new_empty())
                        .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()))
                }

                index += IDX_STEP;
            }

            for liveout in &liveness.live_out {
                if liveout.get_reg().is_some() {
                    continue;
                }

                vreg2range
                    .get_mut(&liveout.get_vreg())
                    .unwrap()
                    .segments
                    .last_mut()
                    .unwrap()
                    .end = cur_pp!();
            }

            bb_idx += 1;
        }

        // debug!(for (vreg, range) in &vreg2range {
        //     println!("{:?}: {:?}", vreg, range)
        // });

        LiveRegMatrix::new(
            vreg2entity,
            id2pp,
            vreg2range
                .into_iter()
                .map(|(vreg, range)| (vreg, LiveInterval::new(vreg, range)))
                .collect(),
            reg2range,
            program_points,
        )
    }
}
