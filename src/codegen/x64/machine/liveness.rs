use super::super::register::*;
use super::{basic_block::*, function::*, inst::*, module::*};
use crate::util::allocator::{Raw, RawAllocator};
use rustc_hash::FxHashMap;
use std::cmp::Ordering;
use std::fmt;

const IDX_STEP: usize = 16;

pub struct LiveRegMatrix {
    pub vreg2entity: FxHashMap<VirtReg, RegisterId>,
    pub id2pp: FxHashMap<MachineInstId, ProgramPoint>,
    pub virt_reg_interval: VirtRegInterval,
    pub phys_reg_range: PhysRegRange,
    pub program_points: ProgramPoints,
}

pub struct PhysRegRange(FxHashMap<RegKey, LiveRange>);
pub struct VirtRegInterval(FxHashMap<VirtReg, LiveInterval>);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct RegKey(usize);

#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub vreg: VirtReg,
    pub reg: Option<PhysReg>,
    pub range: LiveRange,
    pub spill_weight: f32,
}

#[derive(Debug, Clone)]
pub struct LiveRange {
    // TODO: segments should be sorted by LiveSegment.start
    pub segments: Vec<LiveSegment>,
    pub remove: Vec<LiveSegment>,
}

#[derive(Debug, Clone, Copy)]
pub struct LiveSegment {
    start: ProgramPoint,
    end: ProgramPoint,
}

#[derive(Clone, Copy)]
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
        vreg2entity: FxHashMap<VirtReg, RegisterId>,
        id2pp: FxHashMap<MachineInstId, ProgramPoint>,
        virt_reg_interval: VirtRegInterval,
        phys_reg_range: PhysRegRange,
        program_points: ProgramPoints,
    ) -> Self {
        Self {
            vreg2entity,
            id2pp,
            virt_reg_interval,
            phys_reg_range,
            program_points,
        }
    }

    pub fn contains_vreg_entity(&self, vreg: &MachineRegister) -> bool {
        self.vreg2entity.contains_key(&vreg.get_vreg())
    }

    pub fn add_vreg_entity(&mut self, reg: RegisterId) {
        self.vreg2entity.insert(reg.as_virt_reg(), reg);
    }

    pub fn add_live_interval(&mut self, vreg: VirtReg, range: LiveRange) {
        self.virt_reg_interval.add(vreg, range)
    }

    pub fn get_program_point(&self, id: MachineInstId) -> Option<ProgramPoint> {
        self.id2pp.get(&id).map(|x| *x)
    }

    /// Return false if it's legal to allocate reg for vreg
    pub fn interferes(&self, vreg: VirtReg, reg: PhysReg) -> bool {
        let r1 = match self.phys_reg_range.get(reg) {
            Some(r1) => r1,
            None => return false,
        };
        let r2 = &self.virt_reg_interval.get(&vreg).unwrap().range;

        r1.interferes(r2)
    }

    /// Return false if it's legal to allocate reg for vreg
    pub fn interferes_virt_regs(&self, vreg1: VirtReg, vreg2: VirtReg) -> bool {
        let r1 = &self.virt_reg_interval.get(&vreg1).unwrap().range;
        let r2 = &self.virt_reg_interval.get(&vreg2).unwrap().range;
        r1.interferes(r2)
    }

    pub fn interferes_with_range(&self, vreg: VirtReg, range: &LiveRange) -> bool {
        match self.virt_reg_interval.get(&vreg) {
            Some(interval) => range.interferes(&interval.range),
            None => false,
        }
    }

    pub fn interferes_phys_with_range(&self, reg: PhysReg, range: &LiveRange) -> bool {
        match self.phys_reg_range.get(reg.into()) {
            Some(range2) => range.interferes(range2),
            None => false,
        }
    }

    pub fn collect_interfering_assigned_regs(&self, vreg: VirtReg) -> Vec<VirtReg> {
        let mut interferings = vec![];
        let i = self.virt_reg_interval.get(&vreg).unwrap();

        for (cur_vreg, interval) in self.virt_reg_interval.inner() {
            if vreg == *cur_vreg {
                continue;
            }

            if interval.interferes(i) && interval.reg.is_some() {
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
                    let interval2 = self.virt_reg_interval.get(vreg).unwrap();
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
                    let interval = self.virt_reg_interval.get(vreg).unwrap();
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
        self.virt_reg_interval.get_mut(&vreg).unwrap().reg = Some(reg);

        let range = self.virt_reg_interval.get(&vreg).unwrap().range.clone();
        self.phys_reg_range.get_or_create(reg).unite_range(range);
    }

    pub fn unassign_reg(&mut self, vreg: VirtReg) -> Option<PhysReg> {
        let maybe_reg = &mut self.virt_reg_interval.get_mut(&vreg).unwrap().reg;

        if maybe_reg.is_none() {
            return None;
        }

        let reg = maybe_reg.unwrap();
        // unassign physical register
        *maybe_reg = None;

        let range = &self.virt_reg_interval.get(&vreg).unwrap().range;
        self.phys_reg_range.get_or_create(reg).remove_range(range);
        // assert_eq!(
        //     self.phys_reg_range.get_or_create(reg).interferes(range),
        //     false
        // );

        Some(reg)
    }

    /// v2 merges into v1 and remove v2 from matrix
    pub fn merge_virt_regs(&mut self, regs_info: &RegistersInfo, v1: VirtReg, v2: VirtReg) {
        let v2_e = self.vreg2entity.remove(&v2).unwrap();
        let v1_e = *self.vreg2entity.get(&v1).unwrap();
        let mut arena = regs_info.arena_ref_mut();
        for use_ in arena[v2_e].uses.clone() {
            arena[v1_e].add_use(use_)
        }
        let v2_i = self.virt_reg_interval.remove(&v2).unwrap();
        let v1_i = self.virt_reg_interval.get_mut(&v1).unwrap();
        v1_i.range.unite_range(v2_i.range);
    }

    /// r2 merges into r1 and remove r2 from matrix
    pub fn merge_regs(&mut self, r1: PhysReg, r2: VirtReg) {
        self.vreg2entity.remove(&r2);
        let r2_i = self.virt_reg_interval.remove(&r2).unwrap();
        let r1_i = self.phys_reg_range.get_mut(r1).unwrap();
        r1_i.unite_range(r2_i.range);
    }

    pub fn collect_virt_regs(&self) -> Vec<VirtReg> {
        self.virt_reg_interval
            .inner()
            .iter()
            .map(|(vreg, _)| *vreg)
            .collect::<Vec<_>>()
    }

    pub fn get_entity_by_vreg(&self, vreg: VirtReg) -> Option<&RegisterId> {
        self.vreg2entity.get(&vreg)
    }
}

impl PhysRegRange {
    pub fn get_or_create(&mut self, reg: PhysReg) -> &mut LiveRange {
        self.0.entry(reg.into()).or_insert(LiveRange::new_empty())
    }

    pub fn get(&self, reg: PhysReg) -> Option<&LiveRange> {
        self.0.get(&reg.into())
    }

    pub fn get_mut(&mut self, reg: PhysReg) -> Option<&mut LiveRange> {
        self.0.get_mut(&reg.into())
    }
}

impl VirtRegInterval {
    pub fn inner(&self) -> &FxHashMap<VirtReg, LiveInterval> {
        &self.0
    }

    pub fn inner_mut(&mut self) -> &mut FxHashMap<VirtReg, LiveInterval> {
        &mut self.0
    }

    pub fn get(&self, vreg: &VirtReg) -> Option<&LiveInterval> {
        self.0.get(vreg)
    }

    pub fn get_mut(&mut self, vreg: &VirtReg) -> Option<&mut LiveInterval> {
        self.0.get_mut(vreg)
    }

    pub fn remove(&mut self, vreg: &VirtReg) -> Option<LiveInterval> {
        self.0.remove(vreg)
    }

    pub fn add(&mut self, vreg: VirtReg, range: LiveRange) {
        self.0.entry(vreg).or_insert(LiveInterval::new(vreg, range));
    }

    pub fn collect_virt_regs(&self) -> Vec<VirtReg> {
        self.0.iter().map(|(vreg, _)| *vreg).collect()
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
            spill_weight: 0.0,
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
        Self {
            segments,
            remove: Vec::new(),
        }
    }

    pub fn new_empty() -> Self {
        Self {
            segments: vec![],
            remove: Vec::new(),
        }
    }

    pub fn adjust_end_to_start(&mut self) {
        let start = match self.segments.iter().min_by(|x, y| x.start.cmp(&y.start)) {
            Some(seg) => seg.start,
            None => return,
        };
        self.segments = vec![LiveSegment::new(start, start)];
    }

    pub fn add_segment(&mut self, seg: LiveSegment) {
        self.segments.push(seg);
        // let pt = self.segments.binary_search_by(|s| s.start.cmp(&seg.start));
        // let found = pt.is_ok();
        // let pt = pt.unwrap_or_else(|x| x);
        // if pt == 0 {
        //     self.segments.insert(pt, seg);
        //     return;
        // }
        // /* pt > 0 */
        // let mpt = if found { pt } else { pt - 1 };
        // let prev_seg = self.segments[mpt];
        // if prev_seg.end == seg.start {
        //     self.segments[mpt].end = seg.end;
        //     return;
        // }
        // if prev_seg.start != seg.start {
        //     self.segments.insert(pt, seg);
        //     return;
        // }
        // // prev_seg.start == seg.start
        // if prev_seg.end < seg.end {
        //     self.segments[mpt].end = seg.end;
        //     return;
        // }
    }

    pub fn unite_range(&mut self, range: LiveRange) {
        for seg in range.segments {
            self.add_segment(seg)
        }
    }

    pub fn remove_segment(&mut self, seg: &LiveSegment) {
        // self.remove.push(seg);
        // let mut new_segments = vec![];
        // let mut a=None;
        let a = self.segments.len();
        self.segments
            .retain(|seg1| !(seg1.start == seg.start && seg.end == seg1.end));
        assert_ne!(self.segments.len(), a);
        // for seg1 in &self.segments {
        //         return;
        //         // break;
        //         // continue;
        //     }
        // }
        // panic!()
        //     if !seg1.interferes(seg) {
        //         new_segments.push(*seg1);
        //         continue;
        //     }
        //     let mut a = false;
        //     if seg.start < seg1.start && seg1.end < seg.end {
        //         continue;
        //     }
        //     if seg.start == seg1.start && seg1.end == seg.end {
        //         continue;
        //     }
        //     // panic!("{:?} {:?}", seg, seg1);
        //     let aa = if seg1.start < seg.start {
        //         LiveSegment::new(seg1.start, seg.start)
        //     } else {
        //         LiveSegment::new(seg.start, seg1.start)
        //     };
        //     let bb = if seg1.end < seg.end {
        //         LiveSegment::new(seg1.end, seg.end)
        //     } else {
        //         LiveSegment::new(seg.end, seg1.end)
        //     };
        //     if seg1.start <= aa.start && aa.end <= seg1.end {
        //         a = true;
        //         new_segments.push(aa)
        //     }
        //     if seg1.start <= bb.start && bb.end <= seg1.end {
        //         a = true;
        //         new_segments.push(bb)
        //     }
        //     // assert_eq!(a, true);
        // }
        // self.segments = new_segments;
        // return;
    }

    pub fn remove_range(&mut self, range: &LiveRange) {
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
        // let s = self
        //     .segments
        //     .binary_search_by(|s| s.start.cmp(&other.segments[0].end))
        //     .unwrap_or_else(|x| x)
        //     .saturating_sub(1);
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
        (self.start < seg.end && self.end > seg.start)
        // || self.start == seg.start
        // || self.end == seg.end
        // || (self.start == seg.start && self.end == seg.end)
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

impl fmt::Debug for ProgramPoint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.bb(), self.idx())
    }
}

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
        self.clear_liveness_info(cur_func);
        self.set_def(cur_func);
        self.visit(cur_func);
        self.construct_live_reg_matrix(cur_func)
    }

    fn clear_liveness_info(&mut self, cur_func: &MachineFunction) {
        for (_, bb) in &cur_func.body.basic_blocks.arena {
            bb.liveness_ref_mut().clear();
        }
    }

    fn set_def(&mut self, cur_func: &MachineFunction) {
        for (_, bb, iiter) in cur_func.body.mbb_iter() {
            for (_, inst) in iiter {
                self.set_def_on_inst(bb, inst);
            }
        }
    }

    fn set_def_on_inst(&mut self, bb: &MachineBasicBlock, inst: &MachineInst) {
        for &reg in &inst.def {
            bb.liveness.borrow_mut().add_def(reg);
        }
    }

    fn visit(&mut self, cur_func: &MachineFunction) {
        for (bb_id, _, iiter) in cur_func.body.mbb_iter() {
            for (_, inst) in iiter {
                self.visit_inst(cur_func, bb_id, inst);
            }
        }
    }

    fn visit_inst(
        &mut self,
        cur_func: &MachineFunction,
        bb: MachineBasicBlockId,
        inst: &MachineInst,
    ) {
        for operand in &inst.operand {
            // live_in and live_out should contain no assigned(physical) registers
            match operand {
                MachineOperand::Register(reg)
                | MachineOperand::Mem(MachineMemOperand::BaseFi(reg, _))
                | MachineOperand::Mem(MachineMemOperand::BaseFiOff(reg, _, _))
                | MachineOperand::Mem(MachineMemOperand::BaseOff(reg, _))
                | MachineOperand::Mem(MachineMemOperand::Base(reg)) => {
                    self.propagate(cur_func, bb, *reg)
                }
                MachineOperand::Mem(MachineMemOperand::BaseAlignOff(reg, _, reg2))
                | MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(reg, _, _, reg2)) => {
                    self.propagate(cur_func, bb, *reg);
                    self.propagate(cur_func, bb, *reg2);
                }
                _ => {}
            }
        }
    }

    fn propagate(&self, cur_func: &MachineFunction, bb: MachineBasicBlockId, reg: RegisterId) {
        let bb = &cur_func.body.basic_blocks.arena[bb];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(&reg) {
                return;
            }

            if !bb_liveness.add_live_in(reg) {
                // live_in already had the reg
                return;
            }
        }

        for pred_id in &bb.pred {
            let pred = &cur_func.body.basic_blocks.arena[*pred_id];
            if pred.liveness.borrow_mut().add_live_out(reg) {
                // live_out didn't have the reg
                self.propagate(cur_func, *pred_id, reg);
            }
        }
    }

    pub fn construct_live_reg_matrix(&self, cur_func: &MachineFunction) -> LiveRegMatrix {
        let mut vreg2range: FxHashMap<VirtReg, LiveRange> = FxHashMap::default();
        let mut reg2range: PhysRegRange = PhysRegRange(FxHashMap::default());
        let mut id2pp: FxHashMap<MachineInstId, ProgramPoint> = FxHashMap::default();
        let mut vreg2entity: FxHashMap<VirtReg, RegisterId> = FxHashMap::default();
        let mut program_points = ProgramPoints::new();

        let mut last_pp: Option<ProgramPoint> = None;
        let mut bb_idx = 0;

        // TODO: Refine code

        // completely ignore callee saved registers

        for (_, bb, iiter) in cur_func.body.mbb_iter() {
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
                if livein.is_virt_reg() {
                    vreg2range
                        .entry(livein.as_virt_reg())
                        .or_insert_with(|| LiveRange::new_empty())
                } else {
                    reg2range.get_or_create(livein.as_phys_reg().into())
                }
                .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()))
            }

            for (inst_id, inst) in iiter {
                id2pp.insert(inst_id, cur_pp!());

                for reg in inst.collect_used_regs() {
                    if reg.is_phys_reg() && !is_callee_saved_reg(reg.as_phys_reg()) {
                        let phys_reg = reg.as_phys_reg();
                        if let Some(range) = reg2range.get_mut(phys_reg) {
                            range.segments.last_mut().unwrap().end = cur_pp!();
                        }
                    } else if reg.is_virt_reg() {
                        vreg2range
                            .get_mut(&reg.as_virt_reg())
                            .unwrap()
                            .segments
                            .last_mut()
                            .unwrap()
                            .end = cur_pp!();
                    }
                }

                for def in inst.collect_defined_regs() {
                    if def.is_phys_reg() && !is_callee_saved_reg(def.as_phys_reg()) {
                        reg2range
                            .get_or_create(def.as_phys_reg())
                            .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()));
                    } else if def.is_virt_reg() {
                        vreg2entity.insert(def.as_virt_reg(), *def);
                        vreg2range
                            .entry(def.as_virt_reg())
                            .or_insert_with(|| LiveRange::new_empty())
                            .add_segment(LiveSegment::new(cur_pp!(), cur_pp!()));
                    }
                }

                index += IDX_STEP;
            }

            for liveout in &liveness.live_out {
                if liveout.is_virt_reg() {
                    vreg2range.get_mut(&liveout.as_virt_reg())
                } else {
                    reg2range.get_mut(liveout.as_phys_reg())
                }
                .unwrap()
                .segments
                .last_mut()
                .unwrap()
                .end = cur_pp!();
            }

            bb_idx += 1;
        }

        // println!("{:?}", vreg2range);

        LiveRegMatrix::new(
            vreg2entity,
            id2pp,
            VirtRegInterval(
                vreg2range
                    .into_iter()
                    .map(|(vreg, range)| (vreg, LiveInterval::new(vreg, range)))
                    .collect(),
            ),
            reg2range,
            program_points,
        )
    }
}

fn is_callee_saved_reg<T: TargetRegisterTrait>(r: T) -> bool {
    CALLEE_SAVED_REGS.with(|regs| regs.has(r.as_phys_reg()))
}
