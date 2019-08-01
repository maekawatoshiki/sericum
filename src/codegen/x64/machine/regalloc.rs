use super::{function::*, instr::*, module::*};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct PhysicalRegisterAllocator {}

impl PhysicalRegisterAllocator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(func);
        }
    }

    pub fn run_on_function(&mut self, cur_func: &mut MachineFunction) {
        self.collect_regs(cur_func);
        self.scan(cur_func);
    }

    fn scan(&mut self, cur_func: &mut MachineFunction) {
        let mut call_instr_pos = vec![];
        let mut used = FxHashMap::default();
        let mut local4spill: Vec<FrameIndexInfo> = vec![];
        let mut idx = cur_func.locals_ty.len() as i32;

        let mut spill =
            |occupied: &mut FxHashSet</*idx*/ i32>, r: &MachineRegister| -> FrameIndexInfo {
                for fii in &local4spill {
                    if occupied.contains(&fii.idx) {
                        continue;
                    }
                    if r.info_ref().ty == fii.ty {
                        occupied.insert(fii.idx);
                        return fii.clone();
                    }
                }
                idx += 1i32;
                occupied.insert(idx);
                let fii = FrameIndexInfo::new(r.info_ref().ty.clone(), idx);
                local4spill.push(fii.clone());
                fii
            };

        for (bb_id, bb) in &cur_func.basic_blocks {
            for (i, instr_id) in bb.iseq_ref().iter().enumerate() {
                self.scan_on_instr(cur_func, &mut used, *instr_id);

                let instr = &cur_func.instr_arena[*instr_id];
                if instr.opcode == MachineOpcode::Call {
                    call_instr_pos.push((bb_id, i));
                }
            }
        }

        for (bb_id, mut instr_pos) in &call_instr_pos {
            let iseq = &mut cur_func.basic_blocks[*bb_id].iseq_ref_mut();
            let call_instr_vreg = cur_func.instr_arena[iseq[instr_pos]].get_vreg();
            let mut regs_to_save = vec![];

            for (_, i) in &cur_func.instr_arena {
                // TODO
                if i.reg.borrow().reg.is_none() {
                    continue;
                }
                let bgn = i.get_vreg();
                let end = match i.get_last_use() {
                    Some(last_use) => cur_func.instr_arena[last_use].get_vreg(),
                    None => continue,
                };
                if bgn < call_instr_vreg && call_instr_vreg < end {
                    regs_to_save.push(MachineRegister::new(i.reg.clone()));
                }
            }

            println!("SAVE REG: {:?}", regs_to_save);
            let mut occupied = FxHashSet::default();
            let mut fiis = vec![];
            for r in &regs_to_save {
                fiis.push(spill(&mut occupied, r));
            }
            println!("FII: {:?}", fiis);

            for (fii, reg) in fiis.iter().zip(regs_to_save) {
                let store_instr_id = cur_func.instr_arena.alloc(MachineInstr::new(
                    MachineOpcode::Store,
                    vec![
                        MachineOperand::FrameIndex(fii.clone()),
                        MachineOperand::Register(reg.clone()),
                    ],
                    None,
                ));
                let load_instr_id = cur_func.instr_arena.alloc(MachineInstr {
                    opcode: MachineOpcode::Load,
                    operand: vec![MachineOperand::FrameIndex(fii.clone())],
                    ty: Some(reg.info_ref().ty.clone()),
                    reg: reg.info.clone(),
                });
                iseq.insert(instr_pos, store_instr_id);
                iseq.insert(instr_pos + 2, load_instr_id);
                instr_pos += 1; // +1 for store
            }
        }

        cur_func
            .locals_ty
            .append(&mut local4spill.iter().map(|fii| fii.ty.clone()).collect());
    }

    fn scan_on_instr(
        &mut self,
        cur_func: &MachineFunction,
        used: &mut FxHashMap<usize, MachineInstrId>,
        instr_id: MachineInstrId,
    ) {
        // TODO: Refactor

        let instr = &cur_func.instr_arena[instr_id];
        let num_reg = 4;

        if instr.reg.borrow().last_use.is_none() {
            return;
        }

        let mut found = false;

        for i in 0..num_reg - 1 {
            if used.contains_key(&i) {
                let target_last_use_id = cur_func.instr_arena[*used.get(&i).unwrap()]
                    .get_last_use()
                    .unwrap();
                let target_last_use = cur_func.instr_arena[target_last_use_id].get_vreg();
                if instr.get_vreg() < target_last_use {
                    continue;
                }
            }

            instr.set_phy_reg(i, false);
            used.insert(i, instr_id);
            found = true;
            break;
        }

        if found {
            return;
        }

        used.insert(num_reg - 1, instr_id);

        let mut k = 0;
        for i in 1..num_reg {
            let l1 = cur_func.instr_arena[*used.get(&k).unwrap()]
                .get_last_use()
                .unwrap();
            let l2 = cur_func.instr_arena[*used.get(&i).unwrap()]
                .get_last_use()
                .unwrap();
            if l1 < l2 {
                k = i;
            }
        }

        instr.set_phy_reg(k, false);
        cur_func.instr_arena[*used.get(&k).unwrap()].set_phy_reg(num_reg - 1, true);

        *used.get_mut(&k).unwrap() = instr_id;
    }

    fn collect_regs(&mut self, cur_func: &MachineFunction) {
        for (_, bb) in &cur_func.basic_blocks {
            let mut last_instr = None;

            for instr_id in &*bb.iseq_ref() {
                self.collect_regs_on_instr(cur_func, *instr_id);
                last_instr = Some(*instr_id);
            }

            for out in &bb.liveness.borrow().live_out {
                out.set_last_use(last_instr);
            }
        }
    }

    fn collect_regs_on_instr(&mut self, cur_func: &MachineFunction, instr_id: MachineInstrId) {
        let instr = &cur_func.instr_arena[instr_id];
        for operand in &instr.operand {
            match_then!(
                MachineOperand::Register(reg),
                operand,
                reg.set_last_use(Some(instr_id))
            );
        }
    }
}
