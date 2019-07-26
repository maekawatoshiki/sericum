use super::{function::*, instr::*, module::*};
use rustc_hash::FxHashMap;

pub struct PhysicalRegisterAllocator<'a> {
    pub module: &'a MachineModule,
}

impl<'a> PhysicalRegisterAllocator<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self { module }
    }

    pub fn run_on_module(&mut self) {
        for (_, func) in &self.module.functions {
            self.run_on_function(func);
        }
    }

    pub fn run_on_function(&mut self, cur_func: &MachineFunction) {
        self.collect_regs(cur_func);
        self.scan(cur_func);
    }

    fn scan(&mut self, cur_func: &MachineFunction) {
        let mut used = FxHashMap::default();
        for (_, bb) in &cur_func.basic_blocks {
            for instr_id in &*bb.iseq_ref() {
                self.scan_on_instr(cur_func, &mut used, *instr_id);
            }
        }
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
