use super::liveness::*;
use super::{function::*, /*liveness::*,*/ module::*};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub fn coalesce_function(matrix: &mut LiveRegMatrix, f: &mut MachineFunction) {
    let mut removal_list = FxHashSet::default();
    let mut worklist = VecDeque::new();
    for (_, bb) in f.body.basic_blocks.id_and_block() {
        for &id in &*bb.iseq_ref() {
            let inst = f.body.inst_arena[id].clone();

            if inst.opcode.is_copy_like() && inst.operand[0].is_register() {
                worklist.push_back(id);
            }
        }
    }

    while let Some(copy_id) = worklist.pop_front() {
        let copy = f.body.inst_arena[copy_id].clone();

        debug!(println!(
            "dst:{:?}, src:{:?}",
            copy.def[0],
            copy.operand[0].as_register()
        ));

        let copy_dst = &copy.def[0];
        let copy_src = copy.operand[0].as_register();

        if copy_dst.is_phys_reg()
            && copy_src.is_phys_reg()
            && copy_dst.get_reg().unwrap() == copy_src.get_reg().unwrap()
        {
            removal_list.insert(copy_id);
            continue;
        }

        if copy_dst.is_vreg() && copy_src.is_vreg() && copy_dst.get_vreg() == copy_src.get_vreg() {
            if copy_src.info_ref().def_list.len() == 0 {
                let bb = &f.body.basic_blocks.arena[copy.parent];
                bb.liveness_ref_mut().def.remove(&copy_src.get_vreg());
            }
            removal_list.insert(copy_id);
            continue;
        }

        // p_dst = Copy v_src
        if copy_dst.is_phys_reg() && copy_src.is_vreg() {
            let can_eliminate_copy =
                !matrix.interferes(copy_src.get_vreg(), copy_dst.get_reg().unwrap()) && {
                    let bb = &f.body.basic_blocks.arena[copy.parent];
                    let l = &bb.liveness_ref();
                    !l.live_in.contains(&copy_src.get_vreg())
                        && !l.live_out.contains(&copy_src.get_vreg())
                };
            if !can_eliminate_copy {
                continue;
            }

            let def_list = copy_src.info_ref().def_list.clone();
            let use_list = copy_src.info_ref().use_list.clone();
            for &def in &def_list {
                f.body.inst_arena[def].set_def(copy_dst.clone());
            }
            for use_ in use_list {
                f.body.inst_arena[use_].replace_operand_register(copy_src, copy_dst);
            }

            if copy_src.info_ref().def_list.len() == 0 {
                let bb = &f.body.basic_blocks.arena[copy.parent];
                bb.liveness_ref_mut().def.remove(&copy_src.get_vreg());
            }
            matrix.merge_regs(copy_dst.get_reg().unwrap(), copy_src.get_vreg());

            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy p_src
        if copy_dst.is_vreg() && copy_src.is_phys_reg() {
            let can_eliminate_copy =
                !matrix.interferes(copy_dst.get_vreg(), copy_src.get_reg().unwrap()) && {
                    let bb = &f.body.basic_blocks.arena[copy.parent];
                    let l = &bb.liveness_ref();
                    !l.live_out.contains(&copy_dst.get_vreg())
                };
            if !can_eliminate_copy {
                continue;
            }

            let def_list = copy_dst.info_ref().def_list.clone();
            let use_list = copy_dst.info_ref().use_list.clone();
            for &def in &def_list {
                f.body.inst_arena[def].set_def(copy_src.clone());
            }
            for use_ in use_list {
                f.body.inst_arena[use_].replace_operand_register(copy_dst, copy_src);
            }

            if copy_dst.info_ref().def_list.len() == 0 {
                let bb = &f.body.basic_blocks.arena[copy.parent];
                bb.liveness_ref_mut().def.remove(&copy_dst.get_vreg());
            }
            matrix.merge_regs(copy_src.get_reg().unwrap(), copy_dst.get_vreg());

            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy v_src
        if copy_dst.is_vreg() && copy_src.is_vreg() {
            let can_eliminate_copy =
                !matrix.interferes_virt_regs(copy_dst.get_vreg(), copy_src.get_vreg()) && {
                    let bb = &f.body.basic_blocks.arena[copy.parent];
                    let l = &bb.liveness_ref();
                    !l.live_in.contains(&copy_dst.get_vreg())
                        && !l.live_out.contains(&copy_dst.get_vreg())
                        && !l.live_in.contains(&copy_src.get_vreg())
                        && !l.live_out.contains(&copy_src.get_vreg())
                };
            if !can_eliminate_copy {
                continue;
            }

            let def_list = copy_dst.info_ref().def_list.clone();
            let use_list = copy_dst.info_ref().use_list.clone();
            for &def in &def_list {
                f.body.inst_arena[def].set_def(copy_src.clone());
            }
            for use_ in use_list {
                f.body.inst_arena[use_].replace_operand_register(copy_dst, copy_src);
            }

            if copy_dst.info_ref().def_list.len() == 0 {
                let bb = &f.body.basic_blocks.arena[copy.parent];
                bb.liveness_ref_mut().def.remove(&copy_dst.get_vreg());
            }
            matrix.merge_virt_regs(copy_src.get_vreg(), copy_dst.get_vreg());

            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }
    }

    for id in removal_list {
        f.remove_inst(id)
    }
}

pub struct RegisterCoalescer {}

impl RegisterCoalescer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, _f: &mut MachineFunction) {
        // let mut matrix = LivenessAnalysis::new().analyze_function(f);
    }
}
