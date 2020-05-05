use super::{basic_block::*, function::*, inst::*, liveness::*};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub fn coalesce_function(matrix: &mut LiveRegMatrix, f: &mut MachineFunction) {
    let mut removal_list = FxHashSet::default();
    let mut worklist = VecDeque::new();
    for (_, _, iiter) in f.body.mbb_iter() {
        for (id, inst) in iiter {
            if inst.opcode.is_copy_like() && inst.operand[0].is_register() {
                worklist.push_back(id);
            }
        }
    }

    while let Some(copy_id) = worklist.pop_front() {
        let copy = f.body.inst_arena[copy_id].clone();

        // debug!(println!(
        //     "dst:{:?}, src:{:?}",
        //     copy.def[0],
        //     copy.operand[0].as_register()
        // ));
        let copy_dst = &copy.def[0];
        let copy_src = copy.operand[0].as_register();

        // Remove p_A = Copy p_A
        if copy_dst.is_phys_reg()
            && copy_src.is_phys_reg()
            && copy_dst.get_reg().unwrap() == copy_src.get_reg().unwrap()
        {
            removal_list.insert(copy_id);
            continue;
        }

        // Remove v_A = Copy v_A
        if copy_dst.is_vreg() && copy_src.is_vreg() && copy_dst.get_vreg() == copy_src.get_vreg() {
            removal_list.insert(copy_id);
            continue;
        }

        // p_dst = Copy v_src. v_src uses & defs may be replaced with p_dst
        // TODO: SOMEHOW THE FOLLOWING CODE DOESN'T WORK
        // if copy_dst.is_phys_reg() && copy_src.is_vreg() {
        //     println!("{:?} {:?}", copy_src, copy_src.info_ref().defs);
        // 
        //     let can_eliminate_copy =
        //         !matrix.interferes(copy_src.get_vreg(), copy_dst.get_reg().unwrap()) && {
        //             let l = &f.body.basic_blocks.arena[copy.parent].liveness_ref();
        //             !l.live_in.contains(&copy_src.get_vreg())
        //                 && !l.live_out.contains(&copy_src.get_vreg())
        //         };
        //     if !can_eliminate_copy {
        //         continue;
        //     }
        // 
        //     replace_regs(f, copy.parent, copy_src, copy_dst);
        // 
        //     matrix.merge_regs(copy_dst.get_reg().unwrap(), copy_src.get_vreg());
        //     removal_list.insert(copy_id);
        //     worklist.push_back(copy_id);
        //     continue;
        // }

        // v_dst = Copy p_src. v_dst uses & defs may be replaced with p_src
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

            replace_regs(f, copy.parent, copy_dst, copy_src);

            matrix.merge_regs(copy_src.get_reg().unwrap(), copy_dst.get_vreg());
            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy v_src. v_dst uses & defs may be replaced with v_src
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

            replace_regs(f, copy.parent, copy_dst, copy_src);

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

fn replace_regs(
    f: &mut MachineFunction,
    parent: MachineBasicBlockId,
    from: &MachineRegister,
    to: &MachineRegister,
) {
    let defs = from.info_ref().defs.clone();
    let uses = from.info_ref().uses.clone();

    for &def in &defs {
        f.body.inst_arena[def].set_def(to.clone());
    }
    for use_ in uses {
        f.body.inst_arena[use_].replace_operand_register(from, to);
    }

    if from.info_ref().defs.len() == 0 {
        let bb = &f.body.basic_blocks.arena[parent];
        bb.liveness_ref_mut().def.remove(&from.get_vreg());
    }
}
