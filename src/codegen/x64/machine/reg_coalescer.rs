use super::super::register::RegisterId;
use super::{basic_block::*, function::*, liveness::*};
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
        let copy_dst = copy.def[0];
        let copy_src = *copy.operand[0].as_register();

        // Remove p_A = Copy p_A
        if copy_dst.is_phys_reg()
            && copy_src.is_phys_reg()
            && copy_dst.as_phys_reg() == copy_src.as_phys_reg()
        {
            removal_list.insert(copy_id);
            continue;
        }

        // Remove v_A = Copy v_A
        if copy_dst.is_virt_reg()
            && copy_src.is_virt_reg()
            && copy_dst.as_virt_reg() == copy_src.as_virt_reg()
        {
            removal_list.insert(copy_id);
            continue;
        }

        // p_dst = Copy v_src. v_src uses & defs may be replaced with p_dst
        // TODO: SOMEHOW THE FOLLOWING CODE DOESN'T WORK
        // if copy_dst.is_phys_reg() && copy_src.is_virt_reg() {
        //     println!(
        //         "{:?} {:?}",
        //         copy_dst,
        //         f.regs_info.arena_ref()[copy_dst].defs
        //     );
        //
        //     let can_eliminate_copy =
        //         !matrix.interferes(copy_src.as_virt_reg(), copy_dst.as_phys_reg()) && {
        //             let l = &f.body.basic_blocks.arena[copy.parent].liveness_ref();
        //             !l.live_in.contains(&copy_src.as_virt_reg())
        //                 && !l.live_out.contains(&copy_src.as_virt_reg())
        //         };
        //     if !can_eliminate_copy {
        //         continue;
        //     }
        //
        //     replace_regs(f, copy.parent, copy_src, copy_dst);
        //
        //     matrix.merge_regs(copy_dst.as_phys_reg(), copy_src.as_virt_reg());
        //     removal_list.insert(copy_id);
        //     worklist.push_back(copy_id);
        //     continue;
        // }

        // v_dst = Copy p_src. v_dst uses & defs may be replaced with p_src
        if copy_dst.is_virt_reg() && copy_src.is_phys_reg() {
            let can_eliminate_copy =
                !matrix.interferes(copy_dst.as_virt_reg(), copy_src.as_phys_reg()) && {
                    let bb = &f.body.basic_blocks.arena[copy.parent];
                    let l = &bb.liveness_ref();
                    !l.live_out.contains(&copy_dst.as_virt_reg())
                };
            if !can_eliminate_copy {
                continue;
            }

            replace_regs(f, copy.parent, copy_dst, copy_src);

            matrix.merge_regs(copy_src.as_phys_reg(), copy_dst.as_virt_reg());
            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy v_src. v_dst uses & defs may be replaced with v_src
        if copy_dst.is_virt_reg() && copy_src.is_virt_reg() {
            let can_eliminate_copy =
                !matrix.interferes_virt_regs(copy_dst.as_virt_reg(), copy_src.as_virt_reg()) && {
                    let bb = &f.body.basic_blocks.arena[copy.parent];
                    let l = &bb.liveness_ref();
                    !l.live_in.contains(&copy_dst.as_virt_reg())
                        && !l.live_out.contains(&copy_dst.as_virt_reg())
                        && !l.live_in.contains(&copy_src.as_virt_reg())
                        && !l.live_out.contains(&copy_src.as_virt_reg())
                };
            if !can_eliminate_copy {
                continue;
            }

            replace_regs(f, copy.parent, copy_dst, copy_src);

            matrix.merge_virt_regs(&f.regs_info, copy_src.as_virt_reg(), copy_dst.as_virt_reg());
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
    from: RegisterId,
    to: RegisterId,
) {
    let defs = f.regs_info.arena_ref()[from].defs.clone();
    let uses = f.regs_info.arena_ref()[from].uses.clone();

    for &def in &defs {
        f.body.inst_arena[def].set_def(&f.regs_info, to.clone());
    }
    for use_ in uses {
        f.body.inst_arena[use_].replace_operand_register(&f.regs_info, from, to);
    }

    if f.regs_info.arena_ref()[from].defs.len() == 0 {
        let bb = &f.body.basic_blocks.arena[parent];
        bb.liveness_ref_mut()
            .def
            .remove(&f.regs_info.arena_ref()[from].virt_reg);
    }
}
