use super::super::machine::register::RegisterId;
use crate::codegen::common::machine::{
    basic_block::*, function::*, inst::RegisterOperand, liveness::*,
};
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub fn coalesce_function(matrix: &mut LiveRegMatrix, f: &mut MachineFunction) {
    let mut worklist = VecDeque::new();
    for (_, _, iiter) in f.body.mbb_iter() {
        for (id, inst) in iiter {
            if inst.opcode.is_copy_like() && inst.operand[0].is_register() {
                worklist.push_back(id);
            }
        }
    }

    fn no_tied_use(f: &MachineFunction, r: &RegisterId) -> bool {
        f.regs_info.arena_ref()[*r].uses.iter().all(|u| {
            let inst = &f.body.inst_arena[*u];
            match inst
                .operand
                .iter()
                .position(|o| o.is_register() && o.as_register().id == *r)
            {
                Some(pos) => f.body.inst_arena[*u].opcode.inst_def().map_or(true, |i| {
                    assert!(i.tie.len() <= 1); // TODO: support for more than one tied register
                    i.tie.len() == 0 || i.tie.iter().next().unwrap().1.as_use() != pos
                }),
                None => true,
            }
        })
    }

    let mut removal_list = FxHashSet::default();
    while let Some(copy_id) = worklist.pop_front() {
        let copy = f.body.inst_arena[copy_id].clone();

        let copy_dst = copy.def[0];
        let copy_src = *copy.operand[0].as_register();

        // Remove p_A = Copy p_A
        if copy_dst.id.is_phys_reg()
            && copy_src.id.is_phys_reg()
            && copy_dst.converted_id().as_phys_reg() == copy_src.converted_id().as_phys_reg()
        {
            removal_list.insert(copy_id);
            continue;
        }

        // Remove v_A = Copy v_A
        if copy_dst.id.is_virt_reg()
            && copy_src.id.is_virt_reg()
            && copy_dst.id.as_virt_reg() == copy_src.id.as_virt_reg()
        {
            removal_list.insert(copy_id);
            continue;
        }

        // p_dst = Copy v_src. v_src uses & defs may be replaced with p_dst
        // TODO: THE FOLLOWING CODE DOESN'T WORK
        if copy_dst.id.is_phys_reg()
            && copy_src.id.is_virt_reg()
            && copy_dst.sub_super == copy_src.sub_super
        {
            let can_eliminate_copy =
                !matrix.interferes(copy_src.id.as_virt_reg(), copy_dst.id.as_phys_reg()) && {
                    let l = &f.body.basic_blocks.liveness[&copy.parent];
                    !l.live_in.contains(&copy_src.id) && !l.live_out.contains(&copy_src.id)
                };
            if !can_eliminate_copy {
                continue;
            }

            replace_regs(f, copy.parent, copy_src, copy_dst);

            matrix.merge_regs(copy_dst.id.as_phys_reg(), copy_src.id.as_virt_reg());
            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy p_src. v_dst uses & defs may be replaced with p_src
        if copy_dst.id.is_virt_reg()
            && copy_src.id.is_phys_reg()
            && copy_dst.sub_super == copy_src.sub_super
        {
            let can_eliminate_copy = no_tied_use(f, &copy_dst.id)
                && !matrix.interferes(copy_dst.id.as_virt_reg(), copy_src.id.as_phys_reg())
                && {
                    let l = &f.body.basic_blocks.liveness[&copy.parent];
                    !l.live_in.contains(&copy_dst.id) && !l.live_out.contains(&copy_dst.id)
                };
            if !can_eliminate_copy {
                continue;
            }

            replace_regs(f, copy.parent, copy_dst, copy_src);

            matrix.merge_regs(copy_src.id.as_phys_reg(), copy_dst.id.as_virt_reg());
            removal_list.insert(copy_id);
            worklist.push_back(copy_id);
            continue;
        }

        // v_dst = Copy v_src. v_dst uses & defs may be replaced with v_src
        if copy_dst.id.is_virt_reg()
            && copy_src.id.is_virt_reg()
            && copy_dst.sub_super == copy_src.sub_super
        {
            let can_eliminate_copy = f.regs_info.arena_ref()[copy_dst.id].defs.len() == 1
                && !matrix
                    .interferes_virt_regs(copy_dst.id.as_virt_reg(), copy_src.id.as_virt_reg())
                && no_tied_use(f, &copy_dst.id)
                && no_tied_use(f, &copy_src.id)
                && {
                    let l = &f.body.basic_blocks.liveness[&copy.parent];
                    !l.live_in.contains(&copy_dst.id)
                        && !l.live_out.contains(&copy_dst.id)
                        && !l.live_in.contains(&copy_src.id)
                        && !l.live_out.contains(&copy_src.id)
                };
            if !can_eliminate_copy {
                continue;
            }

            replace_regs(f, copy.parent, copy_dst, copy_src);
            matrix.merge_virt_regs(
                &f.regs_info,
                copy_src.id.as_virt_reg(),
                copy_dst.id.as_virt_reg(),
            );

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
    from: RegisterOperand,
    to: RegisterOperand,
) {
    let defs = f.regs_info.arena_ref()[from.id].defs.clone();
    let uses = f.regs_info.arena_ref()[from.id].uses.clone();

    for &def in &defs {
        f.body.inst_arena[def].set_def(&f.regs_info, to);
    }
    for use_ in uses {
        f.body.inst_arena[use_].replace_operand_register(&f.regs_info, from.id, to.id);
    }
    if f.regs_info.arena_ref()[from.id].defs.len() == 0 {
        f.body
            .basic_blocks
            .liveness
            .get_mut(&parent)
            .unwrap()
            .def
            .remove(&from.id);
    }
}
