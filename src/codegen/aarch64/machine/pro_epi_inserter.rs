use super::super::{frame_object::*, machine::register::*};
use super::inst::*;
use crate::codegen::common::machine::{builder::*, function::*, module::MachineModule};
use crate::{ir::types::*, traits::pass::ModulePassTrait};

pub struct PrologueEpilogueInserter {}

impl ModulePassTrait for PrologueEpilogueInserter {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "PrologueEpilogueInserter"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

// struct CopyArgs<'a> {
//     offset: i32,
//     builder: &'a mut Builder<'a>,
//     params_ty: &'a Vec<Type>,
// }

impl PrologueEpilogueInserter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }
            self.run_on_function(&module.types, func);
        }
    }

    pub fn run_on_function(&mut self, tys: &Types, cur_func: &mut MachineFunction) {
        let frame_objects = FrameObjectsInfo::new(tys, cur_func);
        let saved_regs = cur_func
            .body
            .appeared_phys_regs()
            .containing_callee_saved_regs()
            .to_phys_set()
            .into_iter()
            .collect::<Vec<_>>();
        if cur_func.body.has_call() {
            // saved_regs.push(GPR::RA.as_phys_reg())
        }
        Self::remove_adjust_stack_inst(cur_func);
        self.insert_prologue(cur_func, &saved_regs, &frame_objects);
        self.insert_epilogue(cur_func, &saved_regs, &frame_objects);
        cur_func.frame_objects = Some(frame_objects);
    }

    fn remove_adjust_stack_inst(cur_func: &mut MachineFunction) {
        let mut removal_list = vec![];
        for (_, _, iiter) in cur_func.body.mbb_iter() {
            for (id, inst) in iiter {
                if matches!(
                    inst.opcode,
                    MachineOpcode::AdjStackDown | MachineOpcode::AdjStackUp
                ) {
                    removal_list.push(id)
                }
            }
        }
        for id in removal_list {
            cur_func.remove_inst(id)
        }
    }

    fn insert_prologue(
        &mut self,
        /* tys: &Types, */ cur_func: &mut MachineFunction,
        saved_regs: &[PhysReg],
        frame_objects: &FrameObjectsInfo,
    ) {
        let adjust = frame_objects.total_size();
        let callee_saved_regs_adjust = frame_objects.aligned_callee_saved_regs_byte();
        // [0,504], [505, 4096], [4096, oo)
        let stackdown1 = adjust <= 504;
        let stackdown2 = 504 < adjust && bits_within(adjust, 12);
        let stackdown3 = !bits_within(adjust, 12);
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_block();

        let sp = RegisterOperand::new(builder.function.regs_info.get_phys_reg(SP::SP));
        let x16 = RegisterOperand::new(builder.function.regs_info.get_phys_reg(GR64::X16));
        let x29 = RegisterOperand::new(builder.function.regs_info.get_phys_reg(GR64::X29));
        let x30 = RegisterOperand::new(builder.function.regs_info.get_phys_reg(GR64::X30));

        if stackdown1 {
            // stp x29, x30, [sp, -adjust]
            let stp = MachineInst::new_simple(
                MachineOpcode::STP,
                vec![
                    MachineOperand::Register(x29),
                    MachineOperand::Register(x30),
                    MachineOperand::Mem(MachineMemOperand::PreIndex(sp, -adjust)),
                ],
                builder.get_cur_bb().unwrap(),
            );
            builder.insert(stp);
        }

        let saved_regs = frame_objects.aligned_callee_saved_regs_byte();
        let mem_adjust = adjust - saved_regs;

        if stackdown2 {
            // sub sp, sp, mem_adjust
            let sub = MachineInst::new_simple(
                MachineOpcode::SUBrrr64,
                vec![
                    MachineOperand::Register(sp),
                    MachineOperand::imm_i32(mem_adjust),
                ],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![sp]);
            builder.insert(sub);
        }

        if stackdown3 {
            // mov x16, mem_adjust
            let mov = MachineInst::new_simple(
                MachineOpcode::MOVr64i,
                vec![MachineOperand::imm_i32(mem_adjust)],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![x16]);
            // sub sp, sp, x16
            let sub = MachineInst::new_simple(
                MachineOpcode::SUBrrr64,
                vec![MachineOperand::Register(sp), MachineOperand::Register(x16)],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![sp]);
            builder.insert(mov);
            builder.insert(sub);
        }

        if stackdown2 || stackdown3 {
            // stp x29, x30, [sp -saved_regs]!
            let stp = MachineInst::new_simple(
                MachineOpcode::STP,
                vec![
                    MachineOperand::Register(x29),
                    MachineOperand::Register(x30),
                    MachineOperand::Mem(MachineMemOperand::PreIndex(sp, -saved_regs)),
                ],
                builder.get_cur_bb().unwrap(),
            );
            builder.insert(stp);
        }

        // mov x29, sp
        let mov = MachineInst::new_simple(
            MachineOpcode::MOVrr,
            vec![MachineOperand::Register(sp)],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![x29]);
        builder.insert(mov);

        // self.insert_arg_copy(&tys.base.borrow(), &mut builder);
    }

    // fn insert_arg_copy<'a>(&mut self, tys: &'a TypesBase, builder: &'a mut Builder<'a>) {
    //     CopyArgs::new(
    //         builder,
    //         &tys.as_function_ty(builder.function.ty).unwrap().params_ty,
    //     )
    //     .copy();
    // }
    //
    fn insert_epilogue(
        &mut self,
        cur_func: &mut MachineFunction,
        saved_regs: &[PhysReg],
        frame_objects: &FrameObjectsInfo,
    ) {
        // ldp x29, x30, [sp], adjust
        let adjust = frame_objects.total_size();
        let callee_saved_regs_adjust = frame_objects.aligned_callee_saved_regs_byte();
        // [0,504], [505, 4096], [4096, oo)
        let stackdown1 = adjust <= 504;
        let stackdown2 = 504 < adjust && bits_within(adjust, 12);
        let stackdown3 = !bits_within(adjust, 12);

        let mut bb_iseq = vec![];
        // let has_call = cur_func.body.has_call();
        let sp = RegisterOperand::new(cur_func.regs_info.get_phys_reg(SP::SP));
        let x16 = RegisterOperand::new(cur_func.regs_info.get_phys_reg(GR64::X16));
        let x29 = RegisterOperand::new(cur_func.regs_info.get_phys_reg(GR64::X29));
        let x30 = RegisterOperand::new(cur_func.regs_info.get_phys_reg(GR64::X30));

        for (bb_id, bb) in cur_func.body.basic_blocks.id_and_block() {
            let last_inst_id = *bb.iseq_ref().last().unwrap();
            let last_inst = &cur_func.body.inst_arena[last_inst_id];
            let is_return = last_inst.opcode == MachineOpcode::RET;

            if !is_return {
                continue;
            }

            if stackdown1 {
                // ldp x29, x30, [sp], adjust
                let ldp = MachineInst::new_simple(
                    MachineOpcode::LDP64,
                    vec![MachineOperand::Mem(MachineMemOperand::PostIndex(
                        sp, adjust,
                    ))],
                    bb_id,
                )
                .with_def(vec![x29, x30]);
                bb_iseq.push((
                    last_inst_id,
                    vec![cur_func.body.inst_arena.alloc(&cur_func.regs_info, ldp)],
                ));
                continue;
            }

            let saved_regs = frame_objects.aligned_callee_saved_regs_byte();
            let mem_adjust = adjust - saved_regs;
            let mut iseq = vec![];

            if stackdown2 {
                // add sp, sp, mem_adjust
                let add = MachineInst::new_simple(
                    MachineOpcode::ADDrrr64,
                    vec![
                        MachineOperand::Register(sp),
                        MachineOperand::imm_i32(mem_adjust),
                    ],
                    bb_id,
                )
                .with_def(vec![sp]);
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, add));
            }

            if stackdown3 {
                // ldp x29, x30, [sp], saved_regs
                let ldp = MachineInst::new_simple(
                    MachineOpcode::LDP64,
                    vec![MachineOperand::Mem(MachineMemOperand::PostIndex(
                        sp, saved_regs,
                    ))],
                    bb_id,
                )
                .with_def(vec![x29, x30]);
                // mov x16, mem_adjust
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVr64i,
                    vec![MachineOperand::imm_i32(mem_adjust)],
                    bb_id,
                )
                .with_def(vec![x16]);
                // add sp, sp, x16
                let add = MachineInst::new_simple(
                    MachineOpcode::ADDrrr64,
                    vec![MachineOperand::Register(sp), MachineOperand::Register(x16)],
                    bb_id,
                )
                .with_def(vec![sp]);
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, ldp));
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, mov));
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, add));
            }

            bb_iseq.push((last_inst_id, iseq));
        }

        for (ret_id, iseq) in bb_iseq {
            let mut builder = Builder::new(cur_func);
            builder.set_insert_point_before_inst(ret_id);
            for inst in iseq {
                builder.insert(inst)
            }
        }
    }
}

// impl<'a> CopyArgs<'a> {
//     pub fn new(builder: &'a mut Builder<'a>, params_ty: &'a Vec<Type>) -> Self {
//         Self {
//             builder,
//             params_ty,
//             offset: 16, // call + push rbp. TODO: this may vary if there're more pushes
//         }
//     }
//
//     pub fn copy(mut self) {
//         for (i, &ty) in self.params_ty.iter().enumerate() {
//             match ty {
//                 Type::Int32 => self.copy_int(ty, i, 32),
//                 Type::Int64 | Type::Pointer(_) => self.copy_int(ty, i, 64),
//                 Type::F64 => self.copy_f64(i),
//                 _ => unimplemented!(),
//             }
//         }
//     }
//
//     fn copy_f64(&mut self, i: usize) {
//         let ret_reg = XMM::XMM0.as_phys_reg();
//         let dst = FrameIndexInfo::new(Type::F64, FrameIndexKind::Arg(i));
//         let src = match RegisterClassKind::XMM.get_nth_arg_reg(i) {
//             Some(_arg_reg) => return, // MachineOperand::phys_reg(&self.builder.function.regs_info, arg_reg),
//             None => {
//                 let ax = self.builder.function.regs_info.get_phys_reg(ret_reg);
//                 let inst = MachineInst::new_simple(
//                     MachineOpcode::MOVSDrm,
//                     vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
//                         self.builder.function.regs_info.get_phys_reg(GR64::RBP),
//                         self.offset,
//                     ))],
//                     self.builder.get_cur_bb().unwrap(),
//                 )
//                 .with_def(vec![ax.clone()]);
//                 self.builder.insert(inst);
//                 self.offset += 8;
//                 MachineOperand::Register(ax)
//             }
//         };
//         let inst = MachineInst::new_simple(
//             MachineOpcode::MOVSDmr,
//             vec![
//                 MachineOperand::Mem(MachineMemOperand::BaseFi(
//                     self.builder.function.regs_info.get_phys_reg(GR64::RBP),
//                     dst,
//                 )),
//                 src,
//             ],
//             self.builder.get_cur_bb().unwrap(),
//         );
//         self.builder.insert(inst)
//     }
//
//     fn copy_int(&mut self, ty: Type, i: usize, bit: usize) {
//         let (ax, rc, movrm) = match bit {
//             32 => (
//                 GR32::EAX.as_phys_reg(),
//                 RegisterClassKind::GR32,
//                 MachineOpcode::MOVrm32,
//             ),
//             64 => (
//                 GR64::RAX.as_phys_reg(),
//                 RegisterClassKind::GR64,
//                 MachineOpcode::MOVrm64,
//             ),
//             _ => unimplemented!(),
//         };
//         let dst = FrameIndexInfo::new(ty, FrameIndexKind::Arg(i));
//         let src = match rc.get_nth_arg_reg(i) {
//             Some(_arg_reg) => return, // MachineOperand::phys_reg(&self.builder.function.regs_info, arg_reg),
//             None => {
//                 let ax = self.builder.function.regs_info.get_phys_reg(ax);
//                 let inst = MachineInst::new_simple(
//                     movrm,
//                     vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
//                         self.builder.function.regs_info.get_phys_reg(GR64::RBP),
//                         self.offset,
//                     ))],
//                     self.builder.get_cur_bb().unwrap(),
//                 )
//                 .with_def(vec![ax]);
//                 self.builder.insert(inst);
//                 self.offset += 8;
//                 MachineOperand::Register(ax)
//             }
//         };
//         let inst = MachineInst::new_simple(
//             mov_mx(&self.builder.function.regs_info, &src).unwrap(),
//             vec![
//                 MachineOperand::Mem(MachineMemOperand::BaseFi(
//                     self.builder.function.regs_info.get_phys_reg(GR64::RBP),
//                     dst,
//                 )),
//                 src,
//             ],
//             self.builder.get_cur_bb().unwrap(),
//         );
//         self.builder.insert(inst)
//     }
// }

fn bits_within(x: i32, y: u32) -> bool {
    (x << (32 - y)) >> (32 - y) == x
}
