use super::super::exec::roundup;
use super::super::{frame_object::*, register::*};
use super::{builder::*, function::MachineFunction, inst::*, module::MachineModule};
use crate::{ir::types::*, traits::pass::ModulePassTrait};
use std::cmp;

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

struct CopyArgs<'a> {
    offset: i32,
    builder: &'a mut Builder<'a>,
    params_ty: &'a Vec<Type>,
}

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
        if cur_func.is_internal {
            return;
        }

        let frame_info = FrameObjectsInfo::new(tys, cur_func);
        let down = self.calc_max_adjust_stack_down(cur_func);
        let adjust = roundup(frame_info.total_size() + down, 16);
        self.insert_prologue(tys, cur_func, adjust);
        self.insert_epilogue(cur_func, adjust);
    }

    fn calc_max_adjust_stack_down(&mut self, cur_func: &mut MachineFunction) -> i32 {
        let mut down = 0;
        let mut removal_list = vec![];

        for (_, _, iiter) in cur_func.body.mbb_iter() {
            for (id, inst) in iiter {
                match inst.opcode {
                    MachineOpcode::AdjStackDown => {
                        let d = inst.operand[0].as_constant().as_i32();
                        down = cmp::max(d, down);
                        removal_list.push(id)
                    }
                    MachineOpcode::AdjStackUp => removal_list.push(id),
                    _ => continue,
                }
            }
        }

        for id in removal_list {
            cur_func.remove_inst(id)
        }

        down
    }

    fn insert_prologue(&mut self, tys: &Types, cur_func: &mut MachineFunction, adjust: i32) {
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_bb();

        //addi sp,sp,-32
        let sp = builder.function.regs_info.get_phys_reg(GPR::SP);
        let addi = MachineInst::new_simple(
            MachineOpcode::ADDI,
            vec![
                MachineOperand::Register(sp),
                MachineOperand::Constant(MachineConstant::Int32(-adjust)),
            ],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![sp]);
        builder.insert(addi);

        let s0 = builder.function.regs_info.get_phys_reg(GPR::S0);
        let sd = MachineInst::new_simple(
            MachineOpcode::SD,
            vec![
                MachineOperand::Register(s0),
                MachineOperand::Mem(MachineMemOperand::ImmReg(adjust - 8, sp)),
            ],
            builder.get_cur_bb().unwrap(),
        );
        builder.insert(sd);

        let addi = MachineInst::new_simple(
            MachineOpcode::ADDI,
            vec![
                MachineOperand::Register(sp),
                MachineOperand::Constant(MachineConstant::Int32(adjust)),
            ],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![s0]);
        builder.insert(addi);

        // self.insert_arg_copy(&tys.base.borrow(), &mut builder);
    }
    //
    // fn insert_arg_copy<'a>(&mut self, tys: &'a TypesBase, builder: &'a mut Builder<'a>) {
    //     CopyArgs::new(
    //         builder,
    //         &tys.as_function_ty(builder.function.ty).unwrap().params_ty,
    //     )
    //     .copy();
    // }
    //
    fn insert_epilogue(&mut self, cur_func: &mut MachineFunction, adjust: i32) {
        let mut bb_iseq = vec![];
        // let has_call = cur_func.body.has_call();

        for (bb_id, bb) in cur_func.body.basic_blocks.id_and_block() {
            let last_inst_id = *bb.iseq_ref().last().unwrap();
            let last_inst = &cur_func.body.inst_arena[last_inst_id];
            let is_return =
                last_inst.opcode == MachineOpcode::JR && last_inst.operand[0].is_register() && {
                    let r = last_inst.operand[0].as_register();
                    r.is_phys_reg() && r.as_phys_reg() == GPR::RA.as_phys_reg()
                };

            if !is_return {
                continue;
            }

            let mut iseq = vec![];

            let s0 = cur_func.regs_info.get_phys_reg(GPR::S0);
            let sp = cur_func.regs_info.get_phys_reg(GPR::SP);
            let ld = MachineInst::new_simple(
                MachineOpcode::LD,
                vec![MachineOperand::Mem(MachineMemOperand::ImmReg(
                    adjust - 8,
                    sp,
                ))],
                bb_id,
            )
            .with_def(vec![s0]);
            iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, ld));

            let addi = MachineInst::new_simple(
                MachineOpcode::ADDI,
                vec![
                    MachineOperand::Register(sp),
                    MachineOperand::Constant(MachineConstant::Int32(adjust)),
                ],
                bb_id,
            )
            .with_def(vec![sp]);
            iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, addi));

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
