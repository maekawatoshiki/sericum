use super::super::{dag::mc_convert::mov_mx, frame_object::*, register::*};
use super::{builder::*, function::MachineFunction, inst::*, module::MachineModule};
use crate::codegen::x64::exec::roundup;
use crate::ir::types::*;
use std::cmp;

pub struct PrologueEpilogueInserter {}

impl PrologueEpilogueInserter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(&module.types, func);
        }
    }

    pub fn run_on_function(&mut self, tys: &Types, cur_func: &mut MachineFunction) {
        if cur_func.internal {
            return;
        }

        let frame_info = FrameObjectsInfo::new(tys, cur_func);
        let down = self.calc_max_adjust_stack_down(cur_func);
        self.insert_prologue(tys, cur_func, &frame_info, down);
        self.insert_epilogue(cur_func);
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

    fn insert_prologue(
        &mut self,
        tys: &Types,
        cur_func: &mut MachineFunction,
        finfo: &FrameObjectsInfo,
        down: i32,
    ) {
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_bb();

        // push rbp
        let push_rbp = MachineInst::new_simple(
            MachineOpcode::PUSH64,
            vec![MachineOperand::phys_reg(GR64::RBP)],
            builder.get_cur_bb().unwrap(),
        );
        let push_rbp = builder.function.body.inst_arena.alloc(push_rbp);
        builder.insert(push_rbp);

        // mov rbp, rsp
        let mov_rbp_rsp = MachineInst::new_simple(
            MachineOpcode::MOVrr64,
            vec![MachineOperand::phys_reg(GR64::RSP)],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![MachineRegister::phys_reg(GR64::RBP)]);
        let mov_rbp_rsp = builder.function.body.inst_arena.alloc(mov_rbp_rsp);
        builder.insert(mov_rbp_rsp);

        // sub rsp, adjust
        let adjust = roundup(finfo.total_size() + /*push rbp=*/8 + down, 16) - /*push rbp=*/8;
        let sub_rsp = MachineInst::new_simple(
            MachineOpcode::SUBr64i32,
            vec![
                MachineOperand::phys_reg(GR64::RSP),
                MachineOperand::imm_i32(adjust),
            ],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![MachineRegister::phys_reg(GR64::RSP)]);
        let sub_rsp = builder.function.body.inst_arena.alloc(sub_rsp);
        builder.insert(sub_rsp);

        self.insert_arg_copy(tys, &mut builder);
    }

    fn insert_arg_copy<'a>(&mut self, tys: &Types, builder: &mut Builder<'a>) {
        let mut off = 16; // call + push rbp. TODO: this may vary if there're more pushes
        for (i, &ty) in tys
            .as_function_ty(builder.function.ty)
            .unwrap()
            .params_ty
            .iter()
            .enumerate()
        {
            match ty {
                Type::Int32 => {
                    let dst =
                        MachineOperand::FrameIndex(FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)));
                    let src = match RegisterClassKind::GR32.get_nth_arg_reg(i) {
                        Some(arg_reg) => MachineOperand::Register(
                            RegisterInfo::phys_reg(arg_reg).into_machine_register(),
                        ),
                        None => {
                            let eax = RegisterInfo::phys_reg(GR32::EAX).into_machine_register();
                            let inst = MachineInst::new_simple(
                                MachineOpcode::MOVrm32,
                                vec![
                                    MachineOperand::phys_reg(GR64::RBP),
                                    MachineOperand::None,
                                    MachineOperand::None,
                                    MachineOperand::Constant(MachineConstant::Int32(off)),
                                ],
                                builder.get_cur_bb().unwrap(),
                            )
                            .with_def(vec![eax.clone()]);
                            builder.insert(inst);
                            off += 8;
                            MachineOperand::Register(eax)
                        }
                    };
                    let inst = MachineInst::new_simple(
                        mov_mx(&src).unwrap(),
                        vec![
                            MachineOperand::phys_reg(GR64::RBP),
                            dst,
                            MachineOperand::None,
                            MachineOperand::None,
                            src,
                        ],
                        builder.get_cur_bb().unwrap(),
                    );
                    builder.insert(inst)
                }
                Type::Pointer(_) => {
                    unimplemented!()
                    // let off = finfo.offset(-(i as i32 + 1)).unwrap();
                    // dynasm!(self.asm; mov [rbp - off], Ra(reg4arg(i).unwrap()));
                }
                _ => unimplemented!(),
            }
        }
    }

    fn insert_epilogue(&mut self, cur_func: &mut MachineFunction) {
        let mut bb_iseq = vec![];

        for (bb_id, bb) in cur_func.body.basic_blocks.id_and_block() {
            let last_inst_id = *bb.iseq_ref().last().unwrap();
            let last_inst = &cur_func.body.inst_arena[last_inst_id];

            if last_inst.opcode != MachineOpcode::RET {
                continue;
            }

            let mut iseq = vec![];

            // mov rsp, rbp
            let i = MachineInst::new_simple(
                MachineOpcode::MOVrr64,
                vec![MachineOperand::phys_reg(GR64::RBP)],
                bb_id,
            )
            .with_def(vec![
                RegisterInfo::phys_reg(GR64::RSP).into_machine_register()
            ]);
            iseq.push(cur_func.body.inst_arena.alloc(i));

            // pop rbp
            let i = MachineInst::new_simple(
                MachineOpcode::POP64,
                vec![MachineOperand::phys_reg(GR64::RBP)],
                bb_id,
            );
            iseq.push(cur_func.body.inst_arena.alloc(i));

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
