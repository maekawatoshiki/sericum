use super::super::{dag::mc_convert::mov_mx, frame_object::*, register::*};
use super::{builder::*, function::MachineFunction, instr::*, module::MachineModule};
use crate::codegen::x64::exec::roundup;
use crate::ir::types::*;

pub struct PrologueEpilogueInserter {}

impl PrologueEpilogueInserter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            self.run_on_function(func);
        }
    }

    pub fn run_on_function(&mut self, cur_func: &mut MachineFunction) {
        if cur_func.internal {
            return;
        }

        let frame_info = FrameObjectsInfo::new(cur_func);
        self.insert_prologue(cur_func, &frame_info);
        self.insert_epilogue(cur_func);
    }

    fn insert_prologue(&mut self, cur_func: &mut MachineFunction, finfo: &FrameObjectsInfo) {
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_bb();

        // push rbp
        let push_rbp = MachineInstr::new_simple(
            MachineOpcode::PUSH64,
            vec![MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            )],
            builder.get_cur_bb().unwrap(),
        );
        let push_rbp = builder.function.instr_arena.alloc(push_rbp);
        builder.insert(push_rbp);

        // mov rbp, rsp
        let mov_rbp_rsp = MachineInstr::new_simple(
            MachineOpcode::MOVrr64,
            vec![MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register(),
            )],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![
            RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register()
        ]);
        let mov_rbp_rsp = builder.function.instr_arena.alloc(mov_rbp_rsp);
        builder.insert(mov_rbp_rsp);

        // sub rsp, adjust
        let adjust = roundup(finfo.total_size() + /*push rbp=*/8, 16) - 8;
        let mov_rsp_i = MachineInstr::new_simple(
            MachineOpcode::SUBr64i32,
            vec![
                MachineOperand::Register(
                    RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register(),
                ),
                MachineOperand::Constant(MachineConstant::Int32(adjust)),
            ],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![
            RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register()
        ]);
        let mov_rsp_i = builder.function.instr_arena.alloc(mov_rsp_i);
        builder.insert(mov_rsp_i);

        self.insert_arg_copy(&mut builder)
    }

    fn insert_arg_copy<'a>(&mut self, builder: &mut Builder<'a>) {
        for (i, ty) in builder
            .function
            .ty
            .get_function_ty()
            .unwrap()
            .params_ty
            .clone()
            .into_iter()
            .enumerate()
        {
            match ty {
                Type::Int32 => {
                    let dst =
                        MachineOperand::FrameIndex(FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)));
                    let src = MachineOperand::Register(
                        RegisterInfo::new_phy_reg(
                            RegisterClassKind::GR32.get_nth_arg_reg(i).unwrap(),
                        )
                        .into_machine_register(),
                    );
                    let inst = MachineInstr::new_simple(
                        mov_mx(&src).unwrap(),
                        vec![dst, src],
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

        for bb_id in &cur_func.basic_blocks {
            let bb = &cur_func.basic_block_arena[*bb_id];
            let last_instr_id = *bb.iseq_ref().last().unwrap();
            let last_instr = &cur_func.instr_arena[last_instr_id];

            if last_instr.opcode != MachineOpcode::Ret {
                continue;
            }

            let ret = last_instr.clone();
            let mut iseq = vec![];

            bb.iseq_ref_mut().pop(); // Ret

            match &ret.operand[0] {
                MachineOperand::Constant(c) => match c {
                    MachineConstant::Int32(i) => iseq.push(
                        cur_func.instr_arena.alloc(
                            MachineInstr::new_simple(
                                MachineOpcode::MOVri32,
                                vec![MachineOperand::Constant(MachineConstant::Int32(*i))],
                                *bb_id,
                            )
                            .with_def(vec![
                                RegisterInfo::new_phy_reg(GR32::EAX).into_machine_register()
                            ]),
                        ),
                    ),
                    _ => unimplemented!(),
                },
                MachineOperand::Register(r) => {
                    // dynasm!(self.asm; mov rax, Ra(register!(i)))
                    iseq.push(
                        cur_func.instr_arena.alloc(
                            MachineInstr::new_simple(
                                MachineOpcode::MOVrr32,
                                vec![MachineOperand::Register(r.clone())],
                                *bb_id,
                            )
                            .with_def(vec![
                                RegisterInfo::new_phy_reg(GR32::EAX).into_machine_register()
                            ]),
                        ),
                    )
                }
                MachineOperand::None => {}
                _ => unreachable!(),
            }

            // mov rsp, rbp
            let i = MachineInstr::new_simple(
                MachineOpcode::MOVrr64,
                vec![MachineOperand::Register(
                    RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
                )],
                *bb_id,
            )
            .with_def(vec![
                RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register()
            ]);
            iseq.push(cur_func.instr_arena.alloc(i));

            // pop rbp
            let i = MachineInstr::new_simple(
                MachineOpcode::POP64,
                vec![MachineOperand::Register(
                    RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
                )],
                *bb_id,
            );
            iseq.push(cur_func.instr_arena.alloc(i));

            // ret
            let i = MachineInstr::new_simple(MachineOpcode::RET, vec![], *bb_id);
            iseq.push(cur_func.instr_arena.alloc(i));

            bb_iseq.push((*bb_id, iseq));
        }

        for (bb_id, iseq) in bb_iseq {
            let mut builder = Builder::new(cur_func);
            builder.set_insert_point_at_end(bb_id);
            for inst in iseq {
                builder.insert(inst)
            }
        }
    }
}
