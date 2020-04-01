use super::super::{dag::mc_convert::mov_mx, frame_object::*, register::*};
use super::{builder::*, function::MachineFunction, inst::*, module::MachineModule};
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
        let push_rbp = MachineInst::new_simple(
            MachineOpcode::PUSH64,
            vec![MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
            )],
            builder.get_cur_bb().unwrap(),
        );
        let push_rbp = builder.function.inst_arena.alloc(push_rbp);
        builder.insert(push_rbp);

        // mov rbp, rsp
        let mov_rbp_rsp = MachineInst::new_simple(
            MachineOpcode::MOVrr64,
            vec![MachineOperand::Register(
                RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register(),
            )],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![
            RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register()
        ]);
        let mov_rbp_rsp = builder.function.inst_arena.alloc(mov_rbp_rsp);
        builder.insert(mov_rbp_rsp);

        // sub rsp, adjust
        let adjust = roundup(finfo.total_size() + /*push rbp=*/8, 16) - 8;
        let mov_rsp_i = MachineInst::new_simple(
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
        let mov_rsp_i = builder.function.inst_arena.alloc(mov_rsp_i);
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
                    let rbp = MachineOperand::Register(
                        RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
                    );
                    let inst = MachineInst::new_simple(
                        mov_mx(&src).unwrap(),
                        vec![rbp, dst, MachineOperand::None, MachineOperand::None, src],
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

        for (bb_id, bb) in cur_func.basic_blocks.id_and_block() {
            let last_inst_id = *bb.iseq_ref().last().unwrap();
            let last_inst = &cur_func.inst_arena[last_inst_id];

            if last_inst.opcode != MachineOpcode::RET {
                continue;
            }

            let mut iseq = vec![];

            // mov rsp, rbp
            let i = MachineInst::new_simple(
                MachineOpcode::MOVrr64,
                vec![MachineOperand::Register(
                    RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
                )],
                bb_id,
            )
            .with_def(vec![
                RegisterInfo::new_phy_reg(GR64::RSP).into_machine_register()
            ]);
            iseq.push(cur_func.inst_arena.alloc(i));

            // pop rbp
            let i = MachineInst::new_simple(
                MachineOpcode::POP64,
                vec![MachineOperand::Register(
                    RegisterInfo::new_phy_reg(GR64::RBP).into_machine_register(),
                )],
                bb_id,
            );
            iseq.push(cur_func.inst_arena.alloc(i));

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
