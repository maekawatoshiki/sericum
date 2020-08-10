use super::super::{dag::mc_convert::mov_mx, frame_object::*, machine::register::*};
use super::inst::*;
use crate::codegen::common::machine::{
    builder::*, function::MachineFunction, module::MachineModule,
};
use crate::{ir::types::*, traits::pass::ModulePassTrait};
use rustc_hash::FxHashMap;

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
    params_attr: &'a FxHashMap<usize, ParamAttribute>,
}

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
        if cur_func.is_internal {
            return;
        }

        let frame_objects = FrameObjectsInfo::new(tys, cur_func);
        let adjust = frame_objects.total_size();
        Self::remove_adjust_stack_inst(cur_func);
        self.insert_prologue(tys, cur_func, adjust);
        self.insert_epilogue(cur_func, adjust);
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

    fn insert_prologue(&mut self, tys: &Types, cur_func: &mut MachineFunction, adjust: i32) {
        let has_call = cur_func.body.has_call();
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_block();

        if has_call || adjust > 0 {
            // push rbp
            let push_rbp = MachineInst::new_simple(
                MachineOpcode::PUSH64,
                vec![MachineOperand::phys_reg(
                    &builder.function.regs_info,
                    GR64::RBP,
                )],
                builder.get_cur_bb().unwrap(),
            );
            builder.insert(push_rbp);
        }

        if adjust == 0 {
            return;
        }

        // mov rbp, rsp
        let mov_rbp_rsp = MachineInst::new_simple(
            MachineOpcode::MOVrr64,
            vec![MachineOperand::phys_reg(
                &builder.function.regs_info,
                GR64::RSP,
            )],
            builder.get_cur_bb().unwrap(),
        )
        .with_def(vec![builder.function.regs_info.get_phys_reg(GR64::RBP)]);
        builder.insert(mov_rbp_rsp);

        if has_call {
            // sub rsp, adjust
            let sub_rsp = MachineInst::new_simple(
                MachineOpcode::SUBr64i32,
                vec![
                    MachineOperand::phys_reg(&builder.function.regs_info, GR64::RSP),
                    MachineOperand::imm_i32(adjust),
                ],
                builder.get_cur_bb().unwrap(),
            )
            .with_def(vec![builder.function.regs_info.get_phys_reg(GR64::RSP)]);
            builder.insert(sub_rsp);
        }

        self.insert_arg_copy(&tys.base.borrow(), &mut builder);
    }

    fn insert_arg_copy<'a>(&mut self, tys: &'a TypesBase, builder: &'a mut Builder<'a>) {
        CopyArgs::new(
            builder,
            &tys.as_function_ty(builder.function.ty).unwrap().params_ty,
            &tys.as_function_ty(builder.function.ty).unwrap().params_attr,
        )
        .copy();
    }

    fn insert_epilogue(&mut self, cur_func: &mut MachineFunction, adjust: i32) {
        let mut bb_iseq = vec![];
        let has_call = cur_func.body.has_call();

        for (bb_id, bb) in cur_func.body.basic_blocks.id_and_block() {
            let last_inst_id = *bb.iseq_ref().last().unwrap();
            let last_inst = &cur_func.body.inst_arena[last_inst_id];

            if last_inst.opcode != MachineOpcode::RET {
                continue;
            }

            let mut iseq = vec![];

            if has_call && adjust > 0 {
                // add rsp, adjust
                let i = MachineInst::new_simple(
                    MachineOpcode::ADDr64i32,
                    vec![
                        MachineOperand::phys_reg(&cur_func.regs_info, GR64::RSP),
                        MachineOperand::imm_i32(adjust),
                    ],
                    bb_id,
                )
                .with_def(vec![cur_func.regs_info.get_phys_reg(GR64::RSP)]);
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, i));
            }

            if has_call || adjust > 0 {
                // pop rbp
                let i = MachineInst::new_simple(
                    MachineOpcode::POP64,
                    vec![MachineOperand::phys_reg(&cur_func.regs_info, GR64::RBP)],
                    bb_id,
                );
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, i));
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

impl<'a> CopyArgs<'a> {
    pub fn new(
        builder: &'a mut Builder<'a>,
        params_ty: &'a Vec<Type>,
        params_attr: &'a FxHashMap<usize, ParamAttribute>,
    ) -> Self {
        Self {
            builder,
            params_ty,
            params_attr,
            offset: 16, // call + push rbp. TODO: this may vary if there're more pushes
        }
    }

    pub fn copy(mut self) {
        let mut arg_regs_order = RegisterClassKind::arg_regs();
        for (i, &ty) in self.params_ty.iter().enumerate() {
            let byval = self.params_attr.get(&i).map_or(false, |attr| attr.byval);
            if byval {
                let struct_ty = self
                    .builder
                    .function
                    .types
                    .get_element_ty(ty, None)
                    .unwrap();
                self.copy_struct(struct_ty, &mut arg_regs_order, i);
                continue;
            }
            match ty {
                Type::Int32 => self.copy_int(ty, &mut arg_regs_order, i, 32),
                Type::Int64 | Type::Pointer(_) => self.copy_int(ty, &mut arg_regs_order, i, 64),
                Type::F64 => self.copy_f64(&mut arg_regs_order, i),
                _ => unimplemented!(),
            }
        }
    }

    fn copy_struct(&mut self, ty: Type, arg_regs_order: &mut ArgRegs, i: usize) {
        let sz = ty.size_in_byte(&self.builder.function.types);

        let rbp = self.builder.function.regs_info.get_phys_reg(GR64::RBP);

        if sz <= 16 {
            let mov8 = sz / 8;
            let mov4 = (sz - 8 * mov8) / 4;
            assert!((sz - 8 * mov8) % 4 == 0);
            let mut off = 0;

            for (c, s, rc, op) in vec![
                (mov8, 8, RegisterClassKind::GR64, MachineOpcode::MOVmr64),
                (mov4, 4, RegisterClassKind::GR32, MachineOpcode::MOVmr32),
            ]
            .into_iter()
            {
                for _ in 0..c {
                    let r = self
                        .builder
                        .function
                        .regs_info
                        .get_phys_reg(arg_regs_order.next(rc).unwrap());
                    let mem = if off == 0 {
                        MachineOperand::Mem(MachineMemOperand::BaseFi(
                            rbp,
                            FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                        ))
                    } else {
                        MachineOperand::Mem(MachineMemOperand::BaseFiOff(
                            rbp,
                            FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                            off,
                        ))
                    };
                    off += s;
                    let mov = MachineInst::new_simple(
                        op,
                        vec![mem, MachineOperand::Register(r)],
                        self.builder.get_cur_bb().unwrap(),
                    );
                    self.builder.insert(mov);
                }
            }
            return;
        }

        let mut x = 0;
        while x < sz {
            if x + 8 <= sz {
                let rax = self.builder.function.regs_info.get_phys_reg(GR64::RAX);
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVrm64,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        self.offset,
                    ))],
                    self.builder.get_cur_bb().unwrap(),
                )
                .with_def(vec![rax]);
                self.builder.insert(mov);
                let mem = if x == 0 {
                    MachineMemOperand::BaseFi(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                    )
                } else {
                    MachineMemOperand::BaseFiOff(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                        x as i32,
                    )
                };
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVmr64,
                    vec![MachineOperand::Mem(mem), MachineOperand::Register(rax)],
                    self.builder.get_cur_bb().unwrap(),
                );
                self.builder.insert(mov);
                self.offset += 8;
                x += 8;
                continue;
            }
            if x + 4 <= sz {
                let eax = self.builder.function.regs_info.get_phys_reg(GR32::EAX);
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVrm32,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        self.offset,
                    ))],
                    self.builder.get_cur_bb().unwrap(),
                )
                .with_def(vec![eax]);
                self.builder.insert(mov);
                let mem = if x == 0 {
                    MachineMemOperand::BaseFi(
                        self.builder.function.regs_info.get_phys_reg(GR32::EBP),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                    )
                } else {
                    MachineMemOperand::BaseFiOff(
                        self.builder.function.regs_info.get_phys_reg(GR32::EBP),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                        x as i32,
                    )
                };
                let mov = MachineInst::new_simple(
                    MachineOpcode::MOVmr32,
                    vec![MachineOperand::Mem(mem), MachineOperand::Register(eax)],
                    self.builder.get_cur_bb().unwrap(),
                );
                self.builder.insert(mov);
                self.offset += 4;
                x += 4;
                continue;
            }
            unimplemented!()
        }
    }

    fn copy_f64(&mut self, arg_regs_order: &mut ArgRegs, i: usize) {
        let ret_reg = XMM::XMM0.as_phys_reg();
        let dst = FrameIndexInfo::new(Type::F64, FrameIndexKind::Arg(i));
        let src = match arg_regs_order.next(RegisterClassKind::XMM) {
            Some(_arg_reg) => return, // MachineOperand::phys_reg(&self.builder.function.regs_info, arg_reg),
            None => {
                let ax = self.builder.function.regs_info.get_phys_reg(ret_reg);
                let inst = MachineInst::new_simple(
                    MachineOpcode::MOVSDrm,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        self.offset,
                    ))],
                    self.builder.get_cur_bb().unwrap(),
                )
                .with_def(vec![ax.clone()]);
                self.builder.insert(inst);
                self.offset += 8;
                MachineOperand::Register(ax)
            }
        };
        let inst = MachineInst::new_simple(
            MachineOpcode::MOVSDmr,
            vec![
                MachineOperand::Mem(MachineMemOperand::BaseFi(
                    self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                    dst,
                )),
                src,
            ],
            self.builder.get_cur_bb().unwrap(),
        );
        self.builder.insert(inst)
    }

    fn copy_int(&mut self, ty: Type, arg_regs_order: &mut ArgRegs, i: usize, bit: usize) {
        let (ax, rc, movrm) = match bit {
            32 => (
                GR32::EAX.as_phys_reg(),
                RegisterClassKind::GR32,
                MachineOpcode::MOVrm32,
            ),
            64 => (
                GR64::RAX.as_phys_reg(),
                RegisterClassKind::GR64,
                MachineOpcode::MOVrm64,
            ),
            _ => unimplemented!(),
        };
        let dst = FrameIndexInfo::new(ty, FrameIndexKind::Arg(i));
        let src = match arg_regs_order.next(rc) {
            Some(_arg_reg) => return, // MachineOperand::phys_reg(&self.builder.function.regs_info, arg_reg),
            None => {
                let ax = self.builder.function.regs_info.get_phys_reg(ax);
                let inst = MachineInst::new_simple(
                    movrm,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        self.offset,
                    ))],
                    self.builder.get_cur_bb().unwrap(),
                )
                .with_def(vec![ax]);
                self.builder.insert(inst);
                self.offset += 8;
                MachineOperand::Register(ax)
            }
        };
        let inst = MachineInst::new_simple(
            mov_mx(&self.builder.function.regs_info, &src).unwrap(),
            vec![
                MachineOperand::Mem(MachineMemOperand::BaseFi(
                    self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                    dst,
                )),
                src,
            ],
            self.builder.get_cur_bb().unwrap(),
        );
        self.builder.insert(inst)
    }
}
