use super::super::{
    dag::mc_convert::mov_mx,
    frame_object::*,
    machine::{abi::SystemV, register::*},
};
use super::inst::*;
use crate::codegen::common::machine::{
    builder::*,
    calling_conv::{ArgumentRegisterOrder, CallingConv},
    function::MachineFunction,
    module::MachineModule,
};
use crate::{ir::types::*, traits::pass::ModulePassTrait};
use rustc_hash::{FxHashMap, FxHashSet};

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

        let mut saved_regs = cur_func
            .body
            .appeared_phys_regs()
            .containing_callee_saved_regs()
            .to_phys_set()
            .into_iter()
            .map(|r| r.superest_reg())
            .collect::<FxHashSet<_>>();
        if cur_func.body.has_call() {
            saved_regs.insert(GR64::RBP.as_phys_reg());
        }
        let saved_regs = saved_regs.into_iter().collect::<Vec<_>>();
        let frame_objects = FrameObjectsInfo::new(tys, cur_func);
        let adjust = frame_objects.total_size();
        Self::remove_adjust_stack_inst(cur_func);
        self.insert_prologue(tys, cur_func, &saved_regs, adjust);
        self.insert_epilogue(cur_func, &saved_regs, adjust);
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
        tys: &Types,
        cur_func: &mut MachineFunction,
        saved_regs: &[PhysReg],
        adjust: i32,
    ) {
        let has_call = cur_func.body.has_call();
        let mut builder = Builder::new(cur_func);
        builder.set_insert_point_at_entry_block();

        for r in saved_regs {
            let push = MachineInst::new_simple(
                MachineOpcode::PUSH64,
                vec![MachineOperand::phys_reg(&builder.function.regs_info, *r)],
                builder.get_cur_bb().unwrap(),
            );
            builder.insert(push);
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
        .with_def(vec![RegisterOperand::new(
            builder.function.regs_info.get_phys_reg(GR64::RBP),
        )]);
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
            .with_def(vec![RegisterOperand::new(
                builder.function.regs_info.get_phys_reg(GR64::RSP),
            )]);
            builder.insert(sub_rsp);
        }

        self.insert_arg_copy(
            &tys.base.borrow(),
            &mut builder,
            saved_regs.len() as i32 * 8,
        );
    }

    fn insert_arg_copy<'a>(
        &mut self,
        tys: &'a TypesBase,
        builder: &'a mut Builder<'a>,
        saved_regs_bytes: i32,
    ) {
        CopyArgs::new(
            builder,
            &tys.as_function_ty(builder.function.ty).unwrap().params_ty,
            &tys.as_function_ty(builder.function.ty).unwrap().params_attr,
            saved_regs_bytes + 8, /*8=call*/
        )
        .copy();
    }

    fn insert_epilogue(
        &mut self,
        cur_func: &mut MachineFunction,
        saved_regs: &[PhysReg],
        adjust: i32,
    ) {
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
                .with_def(vec![RegisterOperand::new(
                    cur_func.regs_info.get_phys_reg(GR64::RSP),
                )]);
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, i));
            }

            for r in saved_regs.iter().rev() {
                let pop = MachineInst::new_simple(
                    MachineOpcode::POP64,
                    vec![MachineOperand::phys_reg(&cur_func.regs_info, *r)],
                    bb_id,
                );
                iseq.push(cur_func.body.inst_arena.alloc(&cur_func.regs_info, pop));
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
        init_off: i32,
    ) -> Self {
        Self {
            builder,
            params_ty,
            params_attr,
            offset: init_off, // call + push rbp. TODO: this may vary if there're more pushes
        }
    }

    pub fn copy(mut self) {
        let abi = SystemV::new();
        let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);
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
                Type::i8 => self.copy_int(ty, &mut arg_regs_order, i, 8),
                Type::i32 => self.copy_int(ty, &mut arg_regs_order, i, 32),
                Type::i64 | Type::Pointer(_) => self.copy_int(ty, &mut arg_regs_order, i, 64),
                Type::f64 => self.copy_f64(&mut arg_regs_order, i),
                _ => unimplemented!(),
            }
        }
    }

    fn copy_struct<ABI: CallingConv>(
        &mut self,
        ty: Type,
        arg_regs_order: &mut ArgumentRegisterOrder<ABI>,
        i: usize,
    ) where
        ABI: CallingConv,
    {
        let base = self.builder.function.types.base.clone();
        let base = base.borrow();
        let struct_ty = base.as_struct_ty(ty).unwrap();
        let sz = struct_ty.size();
        let mov8 = sz / 8;
        let mov4 = (sz - 8 * mov8) / 4;
        assert!((sz - 8 * mov8) % 4 == 0);
        let rbp = RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(GR64::RBP));
        let reg_classes = SystemV::reg_classes_used_for_passing_byval(struct_ty);

        if sz <= 16 && arg_regs_order.regs_available_for(&reg_classes) {
            let mut off = 0;
            for rc in reg_classes {
                let r = self
                    .builder
                    .function
                    .regs_info
                    .get_phys_reg(arg_regs_order.next(rc).unwrap());
                let mem = MachineOperand::Mem(if off == 0 {
                    MachineMemOperand::BaseFi(rbp, FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)))
                } else {
                    MachineMemOperand::BaseFiOff(
                        rbp,
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                        off as i32,
                    )
                });
                let mov = MachineInst::new_simple(
                    match rc {
                        RegisterClassKind::GR32 => MachineOpcode::MOVmr32,
                        RegisterClassKind::GR64 => MachineOpcode::MOVmr64,
                        RegisterClassKind::XMM => MachineOpcode::MOVSDmr,
                        RegisterClassKind::GR8 => unimplemented!(),
                    },
                    vec![mem, MachineOperand::Register(RegisterOperand::new(r))],
                    self.builder.get_cur_bb().unwrap(),
                );
                self.builder.insert(mov);
                off += match rc {
                    RegisterClassKind::XMM => 8,
                    _ => rc.size_in_byte(),
                };
            }
            return;
        }

        let rax = RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(GR64::RAX));
        let mut roff = 0;
        for (c, s, rm, mr) in vec![
            (mov8, 8, MachineOpcode::MOVrm64, MachineOpcode::MOVmr64),
            (mov4, 4, MachineOpcode::MOVrm32, MachineOpcode::MOVmr32),
        ]
        .into_iter()
        {
            for _ in 0..c {
                let mov = MachineInst::new_simple(
                    rm,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        rbp,
                        self.offset,
                    ))],
                    self.builder.get_cur_bb().unwrap(),
                )
                .with_def(vec![rax]);
                self.builder.insert(mov);
                let mem = if roff == 0 {
                    MachineMemOperand::BaseFi(
                        RegisterOperand::new(
                            self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        ),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                    )
                } else {
                    MachineMemOperand::BaseFiOff(
                        RegisterOperand::new(
                            self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        ),
                        FrameIndexInfo::new(ty, FrameIndexKind::Arg(i)),
                        roff as i32,
                    )
                };
                let mov = MachineInst::new_simple(
                    mr,
                    vec![MachineOperand::Mem(mem), MachineOperand::Register(rax)],
                    self.builder.get_cur_bb().unwrap(),
                );
                self.builder.insert(mov);
                self.offset += s;
                roff += s;
            }
        }
    }

    fn copy_f64<ABI>(&mut self, arg_regs_order: &mut ArgumentRegisterOrder<ABI>, i: usize)
    where
        ABI: CallingConv,
    {
        let ret_reg = XMM::XMM0.as_phys_reg();
        let dst = FrameIndexInfo::new(Type::f64, FrameIndexKind::Arg(i));
        let src = match arg_regs_order.next(RegisterClassKind::XMM) {
            Some(_arg_reg) => return, // MachineOperand::phys_reg(&self.builder.function.regs_info, arg_reg),
            None => {
                let ax =
                    RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(ret_reg));
                let inst = MachineInst::new_simple(
                    MachineOpcode::MOVSDrm,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        RegisterOperand::new(
                            self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        ),
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
            MachineOpcode::MOVSDmr,
            vec![
                MachineOperand::Mem(MachineMemOperand::BaseFi(
                    RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(GR64::RBP)),
                    dst,
                )),
                src,
            ],
            self.builder.get_cur_bb().unwrap(),
        );
        self.builder.insert(inst)
    }

    fn copy_int<ABI>(
        &mut self,
        ty: Type,
        arg_regs_order: &mut ArgumentRegisterOrder<ABI>,
        i: usize,
        bit: usize,
    ) where
        ABI: CallingConv,
    {
        let (ax, rc, movrm) = match bit {
            8 => (
                GR8::AL.as_phys_reg(),
                RegisterClassKind::GR8,
                MachineOpcode::MOVrm8,
            ),
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
                let ax = RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(ax));
                let inst = MachineInst::new_simple(
                    movrm,
                    vec![MachineOperand::Mem(MachineMemOperand::BaseOff(
                        RegisterOperand::new(
                            self.builder.function.regs_info.get_phys_reg(GR64::RBP),
                        ),
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
                    RegisterOperand::new(self.builder.function.regs_info.get_phys_reg(GR64::RBP)),
                    dst,
                )),
                src,
            ],
            self.builder.get_cur_bb().unwrap(),
        );
        self.builder.insert(inst)
    }
}
