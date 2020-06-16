use super::super::frame_object::FrameObjectsInfo;
use super::super::machine::{inst::*, register::RegistersInfo};
use crate::codegen::common::machine::{
    basic_block::MachineBasicBlockId,
    function::{InstIter, MachineFunction},
    module::MachineModule,
};
use crate::ir::types::{TypeSize, Types};

pub struct MachineAsmPrinter {
    pub output: String,
    cur_bb_id_base: usize,
}

impl MachineAsmPrinter {
    pub fn new() -> Self {
        Self {
            output: "".to_string(),
            cur_bb_id_base: 0,
        }
    }

    pub fn run_on_module(&mut self, m: &MachineModule) {
        self.output.push_str("  .text\n");
        self.output.push_str("  .intel_syntax noprefix\n");

        for (_, func) in &m.functions {
            self.run_on_function(&m.types, &func)
        }
    }

    fn run_on_function(&mut self, tys: &Types, f: &MachineFunction) {
        if f.is_internal {
            return;
        }

        let fo = FrameObjectsInfo::new(tys, f);

        self.output
            .push_str(format!("  .globl {}\n", f.name).as_str()); // TODO

        self.output.push_str(format!("{}:\n", f.name).as_str());

        self.run_on_basic_blocks(tys, f, &fo);
    }

    fn run_on_basic_blocks(&mut self, tys: &Types, f: &MachineFunction, fo: &FrameObjectsInfo) {
        for (id, _, inst_iter) in f.body.mbb_iter() {
            self.output
                .push_str(format!("{}:\n", self.bb_id_to_label_id(&id)).as_str());
            self.run_on_basic_block(tys, &f.regs_info, inst_iter, fo);
        }
        self.cur_bb_id_base += f.body.basic_blocks.order.len();
    }

    fn run_on_basic_block<'a>(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        inst_iter: InstIter<'a>,
        fo: &FrameObjectsInfo,
    ) {
        for (_, inst) in inst_iter {
            self.run_on_inst(tys, regs_info, inst, fo);
        }
    }

    fn run_on_inst(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        inst: &MachineInst,
        fo: &FrameObjectsInfo,
    ) {
        self.output.push_str("  ");

        println!("{:?}", inst.opcode);
        self.output.push_str(inst.opcode.inst_def().unwrap().name);
        self.output.push(' ');

        let mut word = None;
        for (i, r) in inst.def.iter().enumerate() {
            use crate::codegen::common::machine::inst_def::DefOrUseReg;
            if inst
                .opcode
                .inst_def()
                .unwrap()
                .tie
                .get(&DefOrUseReg::Def(i))
                .is_some()
            {
                continue;
            }
            self.output.push_str(r.as_phys_reg().name());
            word = Some(byte2word(r.as_phys_reg().reg_class().size_in_byte()));
            if i != inst.def.len() - 1 - inst.opcode.inst_def().unwrap().tie.len() {
                self.output.push_str(", ");
            }
        }

        if inst.def.len() - inst.opcode.inst_def().unwrap().tie.len() > 0 && inst.operand.len() > 0
        {
            self.output.push_str(", ");
        }

        for (i, o) in inst.operand.iter().enumerate() {
            self.operand2asm(inst.opcode, fo, o);
            if i != inst.operand.len() - 1 {
                self.output.push_str(", ");
            }
        }

        self.output.push('\n');

        // if inst.opcode.inst_def().is_none() {
        //     println!(">>> {:?}", inst.opcode);
        // }
        // match inst.opcode {
        //     MachineOpcode::MOVSDrm64 => {}
        //     MachineOpcode::MOVSXDr64m32 => {}
        //     MachineOpcode::MOVrr32
        //     | MachineOpcode::MOVri32
        //     | MachineOpcode::MOVrr64
        //     | MachineOpcode::MOVri64 => self.run_on_inst_mov_rx(inst),
        //     MachineOpcode::MOVrm32 | MachineOpcode::MOVrm64 => self.run_on_inst_mov_rm(inst, fo),
        //     MachineOpcode::MOVmr32 | MachineOpcode::MOVmi32 => {
        //         self.run_on_inst_mov_mx(tys, regs_info, inst, fo)
        //     }
        //     MachineOpcode::LEAr64m => self.run_on_inst_lea_rm(inst, fo),
        //     MachineOpcode::ADDrr32 | MachineOpcode::ADDri32 | MachineOpcode::ADDr64i32 => {
        //         self.run_on_inst_add(inst)
        //     }
        //     MachineOpcode::SUBrr32 | MachineOpcode::SUBri32 | MachineOpcode::SUBr64i32 => {
        //         self.run_on_inst_sub(inst)
        //     }
        //     MachineOpcode::IMULrr32 => self.run_on_inst_imul_rr(inst),
        //     MachineOpcode::IMULrri32 | MachineOpcode::IMULrr64i32 => {
        //         self.run_on_inst_imul_rri(inst)
        //     }
        //     MachineOpcode::CDQ => self.run_on_inst_cdq(),
        //     MachineOpcode::IDIV => self.run_on_inst_idiv(inst),
        //     MachineOpcode::PUSH64 => self.run_on_inst_push(inst),
        //     MachineOpcode::POP64 => self.run_on_inst_pop(inst),
        //     MachineOpcode::RET => self.run_on_inst_ret(),
        //     MachineOpcode::CALL => self.run_on_inst_call(inst),
        //     MachineOpcode::Seteq | MachineOpcode::Setle | MachineOpcode::Setlt => {}
        //     MachineOpcode::JMP => self.run_on_inst_jmp(inst),
        //     MachineOpcode::BrCond => {}
        //     MachineOpcode::CMPri | MachineOpcode::CMPrr => self.run_on_inst_cmp(inst),
        //     MachineOpcode::JE => self.run_on_inst_je(inst),
        //     MachineOpcode::JLE => self.run_on_inst_jle(inst),
        //     MachineOpcode::JL => self.run_on_inst_jl(inst),
        //     _ => panic!("{:?}", inst),
        // }
        //
        // self.output.push('\n');
    }

    fn operand2asm(
        &mut self,
        opcode: MachineOpcode,
        fo: &FrameObjectsInfo,
        operand: &MachineOperand,
    ) {
        match operand {
            MachineOperand::Branch(id) => self.output.push_str(self.bb_id_to_label_id(id).as_str()),
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                self.output.push_str(format!("{}", i).as_str())
            }
            MachineOperand::Constant(MachineConstant::Int8(i)) => {
                self.output.push_str(format!("{}", i).as_str())
            }
            MachineOperand::Register(r) => self.output.push_str(r.as_phys_reg().name()),
            MachineOperand::FrameIndex(i) => self
                .output
                .push_str(format!("{}", fo.offset(i.idx).unwrap()).as_str()),
            // MachineOperand::Mem(MachineMemOperand::FiReg(fi, r)) => self.output.push_str(
            //     format!("{}({})", fo.offset(fi.idx).unwrap(), r.as_phys_reg().name()).as_str(),
            // ),
            // MachineOperand::Mem(MachineMemOperand::ImmReg(imm, r)) => self
            //     .output
            //     .push_str(format!("{}({})", imm, r.as_phys_reg().name()).as_str()),
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(name))) => {
                self.output.push_str(name.as_str())
            }
            MachineOperand::Mem(_) => {
                let w = match opcode {
                    MachineOpcode::MOVSDrm64 => 8,
                    MachineOpcode::MOVSDmr => 8,
                    MachineOpcode::MOVSDrm => 8,
                    MachineOpcode::MOVSDrr => 8,
                    MachineOpcode::MOVrm32 => 4,
                    MachineOpcode::MOVmr32 => 4,
                    MachineOpcode::MOVmi32 => 4,
                    MachineOpcode::MOVmr64 => 8,
                    MachineOpcode::MOVmi64 => 8,
                    MachineOpcode::MOVSXDr64m32 => 4,
                    MachineOpcode::LEAr64m => 8,
                    MachineOpcode::ADDrr32 => 4,
                    MachineOpcode::ADDrr64 => 4,
                    MachineOpcode::ADDri32 => 4,
                    MachineOpcode::ADDr64i32 => 4,
                    MachineOpcode::ADDSDrr => 4,
                    MachineOpcode::ADDSDrm => 4,
                    MachineOpcode::SUBrr32 => 4,
                    MachineOpcode::SUBri32 => 4,
                    MachineOpcode::SUBr64i32 => 4,
                    MachineOpcode::SUBSDrr => 4,
                    MachineOpcode::SUBSDrm => 4,
                    MachineOpcode::IMULrr32 => 4,
                    MachineOpcode::IMULrri32 => 4,
                    MachineOpcode::IMULrr64i32 => 4,
                    MachineOpcode::MULSDrr => 4,
                    MachineOpcode::MULSDrm => 4,
                    MachineOpcode::CDQ => 4,
                    MachineOpcode::IDIV => 4,
                    MachineOpcode::DIVSDrr => 4,
                    MachineOpcode::DIVSDrm => 8,
                    MachineOpcode::SHLr64i8 => 4,
                    MachineOpcode::SHLr32i8 => 4,
                    MachineOpcode::SQRTSDrr => 4,
                    MachineOpcode::MOVrr32 => 4,
                    MachineOpcode::MOVri32 => 4,
                    MachineOpcode::MOVrr64 => 4,
                    MachineOpcode::MOVri64 => 4,
                    MachineOpcode::MOVrm64 => 8,
                    MachineOpcode::PUSH64 => 4,
                    MachineOpcode::POP64 => 4,
                    MachineOpcode::RET => 4,
                    MachineOpcode::CALL => 4,
                    MachineOpcode::CMPrr => 4,
                    MachineOpcode::CMPri => 4,
                    MachineOpcode::UCOMISDrr => 4,
                    MachineOpcode::JE => 4,
                    MachineOpcode::JBE => 4,
                    MachineOpcode::JB => 4,
                    MachineOpcode::JLE => 4,
                    MachineOpcode::JL => 4,
                    MachineOpcode::JA => 4,
                    MachineOpcode::JAE => 4,
                    MachineOpcode::JG => 4,
                    MachineOpcode::JGE => 4,
                    MachineOpcode::JMP => 4,
                    _ => unreachable!(),
                };
                let word = byte2word(w);
                self.run_on_mem_operand(operand, fo, word)
            }
            e => unimplemented!("{:?}", e),
        };
    }

    fn run_on_inst_mov_rx(&mut self, i: &MachineInst) {
        self.output.push_str("mov ");
        self.output
            .push_str(format!("{}, ", i.def[0].as_phys_reg().name()).as_str());
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_mov_rm(&mut self, i: &MachineInst, fo: &FrameObjectsInfo) {
        let word = byte2word(i.def[0].as_phys_reg().reg_class().size_in_byte());
        self.output.push_str("mov ");
        self.output
            .push_str(format!("{}, ", i.def[0].as_phys_reg().name()).as_str());
        self.run_on_mem_operand(&i.operand[0], fo, word);
    }

    fn run_on_inst_mov_mx(
        &mut self,
        tys: &Types,
        regs_info: &RegistersInfo,
        i: &MachineInst,
        fo: &FrameObjectsInfo,
    ) {
        let word = byte2word(i.operand[1].get_type(regs_info).unwrap().size_in_byte(tys));
        self.output.push_str("mov ");
        self.run_on_mem_operand(&i.operand[0], fo, word);
        self.output.push_str(", ");
        self.run_on_operand(&i.operand[1]);
    }

    fn run_on_inst_lea_rm(&mut self, i: &MachineInst, fo: &FrameObjectsInfo) {
        self.output.push_str("lea ");
        self.output
            .push_str(format!("{}, ", i.def[0].as_phys_reg().name()).as_str());

        // out = lea rbp, fi, none, none
        if i.operand[0].is_register() // must be rbp
            && i.operand[1].is_frame_index()
            && i.operand[2].is_none()
            && i.operand[3].is_none()
        {
            let offset = fo.offset(i.operand[1].as_frame_index().idx).unwrap();
            self.output.push_str(format!("[rbp - {}]", offset).as_str());
        }

        // out = lea rbp, fi, none, const.off
        if i.operand[0].is_register() // must be rbp
            && i.operand[1].is_frame_index()
            && i.operand[2].is_none()
            && i.operand[3].is_const_i32()
        {
            let off1 = fo.offset(i.operand[1].as_frame_index().idx).unwrap();
            let off2 = i.operand[3].as_constant().as_i32();
            assert!(off1 >= off2);
            let offset = off1 - off2;
            self.output.push_str(format!("[rbp - {}]", offset).as_str());
        }

        // out = lea rbp, fi, align, off
        if i.operand[0].is_register() // must be rbp
            && i.operand[1].is_frame_index()
            && i.operand[2].is_const_i32()
            && i.operand[3].is_register()
        {
            let offset = fo.offset(i.operand[1].as_frame_index().idx).unwrap();
            let align = i.operand[2].as_constant().as_i32();
            let reg = i.operand[3].as_register().as_phys_reg();
            self.output
                .push_str(format!("[rbp + {}*{} - {}]", align, reg.name(), offset).as_str());
        }

        // out = lea base, none, align, off
        if i.operand[0].is_register()
            && i.operand[1].is_none()
            && i.operand[2].is_const_i32()
            && i.operand[3].is_register()
        {
            let base = i.operand[0].as_register().as_phys_reg();
            let align = i.operand[2].as_constant().as_i32();
            let reg = i.operand[3].as_register().as_phys_reg();
            self.output
                .push_str(format!("[{} + {}*{}]", base.name(), align, reg.name()).as_str());
        }

        // out = lea base, none, none, none
        if i.operand[0].is_register()
            && i.operand[1].is_none()
            && i.operand[2].is_none()
            && i.operand[3].is_none()
        {
            let base = i.operand[0].as_register().as_phys_reg();
            self.output.push_str(format!("[{}]", base.name()).as_str());
        }
    }

    fn run_on_mem_operand(&mut self, op: &MachineOperand, fo: &FrameObjectsInfo, word: &str) {
        match op {
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let base = base.as_phys_reg();
                let offset = fo.offset(fi.idx).unwrap();
                self.output
                    .push_str(format!("{} ptr [{} - {}]", word, base.name(), offset).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let base = base.as_phys_reg();
                let reg = off.as_phys_reg();
                self.output.push_str(
                    format!("{} ptr [{} + {}*{}]", word, base.name(), align, reg.name()).as_str(),
                );
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let offset = fo.offset(fi.idx).unwrap();
                let base = base.as_phys_reg();
                let reg = off.as_phys_reg();
                self.output.push_str(
                    format!(
                        "{} ptr [{} + {}*{} - {}]",
                        word,
                        base.name(),
                        align,
                        reg.name(),
                        offset
                    )
                    .as_str(),
                );
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let base = base.as_phys_reg();
                let off1 = fo.offset(fi.idx).unwrap();
                let off2 = *off;
                assert!(off1 >= off2);
                let offset = off1 - off2;
                self.output
                    .push_str(format!("{} ptr [{} - {}]", word, base.name(), offset).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let base = base.as_phys_reg();
                self.output
                    .push_str(format!("{} ptr [{}]", word, base.name()).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let base = base.as_phys_reg();
                self.output
                    .push_str(format!("{} ptr [{} + {}]", word, base.name(), off).as_str());
            }
            e => panic!("{:?}", e),
        }
    }

    fn run_on_inst_add(&mut self, i: &MachineInst) {
        self.output.push_str("add ");
        self.run_on_operand(&i.operand[0]);
        self.output.push_str(", ");
        self.run_on_operand(&i.operand[1]);
    }

    fn run_on_inst_sub(&mut self, i: &MachineInst) {
        self.output.push_str("sub ");
        self.run_on_operand(&i.operand[0]);
        self.output.push_str(", ");
        self.run_on_operand(&i.operand[1]);
    }

    fn run_on_inst_imul_rr(&mut self, i: &MachineInst) {
        self.output.push_str("imul ");
        self.output
            .push_str(format!("{}, ", i.def[0].as_phys_reg().name()).as_str());
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_imul_rri(&mut self, i: &MachineInst) {
        self.output.push_str("imul ");
        self.output
            .push_str(format!("{}, ", i.def[0].as_phys_reg().name()).as_str());
        self.run_on_operand(&i.operand[0]);
        self.output.push_str(", ");
        self.run_on_operand(&i.operand[1]);
    }

    fn run_on_inst_cdq(&mut self) {
        self.output.push_str("cdq");
    }

    fn run_on_inst_idiv(&mut self, i: &MachineInst) {
        self.output.push_str("idiv ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_push(&mut self, i: &MachineInst) {
        self.output.push_str("push ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_pop(&mut self, i: &MachineInst) {
        self.output.push_str("pop ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_ret(&mut self) {
        self.output.push_str("ret");
    }

    fn run_on_inst_call(&mut self, i: &MachineInst) {
        self.output.push_str("call ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_cmp(&mut self, i: &MachineInst) {
        self.output.push_str("cmp ");
        self.run_on_operand(&i.operand[0]);
        self.output.push_str(", ");
        self.run_on_operand(&i.operand[1]);
    }

    fn run_on_inst_jmp(&mut self, i: &MachineInst) {
        self.output.push_str("jmp ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_je(&mut self, i: &MachineInst) {
        self.output.push_str("je ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_jle(&mut self, i: &MachineInst) {
        self.output.push_str("jle ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_inst_jl(&mut self, i: &MachineInst) {
        self.output.push_str("jl ");
        self.run_on_operand(&i.operand[0]);
    }

    fn run_on_operand(&mut self, operand: &MachineOperand) {
        match operand {
            MachineOperand::Branch(id) => self.output.push_str(self.bb_id_to_label_id(id).as_str()),
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                self.output.push_str(format!("{}", i).as_str())
            }
            MachineOperand::Register(r) => self.output.push_str(r.as_phys_reg().name()),
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(name))) => {
                self.output.push_str(name.as_str())
            }
            _ => unimplemented!(),
        }
    }

    fn bb_id_to_label_id(&self, bb_id: &MachineBasicBlockId) -> String {
        format!(".L{}", bb_id.index() + self.cur_bb_id_base)
    }
}

fn byte2word(byte: usize) -> &'static str {
    match byte {
        4 => "dword",
        8 => "qword",
        _ => unimplemented!(),
    }
}
