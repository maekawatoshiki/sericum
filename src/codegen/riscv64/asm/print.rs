use super::super::frame_object::FrameObjectsInfo;
use super::super::machine::inst::*;
use crate::codegen::common::machine::{
    basic_block::MachineBasicBlockId,
    function::{InstIter, MachineFunction},
    module::MachineModule,
};
use crate::ir::types::Types;

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

        self.run_on_basic_blocks(f, &fo);
    }

    fn run_on_basic_blocks(
        &mut self,
        // tys: &Types,
        f: &MachineFunction,
        fo: &FrameObjectsInfo,
    ) {
        for (id, _, inst_iter) in f.body.mbb_iter() {
            self.output
                .push_str(format!("{}:\n", self.bb_id_to_label_id(&id)).as_str());
            self.run_on_basic_block(inst_iter, fo);
        }
        self.cur_bb_id_base += f.body.basic_blocks.order.len();
    }

    fn run_on_basic_block<'a>(
        &mut self,
        // tys: &Types,
        // regs_info: &RegistersInfo,
        inst_iter: InstIter<'a>,
        fo: &FrameObjectsInfo,
    ) {
        for (_, inst) in inst_iter {
            self.run_on_inst(inst, fo);
        }
    }

    fn run_on_inst(
        &mut self,
        // tys: &Types,
        // regs_info: &RegistersInfo,
        inst: &MachineInst,
        fo: &FrameObjectsInfo,
    ) {
        self.output.push_str("  ");

        self.output.push_str(inst.opcode.inst_def().unwrap().name);
        self.output.push(' ');

        for (i, r) in inst.def.iter().enumerate() {
            self.output.push_str(r.as_phys_reg().name());
            if i != inst.def.len() - 1 {
                self.output.push_str(", ");
            }
        }

        if inst.def.len() > 0 && inst.operand.len() > 0 {
            self.output.push_str(", ");
        }

        for (i, o) in inst.operand.iter().enumerate() {
            self.operand2asm(fo, o);
            if i != inst.operand.len() - 1 {
                self.output.push_str(", ");
            }
        }

        self.output.push('\n');
    }

    fn bb_id_to_label_id(&self, bb_id: &MachineBasicBlockId) -> String {
        format!(".L{}", bb_id.index() + self.cur_bb_id_base)
    }

    fn operand2asm(&mut self, fo: &FrameObjectsInfo, operand: &MachineOperand) {
        match operand {
            MachineOperand::Branch(id) => self.output.push_str(self.bb_id_to_label_id(id).as_str()),
            MachineOperand::Constant(MachineConstant::Int32(i)) => {
                self.output.push_str(format!("{}", i).as_str())
            }
            MachineOperand::Register(r) => self.output.push_str(r.as_phys_reg().name()),
            MachineOperand::FrameIndex(i) => self
                .output
                .push_str(format!("{}", fo.offset(i.idx).unwrap()).as_str()),
            MachineOperand::Mem(MachineMemOperand::FiReg(fi, r)) => self.output.push_str(
                format!("{}({})", fo.offset(fi.idx).unwrap(), r.as_phys_reg().name()).as_str(),
            ),
            MachineOperand::Mem(MachineMemOperand::ImmReg(imm, r)) => self
                .output
                .push_str(format!("{}({})", imm, r.as_phys_reg().name()).as_str()),
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(name))) => {
                self.output.push_str(name.as_str())
            }
            _ => unimplemented!(),
        };
    }
}
