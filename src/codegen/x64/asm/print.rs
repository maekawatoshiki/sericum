use super::super::frame_object::FrameObjectsInfo;
use super::super::machine::inst::*;
use crate::codegen::common::machine::{
    basic_block::MachineBasicBlockId,
    const_data::DataId,
    function::{InstIter, MachineFunction},
    module::MachineModule,
};
use crate::ir::{global_val::GlobalVariableId, types::TypeSize};
use rustc_hash::FxHashMap;

pub struct MachineAsmPrinter {
    pub output: String,
    cur_bb_id_base: usize,
    id_to_global_name: FxHashMap<GlobalVariableId, String>,
}

impl MachineAsmPrinter {
    pub fn new() -> Self {
        Self {
            output: "".to_string(),
            cur_bb_id_base: 0,
            id_to_global_name: FxHashMap::default(),
        }
    }

    pub fn run_on_module(&mut self, m: &MachineModule) {
        self.output.push_str("  .text\n");
        self.output.push_str("  .intel_syntax noprefix\n");

        for (id, g) in &m.global_vars.arena {
            let size = g.ty.size_in_byte(&m.types);
            let align = g.ty.align_in_byte(&m.types);
            self.output
                .push_str(format!("  .comm {},{},{}\n", g.name, size, align).as_str());
            self.id_to_global_name.insert(id, g.name.clone());
        }

        for (_, func) in &m.functions {
            self.run_on_function(&func)
        }
    }

    fn run_on_function(&mut self, f: &MachineFunction) {
        if f.is_internal || f.is_empty() {
            return;
        }

        for (id, data) in f.const_data.id_and_data() {
            self.output
                .push_str(format!(".Lconst{}{}:\n", id.arena_id(), id.id()).as_str());
            self.output.push_str(
                format!("  .quad {}\n", unsafe {
                    ::std::mem::transmute::<f64, u64>(data.as_f64())
                })
                .as_str(),
            )
        }

        self.output
            .push_str(format!("  .globl {}\n", f.name).as_str()); // TODO

        self.output.push_str(format!("{}:\n", f.name).as_str());

        self.run_on_basic_blocks(f, f.frame_objects.as_ref().unwrap());
    }

    fn run_on_basic_blocks(&mut self, f: &MachineFunction, fo: &FrameObjectsInfo) {
        for (id, _, inst_iter) in f.body.mbb_iter() {
            self.output
                .push_str(format!("{}:\n", self.bb_id_to_label_id(&id)).as_str());
            self.run_on_basic_block(inst_iter, fo);
        }
        self.cur_bb_id_base += f.body.basic_blocks.arena.len();
    }

    fn run_on_basic_block<'a>(&mut self, inst_iter: InstIter<'a>, fo: &FrameObjectsInfo) {
        for (_, inst) in inst_iter {
            self.run_on_inst(inst, fo);
        }
    }

    fn run_on_inst(&mut self, inst: &MachineInst, fo: &FrameObjectsInfo) {
        self.output.push_str("  ");

        // println!("{:?}", inst.opcode);
        let inst_def = inst.opcode.inst_def().unwrap();

        self.output.push_str(inst_def.name);
        self.output.push(' ');

        for (i, r) in inst.def.iter().enumerate() {
            self.output.push_str(r.id.as_phys_reg().name());
            if i != inst.def.len() - 1 {
                self.output.push_str(", ");
            }
        }

        if inst.def.len() > 0 && inst.operand.len() > 0 {
            self.output.push_str(", ");
        }

        for (i, o) in inst.operand.iter().enumerate().filter(|(i, _)| {
            inst_def
                .tie
                .iter()
                .position(|(_, u)| &u.as_use() == i)
                .is_none()
        }) {
            self.operand2asm(&inst.opcode, fo, o);
            if i != inst.operand.len() - 1 {
                self.output.push_str(", ");
            }
        }

        self.output.push('\n');
    }

    fn operand2asm(
        &mut self,
        opcode: &MachineOpcode,
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
            MachineOperand::Register(r) => self.output.push_str(r.id.as_phys_reg().name()),
            MachineOperand::FrameIndex(i) => self
                .output
                .push_str(format!("{}", fo.offset(i.idx).unwrap()).as_str()),
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::FunctionName(name))) => {
                self.output.push_str(name.replace('.', "_").as_str())
            }
            MachineOperand::Mem(_) => {
                let word = opcode2word(opcode);
                self.run_on_mem_operand(operand, fo, word)
            }
            e => unimplemented!("{:?}", e),
        };
    }

    fn run_on_mem_operand(&mut self, op: &MachineOperand, fo: &FrameObjectsInfo, word: &str) {
        match op {
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Label(id))) => {
                self.output.push_str(self.data_id_to_label_id(id).as_str())
            }
            MachineOperand::Mem(MachineMemOperand::Address(AddressKind::Global(id))) => self
                .output
                .push_str(format!("{} ptr [{}]", word, self.global_var_name(id)).as_str()),
            MachineOperand::Mem(MachineMemOperand::AddressOff(AddressKind::Global(id), off)) => {
                self.output.push_str(
                    format!("{} ptr [{}+{}]", word, self.global_var_name(id), off).as_str(),
                )
            }
            MachineOperand::Mem(MachineMemOperand::AddressAlignOff(
                AddressKind::Global(id),
                align,
                off,
            )) => self.output.push_str(
                format!(
                    "{} ptr [{}+{}*{}]",
                    word,
                    self.global_var_name(id),
                    align,
                    off.id.as_phys_reg().name()
                )
                .as_str(),
            ),
            MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                let base = base.id.as_phys_reg();
                let offset = fo.offset(fi.idx).unwrap();
                self.output
                    .push_str(format!("{} ptr [{}{}]", word, base.name(), off_s(offset)).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::BaseAlignOff(base, align, off)) => {
                let base = base.id.as_phys_reg();
                let reg = off.id.as_phys_reg();
                self.output.push_str(
                    format!("{} ptr [{}+{}*{}]", word, base.name(), align, reg.name()).as_str(),
                );
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                let offset = fo.offset(fi.idx).unwrap();
                let base = base.id.as_phys_reg();
                let reg = off.id.as_phys_reg();
                self.output.push_str(
                    format!(
                        "{} ptr [{}+{}*{}{}]",
                        word,
                        base.name(),
                        align,
                        reg.name(),
                        off_s(offset)
                    )
                    .as_str(),
                );
            }
            MachineOperand::Mem(MachineMemOperand::BaseOffAlignOff(base, off1, align, off2)) => {
                let base = base.id.as_phys_reg();
                let reg = off2.id.as_phys_reg();
                self.output.push_str(
                    format!(
                        "{} ptr [{}+{}*{}{}]",
                        word,
                        base.name(),
                        align,
                        reg.name(),
                        off1
                    )
                    .as_str(),
                );
            }
            MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                let base = base.id.as_phys_reg();
                let off1 = fo.offset(fi.idx).unwrap();
                let off2 = *off;
                let offset = off1 + off2;
                self.output
                    .push_str(format!("{} ptr [{}{}]", word, base.name(), off_s(offset)).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::Base(base)) => {
                let base = base.id.as_phys_reg();
                self.output
                    .push_str(format!("{} ptr [{}]", word, base.name()).as_str());
            }
            MachineOperand::Mem(MachineMemOperand::BaseOff(base, off)) => {
                let base = base.id.as_phys_reg();
                self.output
                    .push_str(format!("{} ptr [{}{}]", word, base.name(), off_s(*off)).as_str());
            }
            e => panic!("{:?}", e),
        }
    }

    fn bb_id_to_label_id(&self, bb_id: &MachineBasicBlockId) -> String {
        format!(".L{}", bb_id.index() + self.cur_bb_id_base)
    }

    fn data_id_to_label_id(&self, data_id: &DataId) -> String {
        format!(
            "qword ptr [rip + .Lconst{}{}]",
            data_id.arena_id(),
            data_id.id()
        )
    }

    fn global_var_name(&self, id: &GlobalVariableId) -> &String {
        self.id_to_global_name.get(id).unwrap()
    }
}

fn off_s(off: i32) -> String {
    if off < 0 {
        format!("-{}", -off)
    } else {
        format!("+{}", off)
    }
}

fn opcode2word(opcode: &MachineOpcode) -> &'static str {
    let byte = match opcode {
        MachineOpcode::LEAr64m
        | MachineOpcode::MOVSDrm64
        | MachineOpcode::MOVSDmr
        | MachineOpcode::MOVmr64
        | MachineOpcode::DIVSDrm
        | MachineOpcode::MOVrm64
        | MachineOpcode::MOVmi64
        | MachineOpcode::MOVSDrm => 8,
        MachineOpcode::MOVmr32
        | MachineOpcode::MOVrm32
        | MachineOpcode::MOVmi32
        | MachineOpcode::MOVSXDr64m32
        | MachineOpcode::ADDSDrm
        | MachineOpcode::SUBSDrm
        | MachineOpcode::MULSDrm
        | MachineOpcode::DIVSDrr => 4,
        MachineOpcode::MOVrm8 | MachineOpcode::MOVmr8 | MachineOpcode::MOVmi8 => 1,
        _ => 0,
    };
    match byte {
        1 => "byte",
        4 => "dword",
        8 => "qword",
        _ => "",
    }
}
