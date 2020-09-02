use crate::codegen::arch::machine::inst::MachineMemOperand;
use crate::codegen::common::machine::eliminate_fi::EliminateFrameIndexOnFunction;
use crate::codegen::common::machine::frame_object::{FrameIndexInfo, FrameObjectsInfo};
use crate::codegen::common::machine::inst::{MachineInst, MachineOperand};

impl<'a> EliminateFrameIndexOnFunction<'a> {
    pub fn get_frame_index(inst: &MachineInst) -> Option<&FrameIndexInfo> {
        for op in &inst.operand {
            match op {
                MachineOperand::Mem(MachineMemOperand::BaseFi(_, fi))
                | MachineOperand::Mem(MachineMemOperand::BaseFiOff(_, fi, _))
                | MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(_, fi, _, _))
                | MachineOperand::Mem(MachineMemOperand::BaseFiAlignOffOff(_, fi, _, _, _)) => {
                    return Some(fi)
                }
                _ => {}
            }
        }
        None
    }

    pub fn replace_frame_index(frame_objects: &FrameObjectsInfo, inst: &mut MachineInst) {
        for op in &mut inst.operand {
            match op {
                MachineOperand::Mem(MachineMemOperand::BaseFi(base, fi)) => {
                    *op = MachineOperand::Mem(MachineMemOperand::BaseOff(
                        *base,
                        frame_objects.offset(fi.idx).unwrap(),
                    ))
                }
                MachineOperand::Mem(MachineMemOperand::BaseFiOff(base, fi, off)) => {
                    *op = MachineOperand::Mem(MachineMemOperand::BaseOff(
                        *base,
                        frame_objects.offset(fi.idx).unwrap() + *off,
                    ))
                }
                MachineOperand::Mem(MachineMemOperand::BaseFiAlignOff(base, fi, align, off)) => {
                    *op = MachineOperand::Mem(MachineMemOperand::BaseOffAlignOff(
                        *base,
                        frame_objects.offset(fi.idx).unwrap(),
                        *align,
                        *off,
                    ));
                }
                MachineOperand::Mem(MachineMemOperand::BaseFiAlignOffOff(
                    base,
                    fi,
                    align,
                    off,
                    off2,
                )) => {
                    *op = MachineOperand::Mem(MachineMemOperand::BaseOffAlignOff(
                        *base,
                        frame_objects.offset(fi.idx).unwrap() + *off2,
                        *align,
                        *off,
                    ));
                }
                _ => {}
            }
        }
    }
}
