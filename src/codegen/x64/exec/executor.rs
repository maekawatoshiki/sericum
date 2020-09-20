use crate::codegen::arch::asm::assembler::{mod_rm, Mod};
use crate::codegen::arch::machine::register::RegisterClassKind;
use crate::codegen::common::asm::assembler::{FunctionAssembler, InstructionStream, Labels};
use crate::codegen::common::machine::{function::MachineFunctionId, module::MachineModule};
use crate::codegen::{
    arch::machine::abi::SystemV, common::machine::calling_conv::ArgumentRegisterOrder,
};
use crate::ir::types::Type;
use mmap::{MapOption, MemoryMap};
use rustc_hash::FxHashMap;

pub struct Executor {
    asm: Assembler,
}

pub struct Assembler {
    pub module: MachineModule,
    labels: Labels,
    pub func_global_offset: FxHashMap<MachineFunctionId, usize>,
    stream: InstructionStream,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum GenericValue {
    i32(i32),
    None,
}

impl Executor {
    pub fn new(module: MachineModule) -> Self {
        Self {
            asm: Assembler::new(module),
        }
    }

    pub fn find_function_by_name(&self, name: &str) -> Option<MachineFunctionId> {
        self.asm.module.find_function_by_name(name)
    }

    pub fn compile(&mut self) {
        self.asm.assemble();
    }

    pub fn execute(&mut self, id: MachineFunctionId, args: Vec<GenericValue>) -> GenericValue {
        self.asm.execute(id, args)
    }
}

impl Assembler {
    pub fn new(module: MachineModule) -> Self {
        Self {
            module,
            labels: Labels::new(),
            stream: InstructionStream::new(),
            func_global_offset: FxHashMap::default(),
        }
    }

    pub fn assemble(&mut self) {
        let mut global_offset = 0;

        for (id, func) in &self.module.functions {
            self.func_global_offset.insert(id, global_offset);
            let mut func_asmer = FunctionAssembler::new(&self.module, func, &mut self.labels);
            func_asmer.assemble();
            global_offset += func_asmer.stream.data().len();
            self.stream.append(&mut func_asmer.stream.clone())
        }

        for (off, label) in &self.labels.replace_disp32 {
            let insert_pt = self.func_global_offset[&off.func_id()] + off.offset();
            let label = self.labels.arena[*label].as_func_offset();
            let label = self.func_global_offset[&label.func_id()] + label.offset();
            self.stream
                .insert_u32_le(insert_pt, (label as i32 - (insert_pt as i32 + 4)) as u32);
        }
    }

    pub fn execute(&mut self, func_id: MachineFunctionId, args: Vec<GenericValue>) -> GenericValue {
        let abi = SystemV::new();
        let mut arg_reg_order = ArgumentRegisterOrder::new(&abi);

        let start = self.stream.data().len();

        for (_idx, arg) in args.iter().enumerate() {
            match arg {
                GenericValue::i32(i) => {
                    let r = arg_reg_order.next(RegisterClassKind::GR32).unwrap();
                    // mov r, i
                    self.stream.push_u8(0xc7);
                    self.stream.push_u8(mod_rm(
                        Mod::Reg,
                        0,
                        r.retrieve() as u8 - r.reg_class() as u8,
                    ));
                    self.stream.push_u32_le(*i as u32);
                }
                GenericValue::None => unreachable!(),
            }
        }

        let func_off = self.func_global_offset[&func_id];

        // sub rsp, 8
        self.stream.push_u8(0x48);
        self.stream.push_u8(0x83);
        self.stream.push_u8(0xec);
        self.stream.push_u8(8);
        // call
        self.stream.push_u8(0xe8);
        self.stream
            .push_u32_le((func_off as i32 - (self.stream.data().len() as i32 + 4)) as u32);
        // add rsp, 8
        self.stream.push_u8(0x48);
        self.stream.push_u8(0x83);
        self.stream.push_u8(0xc4);
        self.stream.push_u8(8);
        // ret
        self.stream.push_u8(0xc3);

        // TODO: Allocating memory every time is inefficient
        let memory = MemoryMap::new(
            self.stream.data().len(),
            &[
                MapOption::MapReadable,
                MapOption::MapWritable,
                MapOption::MapExecutable,
            ],
        )
        .unwrap();

        unsafe {
            ::std::ptr::copy(
                self.stream.data().as_ptr() as *const u8,
                memory.data(),
                self.stream.data().len(),
            );
        }

        match self
            .module
            .types
            .base
            .borrow()
            .as_function_ty(self.module.function_ref(func_id).ty)
            .unwrap()
            .ret_ty
        {
            Type::i32 => GenericValue::i32(unsafe {
                ::std::mem::transmute::<*mut u8, fn() -> i32>(memory.data().add(start))
            }()),
            Type::Void => GenericValue::None,
            _ => unimplemented!(),
        }
    }
}
