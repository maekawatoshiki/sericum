use crate::codegen::common::asm::assembler::{FunctionAssembler, InstructionStream, Label, Labels};
use crate::codegen::common::machine::{function::MachineFunctionId, module::MachineModule};
use id_arena::Arena;
use mmap::{MapOption, MemoryMap};
use rustc_hash::FxHashMap;

pub struct Executor {
    module: MachineModule,
}

pub struct Assembler<'a> {
    module: &'a MachineModule,
    labels: Labels,
}

impl Executor {
    pub fn new(module: MachineModule) -> Self {
        Self { module }
    }

    pub fn find_function_by_name(&self, name: &str) -> Option<MachineFunctionId> {
        self.module.find_function_by_name(name)
    }

    pub fn compile(&mut self) {
        let mut asm = Assembler::new(&self.module);
        asm.assemble();
    }

    pub fn execute(&mut self, _id: MachineFunctionId, _args: Vec<()>) {
        unimplemented!()
    }
}

impl<'a> Assembler<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self {
            module,
            labels: Labels::new(),
        }
    }

    pub fn assemble(&mut self) {
        let mut stream = vec![];
        let mut offset = 0;
        let mut func_offset = FxHashMap::default();
        for (id, func) in &self.module.functions {
            func_offset.insert(id, offset);
            let mut func_asmer = FunctionAssembler::new(self.module, func, &mut self.labels);
            func_asmer.assemble();
            offset += func_asmer.stream.data().len();
            stream.append(&mut func_asmer.stream.data().clone());
        }

        for (func, label, off) in &self.labels.replace_disp32 {
            let offset = func_offset[func] + *off;
            let label = &self.labels.arena[*label];
            let label = func_offset[&label.func_id] + label.offset;
            let u = label as i32 - (offset as i32 + 4);
            println!("{}", u);
            let u = u as u32;
            println!("{}", u);
            stream[offset + 0] = (u & 0x000000ff) as u8;
            stream[offset + 1] = ((u & 0x0000ff00) >> 8) as u8;
            stream[offset + 2] = ((u & 0x00ff0000) >> 16) as u8;
            stream[offset + 3] = ((u & 0xff000000) >> 24) as u8;
        }

        let id = self.module.find_function_by_name("main").unwrap();
        let off = func_offset[&id];

        let mem = MemoryMap::new(
            stream.len(),
            &[
                MapOption::MapReadable,
                MapOption::MapWritable,
                MapOption::MapExecutable,
            ],
        )
        .unwrap();

        unsafe {
            ::std::ptr::copy(stream.as_ptr() as *const u8, mem.data(), stream.len());
        }

        let f = unsafe { ::std::mem::transmute::<*mut u8, fn(i32) -> i32>(mem.data().add(off)) };
        println!(">>> {}", f(10));
    }
}
