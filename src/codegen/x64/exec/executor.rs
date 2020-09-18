use crate::codegen::common::asm::assembler::{FunctionAssembler, InstructionStream, Label, Labels};
use crate::codegen::common::machine::{function::MachineFunctionId, module::MachineModule};
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
        let mut global_offset = 0;
        let mut func_offset = FxHashMap::default();

        for (id, func) in &self.module.functions {
            func_offset.insert(id, global_offset);
            let mut func_asmer = FunctionAssembler::new(self.module, func, &mut self.labels);
            func_asmer.assemble();
            global_offset += func_asmer.stream.data().len();
            stream.append(&mut func_asmer.stream.data().clone());
        }

        for (off, label) in &self.labels.replace_disp32 {
            let insert_pt = func_offset[&off.func_id()] + off.offset();
            let label = self.labels.arena[*label].as_func_offset();
            let label = func_offset[&label.func_id()] + label.offset();
            let x = (label as i32 - (insert_pt as i32 + 4)) as u32;
            stream[insert_pt + 0] = (x & 0x000000ff) as u8;
            stream[insert_pt + 1] = ((x & 0x0000ff00) >> 8) as u8;
            stream[insert_pt + 2] = ((x & 0x00ff0000) >> 16) as u8;
            stream[insert_pt + 3] = ((x & 0xff000000) >> 24) as u8;
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
