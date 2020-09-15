use crate::codegen::common::asm::assembler::FunctionAssembler;
use crate::codegen::common::machine::{function::MachineFunctionId, module::MachineModule};
use mmap::{MapOption, MemoryMap};

pub struct Executor {
    module: MachineModule,
}

pub struct Assembler<'a> {
    module: &'a MachineModule,
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
        Self { module }
    }

    pub fn assemble(&mut self) {
        for (_, func) in &self.module.functions {
            let mut func_asmer = FunctionAssembler::new(self.module, func);
            func_asmer.assemble();

            let mem = MemoryMap::new(
                func_asmer.stream.data().len(),
                &[
                    MapOption::MapReadable,
                    MapOption::MapWritable,
                    MapOption::MapExecutable,
                ],
            )
            .unwrap();

            unsafe {
                ::std::ptr::copy(
                    func_asmer.stream.data().as_ptr() as *const u8,
                    mem.data(),
                    func_asmer.stream.data().len(),
                );
            }

            let f = unsafe { ::std::mem::transmute::<*mut u8, fn(i32) -> i32>(mem.data()) };
            println!(">>> {}", f(10));
        }
    }
}
