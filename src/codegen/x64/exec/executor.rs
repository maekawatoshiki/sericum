use crate::codegen::common::asm::assembler::{FunctionAssembler, InstructionStream, Labels};
use crate::codegen::common::machine::{function::MachineFunctionId, module::MachineModule};
use mmap::{MapOption, MemoryMap};
use rustc_hash::FxHashMap;

pub struct Executor {
    asm: Assembler,
}

pub struct Assembler {
    pub module: MachineModule,
    labels: Labels,
    func_global_offset: FxHashMap<MachineFunctionId, usize>,
    stream: InstructionStream,
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

    pub fn execute(&mut self) {
        self.asm.execute()
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

    pub fn execute(&mut self) {
        let id = self.module.find_function_by_name("main").unwrap();
        let off = self.func_global_offset[&id];

        let mem = MemoryMap::new(
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
                mem.data(),
                self.stream.data().len(),
            );
        }

        let f = unsafe { ::std::mem::transmute::<*mut u8, fn(i32) -> i32>(mem.data().add(off)) };
        println!(">>> {}", f(1));
    }
}
