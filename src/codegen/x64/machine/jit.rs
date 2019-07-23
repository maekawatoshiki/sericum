use super::{function::*, module::*};

pub struct JITCompiler<'a> {
    module: &'a MachineModule,
}

impl<'a> JITCompiler<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self { module }
    }

    pub fn compile_module(&mut self) {
        for (f_id, _) in &self.module.functions {
            self.compile_function(f_id);
        }
    }

    fn compile_function(&mut self, id: MachineFunctionId) {}
}
