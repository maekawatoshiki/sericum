use super::module::*;
use crate::util::allocator::*;

pub struct Context {
    module_alloc: RawAllocator<Module>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            module_alloc: RawAllocator::new(),
        }
    }

    pub fn create_module(&mut self, name: &str) -> ModuleRef {
        let module = Module::new(name);
        ModuleRef(self.module_alloc.alloc(module))
    }
}
