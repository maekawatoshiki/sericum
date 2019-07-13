pub mod dead_code_elimination;

use crate::ir::module::Module;

pub trait Pass {
    fn run(&mut self, m: &Module);
}

pub struct PassManager {
    pass_list: Vec<Box<dyn Pass>>,
    pass_list_changed: bool,
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            pass_list: vec![],
            pass_list_changed: false,
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.pass_list.push(pass);
        self.pass_list_changed = true;
    }

    pub fn run_as_necessary(&mut self, module: &Module) {
        if !self.pass_list_changed {
            return;
        }

        for pass in &mut self.pass_list {
            pass.run(module)
        }

        self.pass_list_changed = false;
    }
}
