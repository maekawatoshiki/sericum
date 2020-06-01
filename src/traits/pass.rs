use std::fmt::Debug;

pub trait ModulePassTrait {
    type M: Debug;
    fn name(&self) -> &'static str;
    fn run_on_module(&mut self, module: &mut Self::M);
}

pub struct ModulePassManager<M: Debug> {
    pub list: Vec<Box<dyn ModulePassTrait<M = M>>>,
}

impl<M: Debug> ModulePassManager<M> {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn run_on_module(&mut self, module: &mut M) {
        for pass in &mut self.list {
            let now = ::std::time::Instant::now();
            pass.run_on_module(module);
            debug!(println!(
                "after pass '{}': {:?}",
                pass.name(),
                ::std::time::Instant::now().duration_since(now)
            ));
        }
    }

    pub fn add_pass<A: 'static + ModulePassTrait<M = M>>(&mut self, pass: A) {
        self.list.push(Box::new(pass))
    }
}
