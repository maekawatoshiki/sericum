use super::{function::*, liveness::*, module::*};
// use crate::ir::types::*;

pub struct RegisterCoalescer {}

impl RegisterCoalescer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, f) in &mut module.functions {
            self.run_on_function(f);
        }
    }

    pub fn run_on_function(&mut self, f: &mut MachineFunction) {
        let _matrix = LivenessAnalysis::new().analyze_function(f);
    }
}
