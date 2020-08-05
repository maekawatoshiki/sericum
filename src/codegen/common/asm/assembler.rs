use crate::codegen::common::machine::{
    basic_block::MachineBasicBlock, function::MachineFunction, module::MachineModule,
};

pub struct Assembler<'a> {
    module: &'a MachineModule,
}

pub struct FunctionAssembler<'a> {
    module: &'a MachineModule,
    function: &'a MachineFunction,
}

pub struct BlockAssembler<'a> {
    module: &'a MachineModule,
    function: &'a MachineFunction,
    block: &'a MachineBasicBlock,
    bytes: Vec<u8>,
}

impl<'a> Assembler<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self { module }
    }

    pub fn assemble(&mut self) {
        for (_id, func) in &self.module.functions {
            let func_asmer = FunctionAssembler::new(self.module, func);
        }
    }
}

impl<'a> FunctionAssembler<'a> {
    pub fn new(module: &'a MachineModule, function: &'a MachineFunction) -> Self {
        Self { module, function }
    }

    pub fn assemble(&mut self) {
        for (block_id, block, iseq) in self.function.body.mbb_iter() {
            let block_asmer = BlockAssembler::new(self.module, self.function, block);
        }
    }
}

impl<'a> BlockAssembler<'a> {
    pub fn new(
        module: &'a MachineModule,
        function: &'a MachineFunction,
        block: &'a MachineBasicBlock,
    ) -> Self {
        Self {
            module,
            function,
            block,
            bytes: vec![],
        }
    }
}
