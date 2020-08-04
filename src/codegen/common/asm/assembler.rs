use crate::codegen::common::machine::{
    basic_block::MachineBasicBlock, function::MachineFunction, inst::MachineInst,
    module::MachineModule,
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
    stream: InstructionStream,
}

pub struct InstAssembler<'a> {
    module: &'a MachineModule,
    function: &'a MachineFunction,
    block: &'a MachineBasicBlock,
    inst: &'a MachineInst,
    stream: &'a mut InstructionStream,
}

pub struct InstructionStream {
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
        for (block_id, block) in self.function.body.basic_blocks.id_and_block() {
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
            stream: InstructionStream::new(),
        }
    }

    pub fn assemble(&mut self) {
        for &id in &*self.block.iseq_ref() {
            let inst = &self.function.body.inst_arena[id];
            let inst_asmer = InstAssembler::new(
                self.module,
                self.function,
                self.block,
                &mut self.stream,
                inst,
            );
        }
    }
}

impl<'a> InstAssembler<'a> {
    pub fn new(
        module: &'a MachineModule,
        function: &'a MachineFunction,
        block: &'a MachineBasicBlock,
        stream: &'a mut InstructionStream,
        inst: &'a MachineInst,
    ) -> Self {
        Self {
            module,
            function,
            block,
            inst,
            stream,
        }
    }
}

impl InstructionStream {
    pub fn new() -> Self {
        Self { bytes: vec![] }
    }
}
