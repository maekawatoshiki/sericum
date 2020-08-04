use crate::codegen::common::machine::{
    basic_block::MachineBasicBlock, function::MachineFunction, inst::MachineInst,
    module::MachineModule,
};
use faerie::*;
use std::str::FromStr;
use std::{fmt, fs::File, path::Path};

pub struct Assembler<'a> {
    module: &'a MachineModule,
    artifact: Artifact,
}

pub struct FunctionAssembler<'a> {
    module: &'a MachineModule,
    function: &'a MachineFunction,
    stream: InstructionStream,
}

pub struct BlockAssembler<'a> {
    module: &'a MachineModule,
    function: &'a MachineFunction,
    block: &'a MachineBasicBlock,
    pub stream: InstructionStream,
}

pub struct InstAssembler<'a> {
    pub module: &'a MachineModule,
    pub function: &'a MachineFunction,
    pub block: &'a MachineBasicBlock,
    pub inst: &'a MachineInst,
    pub stream: &'a mut InstructionStream,
}

pub struct InstructionStream {
    bytes: Vec<u8>,
}

impl<'a> Assembler<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self {
            module,
            artifact: ArtifactBuilder::new(triple!("x86_64-unknown-unknown-unknown-elf"))
                .name(module.name.to_owned())
                .finish(),
        }
    }

    pub fn assemble(&mut self) {
        for (_, func) in &self.module.functions {
            self.artifact
                .declare(&func.name, Decl::function().global())
                .unwrap();
        }

        for (_id, func) in &self.module.functions {
            let mut func_asmer = FunctionAssembler::new(self.module, func);
            func_asmer.assemble();
            self.artifact
                .define(func.name.as_str(), func_asmer.stream.bytes)
                .unwrap();
        }
    }

    pub fn write_to_file(&mut self, name: &str) {
        let file = File::create(Path::new(name)).unwrap();
        self.artifact.write(file).unwrap();
    }
}

impl<'a> FunctionAssembler<'a> {
    pub fn new(module: &'a MachineModule, function: &'a MachineFunction) -> Self {
        Self {
            module,
            function,
            stream: InstructionStream::new(),
        }
    }

    pub fn assemble(&mut self) {
        for (_block_id, block) in self.function.body.basic_blocks.id_and_block() {
            let mut block_asmer = BlockAssembler::new(self.module, self.function, block);
            block_asmer.assemble();
            self.stream.append(&mut block_asmer.stream);
        }

        println!("inst stream: {:?}", self.stream);
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
            let mut inst_asmer = InstAssembler::new(
                self.module,
                self.function,
                self.block,
                &mut self.stream,
                inst,
            );
            inst_asmer.assemble();
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

    // pub fn assemble(&mut self) {
    //     debug!(println!("{:?}", self.inst));
    // }
}

impl InstructionStream {
    pub fn new() -> Self {
        Self { bytes: vec![] }
    }

    pub fn push_u8(&mut self, u: u8) {
        self.bytes.push(u)
    }

    pub fn push_little_u32(&mut self, u: u32) {
        self.bytes.push((u & 0x000000ff) as u8);
        self.bytes.push(((u & 0x0000ff00) >> 8) as u8);
        self.bytes.push(((u & 0x00ff0000) >> 16) as u8);
        self.bytes.push(((u & 0xff000000) >> 24) as u8);
    }

    pub fn append(&mut self, x: &mut Self) {
        self.bytes.append(&mut x.bytes)
    }
}

impl fmt::Debug for InstructionStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for &byte in &self.bytes {
            write!(f, "{:02x}, ", byte)?
        }
        write!(f, "]")
    }
}
