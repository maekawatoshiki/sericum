use super::{function::*, module::*};
use crate::ir::types::*;
use dynasmrt::*;
use rustc_hash::FxHashMap;

pub struct JITCompiler<'a> {
    module: &'a MachineModule,
    asm: x64::Assembler,
    function_map: FxHashMap<MachineFunctionId, DynamicLabel>,
}

#[derive(Debug)]
pub struct FrameObjectsInfo {
    offset_map: FxHashMap<i32, usize>, // frame index -> offset
}

impl<'a> JITCompiler<'a> {
    pub fn new(module: &'a MachineModule) -> Self {
        Self {
            module,
            asm: x64::Assembler::new().unwrap(),
            function_map: FxHashMap::default(),
        }
    }

    pub fn compile_module(&mut self) {
        for (f_id, _) in &self.module.functions {
            self.compile_function(f_id);
        }
    }

    fn compile_function(&mut self, id: MachineFunctionId) {
        let f = self.module.function_ref(id);
        let f_entry = self.get_function_entry_label(id);
    }

    fn get_function_entry_label(&mut self, f_id: MachineFunctionId) -> DynamicLabel {
        if self.function_map.contains_key(&f_id) {
            return *self.function_map.get(&f_id).unwrap();
        }

        let f_entry = self.asm.new_dynamic_label();
        self.function_map.insert(f_id, f_entry);
        f_entry
    }
}

impl FrameObjectsInfo {
    pub fn new(f: &MachineFunction) -> Self {
        Self {
            offset_map: {
                let mut offset_map = FxHashMap::default();
                let mut offset = 0;
                for (i, param_ty) in f.ty.get_function_ty().unwrap().params_ty.iter().enumerate() {
                    offset += param_ty.size_in_byte();
                    offset_map.insert(-(i as i32 + 1), offset);
                }
                for (i, param_ty) in f.locals_ty.iter().enumerate() {
                    offset += param_ty.size_in_byte();
                    offset_map.insert(i as i32 + 1, offset);
                }
                offset_map
            },
        }
    }

    pub fn offset(&self, frame_index: i32) -> Option<usize> {
        self.offset_map.get(&frame_index).map(|x| *x)
    }
}

fn roundup(n: i32, align: i32) -> i32 {
    (n + align - 1) & !(align - 1)
}
