use super::machine::function::MachineFunction;
use crate::ir::types::*;
use rustc_hash::FxHashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub struct LocalVariableManager {
    pub locals: Vec<FrameIndexInfo>,
    pub cur_idx: usize,
}

#[derive(Debug)]
pub struct FrameObjectsInfo {
    offset_map: FxHashMap<i32, usize>, // frame index -> offset
    total_size: usize,
}

impl LocalVariableManager {
    pub fn new() -> Self {
        Self {
            locals: vec![],
            cur_idx: 1,
        }
    }

    pub fn alloc(&mut self, ty: &Type) -> FrameIndexInfo {
        let info = FrameIndexInfo::new(ty.clone(), self.cur_idx as i32);
        self.cur_idx += 1;
        self.locals.push(info.clone());
        info
    }
}

impl FrameObjectsInfo {
    pub fn new(f: &MachineFunction) -> Self {
        let mut offset_map = FxHashMap::default();
        let mut offset = 0;

        for (i, param_ty) in f.ty.get_function_ty().unwrap().params_ty.iter().enumerate() {
            offset += param_ty.size_in_byte();
            offset_map.insert(-(i as i32 + 1), offset);
        }

        for FrameIndexInfo { idx, ty } in &f.local_mgr.locals {
            offset += ty.size_in_byte();
            offset_map.insert(*idx, offset);
        }

        Self {
            offset_map,
            total_size: offset,
        }
    }

    pub fn offset(&self, frame_index: i32) -> Option<i32> {
        self.offset_map.get(&frame_index).map(|x| *x as i32)
    }

    pub fn total_size(&self) -> i32 {
        self.total_size as i32
    }
}

#[derive(Clone)]
pub struct FrameIndexInfo {
    pub ty: Type,
    pub idx: i32,
}

impl FrameIndexInfo {
    pub fn new(ty: Type, idx: i32) -> Self {
        Self { ty, idx }
    }
}

impl fmt::Debug for FrameIndexInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fi<{:?}, {}>", self.ty, self.idx)
    }
}
