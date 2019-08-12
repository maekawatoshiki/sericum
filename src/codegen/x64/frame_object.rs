use crate::ir::types::*;
use std::fmt;

#[derive(Debug, Clone)]
pub struct LocalVariableManager {
    pub locals: Vec<FrameIndexInfo>,
    pub cur_idx: usize,
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

    // pub fn add_frame_index_info(&mut self, info:FrameIndexInfo) {
    //     self.locals
    // }
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
