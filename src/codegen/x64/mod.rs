pub mod dag;
pub mod exec;
pub mod frame_object;
pub mod machine;
pub mod register;

use crate::ir::types::*;

impl TypeSize for Type {
    fn size_in_byte(&self) -> usize {
        match self {
            Type::Int1 => 1,
            Type::Int32 => 4,
            Type::Int64 => 8,
            Type::F64 => 8,
            Type::Array(arrty) => arrty.size_in_byte(),
            Type::Pointer(_) => 8,
            Type::Function(_) => unimplemented!(),
            Type::Void => 0,
        }
    }

    fn size_in_bits(&self) -> usize {
        self.size_in_byte() * 8
    }
}

impl TypeSize for ArrayType {
    fn size_in_byte(&self) -> usize {
        self.elem_ty.size_in_byte() * self.len
    }

    fn size_in_bits(&self) -> usize {
        self.size_in_byte() * 8
    }
}
