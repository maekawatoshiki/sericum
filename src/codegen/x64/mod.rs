pub mod asm;
pub mod dag;
pub mod exec;
pub mod frame_object;
pub mod inst;
pub mod machine;
pub mod register;

use crate::ir::types::*;

impl TypeSize for Type {
    fn size_in_byte(&self, tys: &Types) -> usize {
        match self {
            Type::Int1 => 1,
            Type::Int8 => 1,
            Type::Int32 => 4,
            Type::Int64 => 8,
            Type::F64 => 8,
            Type::Array(id) => tys.base.borrow().non_primitive_types[*id]
                .as_array()
                .size_in_byte(tys),
            Type::Struct(id) => tys.base.borrow().non_primitive_types[*id]
                .as_struct()
                .size_in_byte(tys),
            Type::Pointer(_) => 8,
            Type::Function(_) => unimplemented!(),
            Type::Void => 0,
        }
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}

impl TypeSize for ArrayType {
    fn size_in_byte(&self, tys: &Types) -> usize {
        self.elem_ty.size_in_byte(tys) * self.len
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}

impl TypeSize for StructType {
    fn size_in_byte(&self, tys: &Types) -> usize {
        let mut size_total = 0;
        let calc_padding = |off, align| -> usize {
            if off % align == 0 {
                0
            } else {
                align - off % align
            }
        };
        for ty in &self.fields_ty {
            size_total += {
                let size = ty.size_in_byte(tys);
                size + calc_padding(size_total, size)
            };
        }
        size_total
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}
