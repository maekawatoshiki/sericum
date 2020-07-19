use super::value::Value;
use id_arena::*;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone)]
pub struct Types {
    pub base: Rc<RefCell<TypesBase>>,
    // pub non_primitive_types: Arena<NonPrimitiveType>,
}

#[derive(Clone)]
pub struct TypesBase {
    pub non_primitive_types: Arena<NonPrimitiveType>,
}

pub type NonPrimitiveTypeId = Id<NonPrimitiveType>;

#[derive(Clone, Eq, PartialEq)]
pub enum NonPrimitiveType {
    Pointer(Type),
    Array(ArrayType),
    Function(FunctionType),
    Struct(StructType),
}

#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum Type {
    Void,
    Int1,
    Int8,
    Int32,
    Int64,
    F64,
    Pointer(NonPrimitiveTypeId),
    Array(NonPrimitiveTypeId),
    Function(NonPrimitiveTypeId),
    Struct(NonPrimitiveTypeId),
}

pub trait TypeSize {
    fn size_in_byte(&self, tys: &Types) -> usize;
    fn size_in_bits(&self, tys: &Types) -> usize;
    fn align_in_byte(&self, tys: &Types) -> usize;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub ret_ty: Type,
    pub params_ty: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub elem_ty: Type,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub fields_ty: Vec<Type>,
}

impl Types {
    pub fn new() -> Self {
        Self {
            base: Rc::new(RefCell::new(TypesBase::new())),
            // non_primitive_types: Arena::new(),
        }
    }

    fn new_non_primitive_ty(&mut self, t: NonPrimitiveType) -> NonPrimitiveTypeId {
        let non_primitive_types = &mut self.base.borrow_mut().non_primitive_types;
        for (id, t_) in &*non_primitive_types {
            if &t == t_ {
                return id;
            }
        }
        non_primitive_types.alloc(t)
    }

    pub fn new_pointer_ty(&mut self, elem_ty: Type) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Pointer(elem_ty));
        Type::Pointer(id)
    }

    pub fn new_array_ty(&mut self, elem_ty: Type, len: usize) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Array(ArrayType::new(elem_ty, len)));
        Type::Array(id)
    }

    pub fn new_function_ty(&mut self, ret_ty: Type, params_ty: Vec<Type>) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Function(FunctionType::new(
            ret_ty, params_ty,
        )));
        Type::Function(id)
    }

    pub fn new_struct_ty(&mut self, fields_ty: Vec<Type>) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Struct(StructType::new(fields_ty)));
        Type::Struct(id)
    }

    // pub fn as_function_ty(&self, ty: Type) -> Option<&FunctionType> {
    //     match ty {
    //         Type::Function(id) => Some(self.non_primitive_types[id].as_function()),
    //         _ => None,
    //     }
    // }

    pub fn get_element_ty(&self, ty: Type, index: Option<&Value>) -> Option<Type> {
        match ty {
            Type::Pointer(id) => Some(*self.base.borrow().non_primitive_types[id].as_pointer()),
            Type::Array(id) => Some(
                self.base.borrow().non_primitive_types[id]
                    .as_array()
                    .elem_ty,
            ),
            Type::Struct(id) => Some(
                self.base.borrow().non_primitive_types[id]
                    .as_struct()
                    .fields_ty[index.unwrap().as_imm().as_int32() as usize],
            ),
            Type::Void
            | Type::Int1
            | Type::Int8
            | Type::Int32
            | Type::Int64
            | Type::F64
            | Type::Function(_) => Some(ty),
        }
    }

    pub fn get_element_ty_with_indices(&self, ty: Type, indices: &[Value]) -> Option<Type> {
        if indices.len() == 0 {
            return Some(ty);
        }

        match ty {
            Type::Void
            | Type::Int1
            | Type::Int8
            | Type::Int32
            | Type::Int64
            | Type::F64
            | Type::Function(_) => None,
            Type::Pointer(id) => match indices.len() {
                1 => Some(*self.base.borrow().non_primitive_types[id].as_pointer()),
                _ => {
                    let elem_ty = *self.base.borrow().non_primitive_types[id].as_pointer();
                    self.get_element_ty_with_indices(elem_ty, &indices[1..])
                }
            },
            Type::Array(id) => match indices.len() {
                1 => Some(
                    self.base.borrow().non_primitive_types[id]
                        .as_array()
                        .elem_ty,
                ),
                _ => {
                    let elem_ty = self.base.borrow().non_primitive_types[id]
                        .as_array()
                        .elem_ty;
                    self.get_element_ty_with_indices(elem_ty, &indices[1..])
                }
            },
            Type::Struct(id) => match indices.len() {
                1 => Some(
                    self.base.borrow().non_primitive_types[id]
                        .as_struct()
                        .fields_ty[indices[0].as_imm().as_int32() as usize],
                ),
                _ => self.get_element_ty_with_indices(
                    self.base.borrow().non_primitive_types[id]
                        .as_struct()
                        .fields_ty[indices[0].as_imm().as_int32() as usize],
                    &indices[1..],
                ),
            },
        }
    }

    pub fn to_string(&self, ty: Type) -> String {
        self.base.borrow().to_string(ty)
        // match ty {
        //     Type::Void => "void".to_string(),
        //     Type::Int1 => "i1".to_string(),
        //     Type::Int32 => "i32".to_string(),
        //     Type::Int64 => "i64".to_string(),
        //     Type::F64 => "f64".to_string(),
        //     Type::Pointer(id) => {
        //         let elem_ty = self.base.borrow().non_primitive_types[id].as_pointer();
        //         format!("{}*", self.to_string(*elem_ty))
        //     }
        //     Type::Array(id) => {
        //         let arr = self.base.borrow().non_primitive_types[id].as_array();
        //         arr.to_string(self)
        //     }
        //     Type::Function(id) => {
        //         let f = self.base.borrow().non_primitive_types[id].as_function();
        //         f.to_string(self)
        //     }
        //     Type::Struct(id) => {
        //         let s = self.base.borrow().non_primitive_types[id].as_struct();
        //         s.to_string(self)
        //     }
        // }
    }

    // pub fn get_pointer_ty(&self) -> Type {
    //     Type::Pointer(Box::new(self.clone()))
    // }
}

impl TypesBase {
    pub fn new() -> Self {
        Self {
            non_primitive_types: Arena::new(),
        }
    }

    fn new_non_primitive_ty(&mut self, t: NonPrimitiveType) -> NonPrimitiveTypeId {
        for (id, t_) in &self.non_primitive_types {
            if &t == t_ {
                return id;
            }
        }
        self.non_primitive_types.alloc(t)
    }

    pub fn new_pointer_ty(&mut self, elem_ty: Type) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Pointer(elem_ty));
        Type::Pointer(id)
    }

    pub fn new_array_ty(&mut self, elem_ty: Type, len: usize) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Array(ArrayType::new(elem_ty, len)));
        Type::Array(id)
    }

    pub fn new_function_ty(&mut self, ret_ty: Type, params_ty: Vec<Type>) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Function(FunctionType::new(
            ret_ty, params_ty,
        )));
        Type::Function(id)
    }

    pub fn new_struct_ty(&mut self, fields_ty: Vec<Type>) -> Type {
        let id = self.new_non_primitive_ty(NonPrimitiveType::Struct(StructType::new(fields_ty)));
        Type::Struct(id)
    }

    pub fn as_function_ty(&self, ty: Type) -> Option<&FunctionType> {
        match ty {
            Type::Function(id) => Some(self.non_primitive_types[id].as_function()),
            _ => None,
        }
    }

    pub fn get_element_ty(&self, ty: Type, index: Option<&Value>) -> Option<Type> {
        match ty {
            Type::Pointer(id) => Some(*self.non_primitive_types[id].as_pointer()),
            Type::Array(id) => Some(self.non_primitive_types[id].as_array().elem_ty),
            Type::Struct(id) => Some(
                self.non_primitive_types[id].as_struct().fields_ty
                    [index.unwrap().as_imm().as_int32() as usize],
            ),
            Type::Void
            | Type::Int1
            | Type::Int8
            | Type::Int32
            | Type::Int64
            | Type::F64
            | Type::Function(_) => Some(ty),
        }
    }

    pub fn get_element_ty_with_indices(&self, ty: Type, indices: &[Value]) -> Option<Type> {
        if indices.len() == 0 {
            return Some(ty);
        }

        match ty {
            Type::Void
            | Type::Int1
            | Type::Int8
            | Type::Int32
            | Type::Int64
            | Type::F64
            | Type::Function(_) => None,
            Type::Pointer(id) => match indices.len() {
                1 => Some(*self.non_primitive_types[id].as_pointer()),
                _ => {
                    let elem_ty = self.non_primitive_types[id].as_pointer();
                    self.get_element_ty_with_indices(*elem_ty, &indices[1..])
                }
            },
            Type::Array(id) => match indices.len() {
                1 => Some(self.non_primitive_types[id].as_array().elem_ty),
                _ => {
                    let elem_ty = self.non_primitive_types[id].as_array().elem_ty;
                    self.get_element_ty_with_indices(elem_ty, &indices[1..])
                }
            },
            Type::Struct(id) => match indices.len() {
                1 => Some(
                    self.non_primitive_types[id].as_struct().fields_ty
                        [indices[0].as_imm().as_int32() as usize],
                ),
                _ => self.get_element_ty_with_indices(
                    self.non_primitive_types[id].as_struct().fields_ty
                        [indices[0].as_imm().as_int32() as usize],
                    &indices[1..],
                ),
            },
        }
    }

    pub fn to_string(&self, ty: Type) -> String {
        match ty {
            Type::Void => "void".to_string(),
            Type::Int1 => "i1".to_string(),
            Type::Int8 => "i8".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Int64 => "i64".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Pointer(id) => {
                let elem_ty = self.non_primitive_types[id].as_pointer();
                format!("{}*", self.to_string(*elem_ty))
            }
            Type::Array(id) => {
                let arr = self.non_primitive_types[id].as_array();
                arr.to_string(self)
            }
            Type::Function(id) => {
                let f = self.non_primitive_types[id].as_function();
                f.to_string(self)
            }
            Type::Struct(id) => {
                let s = self.non_primitive_types[id].as_struct();
                s.to_string(self)
            }
        }
    }

    // pub fn get_pointer_ty(&self) -> Type {
    //     Type::Pointer(Box::new(self.clone()))
    // }
}

impl Type {
    pub fn is_atomic(&self) -> bool {
        matches!(
            self,
            Self::Void | Self::Int1 | Self::Int8 | Self::Int32 | Self::Int64 | Self::F64
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Int1 | Self::Int8 | Self::Int32 | Self::Int64)
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int1 => "i1".to_string(),
            Type::Int8 => "i8".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Int64 => "i64".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Pointer(id) => format!("(ty:{})*", id.index()),
            Type::Array(id) => format!("arrty:{}", id.index()),
            Type::Function(id) => format!("functy:{}", id.index()),
            Type::Struct(id) => format!("structty:{}", id.index()),
        }
    }
}

impl FunctionType {
    pub fn new(ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Self { ret_ty, params_ty }
    }

    pub fn to_string(&self, tys: &TypesBase) -> String {
        format!(
            "{} ({})",
            tys.to_string(self.ret_ty),
            self.params_ty.iter().fold("".to_string(), |mut s, p| {
                s += &(tys.to_string(*p) + ", ");
                s
            }),
        )
    }
}

impl ArrayType {
    pub fn new(elem_ty: Type, len: usize) -> Self {
        Self { elem_ty, len }
    }

    pub fn to_string(&self, tys: &TypesBase) -> String {
        format!("[{} x {}]", self.len, tys.to_string(self.elem_ty))
    }
}

impl StructType {
    pub fn new(fields_ty: Vec<Type>) -> Self {
        Self { fields_ty }
    }

    pub fn to_string(&self, tys: &TypesBase) -> String {
        format!(
            "struct {{{}}}",
            self.fields_ty
                .iter()
                .fold("".to_string(), |mut s, t| {
                    s += &(tys.to_string(*t) + ", ");
                    s
                })
                .trim_matches(&[',', ' '][0..])
        )
    }
}

impl NonPrimitiveType {
    pub fn as_pointer(&self) -> &Type {
        match self {
            NonPrimitiveType::Pointer(p) => p,
            _ => panic!(),
        }
    }

    pub fn as_array(&self) -> &ArrayType {
        match self {
            NonPrimitiveType::Array(a) => a,
            _ => panic!(),
        }
    }

    pub fn as_function(&self) -> &FunctionType {
        match self {
            NonPrimitiveType::Function(f) => f,
            _ => panic!(),
        }
    }

    pub fn as_struct(&self) -> &StructType {
        match self {
            NonPrimitiveType::Struct(s) => s,
            _ => panic!(),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Debug for Types {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        fmt::Result::Ok(())
    }
}
