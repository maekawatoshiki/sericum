use super::{basic_block::*, function::*, opcode::*, types::*, value::*};

#[derive(Debug)]
pub struct Builder<'a> {
    function: &'a mut Function,
    cur_bb: Option<BasicBlockId>,
}

impl<'a> Builder<'a> {
    pub fn new(function: &'a mut Function) -> Self {
        Self {
            function,
            cur_bb: None,
        }
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.function.append_basic_block()
    }

    pub fn set_insert_point(&mut self, id: BasicBlockId) {
        self.cur_bb = Some(id);
    }

    pub fn build_alloca(&mut self, ty: Type) -> Value {
        let val = Value::Id(self.function.next_value_id(), ty.get_pointer_ty());
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(Instruction::new(Opcode::Alloca(ty), val.clone()));
        val
    }

    pub fn build_load(&mut self, v: Value) -> Value {
        let val = Value::Id(
            self.function.next_value_id(),
            v.get_type().get_element_ty().unwrap().clone(),
        );
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(Instruction::new(Opcode::Load(v), val.clone()));
        val
    }

    pub fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        let val = Value::Id(self.function.next_value_id(), v1.get_type().clone());
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(Instruction::new(Opcode::Add(v1, v2), val.clone()));
        val
    }
}
