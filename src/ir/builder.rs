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
        let val_id = self.function.next_value_id();
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(Instruction::new(Opcode::Alloca(ty), Value::Id(val_id)));
        Value::Id(val_id)
    }
}
