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

    pub fn get_param(&self, idx: usize) -> Option<Value> {
        self.function.get_param_value(idx)
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.function.append_basic_block()
    }

    pub fn set_insert_point(&mut self, id: BasicBlockId) {
        self.cur_bb = Some(id);
    }

    pub fn build_alloca(&mut self, ty: Type) -> Value {
        let ptr_ty = ty.get_pointer_ty();
        let instr = Instruction::new(Opcode::Alloca(ty), ptr_ty);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        val
    }

    pub fn build_load(&mut self, v: Value) -> Value {
        let ty = v.get_type(&self.function).get_element_ty().unwrap().clone();
        let instr = Instruction::new(Opcode::Load(v), ty);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        val
    }

    pub fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        let ty = v1.get_type(&self.function).clone();
        let instr = Instruction::new(Opcode::Add(v1, v2), ty);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        val
    }

    pub fn build_icmp(&mut self, kind: ICmpKind, v1: Value, v2: Value) -> Value {
        let instr = Instruction::new(Opcode::ICmp(kind, v1, v2), Type::Int1);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        val
    }

    pub fn build_br(&mut self, id: BasicBlockId) -> Value {
        let instr = Instruction::new(Opcode::Br(id), Type::Void);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        Value::None
    }

    pub fn build_ret(&mut self, v: Value) -> Value {
        let instr = Instruction::new(Opcode::Ret(v), Type::Void);
        let instr_id = self.function.instr_id(instr);
        let val = Value::Instruction(instr_id);
        self.function
            .basic_block_ref_mut(self.cur_bb.unwrap())
            .iseq
            .push(val);
        Value::None
    }
}
