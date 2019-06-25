use super::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};

#[derive(Debug)]
pub struct Builder<'a> {
    module: &'a mut Module,
    func_id: FunctionId,
    cur_bb: Option<BasicBlockId>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module, func_id: FunctionId) -> Self {
        Self {
            module,
            func_id,
            cur_bb: None,
        }
    }

    pub fn function_ref(&self) -> &Function {
        self.module.function_ref(self.func_id)
    }

    pub fn function_ref_mut(&mut self) -> &mut Function {
        self.module.function_ref_mut(self.func_id)
    }

    pub fn get_param(&self, idx: usize) -> Option<Value> {
        self.function_ref().get_param_value(self.func_id, idx)
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.function_ref_mut().append_basic_block()
    }

    pub fn set_insert_point(&mut self, id: BasicBlockId) {
        self.cur_bb = Some(id);
    }

    pub fn build_alloca(&mut self, ty: Type) -> Value {
        let ptr_ty = ty.get_pointer_ty();
        let instr = Instruction::new(Opcode::Alloca(ty), ptr_ty);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        val
    }

    pub fn build_load(&mut self, v: Value) -> Value {
        let ty = v.get_type(self.module).get_element_ty().unwrap().clone();
        let instr = Instruction::new(Opcode::Load(v), ty);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        val
    }

    pub fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        let ty = v1.get_type(self.module).clone();
        let instr = Instruction::new(Opcode::Add(v1, v2), ty);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        val
    }

    pub fn build_icmp(&mut self, kind: ICmpKind, v1: Value, v2: Value) -> Value {
        let instr = Instruction::new(Opcode::ICmp(kind, v1, v2), Type::Int1);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        val
    }

    pub fn build_br(&mut self, id: BasicBlockId) -> Value {
        let instr = Instruction::new(Opcode::Br(id), Type::Void);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        Value::None
    }

    pub fn build_cond_br(&mut self, cond: Value, bb1: BasicBlockId, bb2: BasicBlockId) -> Value {
        let instr = Instruction::new(Opcode::CondBr(cond, bb1, bb2), Type::Void);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        Value::None
    }

    pub fn build_phi(&mut self, pairs: Vec<(Value, BasicBlockId)>) -> Value {
        let ty = pairs.get(0).unwrap().0.get_type(self.module).clone();
        let instr = Instruction::new(Opcode::Phi(pairs), ty);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        val
    }

    pub fn build_ret(&mut self, v: Value) -> Value {
        let instr = Instruction::new(Opcode::Ret(v), Type::Void);
        let instr_id = self.function_ref_mut().instr_id(instr);
        let val = Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        });
        let bb = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(bb)
            .iseq
            .push(val);
        Value::None
    }
}
