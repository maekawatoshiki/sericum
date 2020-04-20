use super::{basic_block::*, function::*, module::Module, opcode::*, types::*, value::*};

#[derive(Debug)]
pub struct Builder<'a> {
    pub module: &'a mut Module,
    func_id: FunctionId,
    cur_bb: Option<BasicBlockId>,
    insert_point: usize,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module, func_id: FunctionId) -> Self {
        Self {
            module,
            func_id,
            cur_bb: None,
            insert_point: 0,
        }
    }

    pub fn get_param(&self, idx: usize) -> Option<Value> {
        self.module.function_ref(self.func_id).get_param_value(
            &self.module.types,
            self.func_id,
            idx,
        )
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.module
            .function_ref_mut(self.func_id)
            .append_basic_block()
    }

    pub fn set_insert_point(&mut self, id: BasicBlockId) {
        self.cur_bb = Some(id);
        let function = self.module.function_ref_mut(self.func_id);
        function.append_existing_basic_block(id);
        let iseq_len = function.basic_block_ref(id).iseq_ref().len();
        self.insert_point = iseq_len;
    }

    pub fn set_insert_point_at(&mut self, pt: usize, id: BasicBlockId) {
        self.cur_bb = Some(id);
        self.insert_point = pt;
    }

    pub fn build_alloca(&mut self, ty: Type) -> Value {
        let ptr_ty = self.module.types.new_pointer_ty(ty);
        let inst = self.create_inst_value(Opcode::Alloca(ty), ptr_ty);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_gep(&mut self, v: Value, indices: Vec<Value>) -> Value {
        let ty = self.module.types.new_pointer_ty(
            self.module
                .types
                .get_element_ty_with_indices(v.get_type(self.module), &indices)
                .unwrap(),
        );
        let inst = self.create_inst_value(Opcode::GetElementPtr(v, indices), ty);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_load(&mut self, v: Value) -> Value {
        let inst = self.create_inst_value(
            Opcode::Load(v),
            self.module
                .types
                .get_element_ty(v.get_type(self.module), None)
                .unwrap()
                .clone(),
        );
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_store(&mut self, src: Value, dst: Value) -> Value {
        let inst = self.create_inst_value(Opcode::Store(src, dst), Type::Void);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_add(&v2) {
            return konst;
        }

        let inst = self.create_inst_value(Opcode::Add(v1, v2), v1.get_type(self.module).clone());
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_sub(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_sub(&v2) {
            return konst;
        }

        let inst = self.create_inst_value(Opcode::Sub(v1, v2), v1.get_type(self.module).clone());
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_mul(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_mul(&v2) {
            return konst;
        }

        let inst = self.create_inst_value(Opcode::Mul(v1, v2), v1.get_type(self.module).clone());
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_rem(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_rem(&v2) {
            return konst;
        }

        let inst = self.create_inst_value(Opcode::Rem(v1, v2), v1.get_type(self.module).clone());
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_icmp(&mut self, kind: ICmpKind, v1: Value, v2: Value) -> Value {
        let inst = self.create_inst_value(Opcode::ICmp(kind, v1, v2), Type::Int1);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_br(&mut self, dst_id: BasicBlockId) -> Value {
        let inst = self.create_inst_value(Opcode::Br(dst_id), Type::Void);
        self.append_inst_to_cur_bb(inst);

        let cur_bb_id = self.cur_bb.unwrap();
        self.with_function(|f| {
            f.basic_block_ref_mut(cur_bb_id).succ.push(dst_id);
            f.basic_block_ref_mut(dst_id).pred.push(cur_bb_id);
        });

        inst
    }

    pub fn build_cond_br(&mut self, cond: Value, bb1: BasicBlockId, bb2: BasicBlockId) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        let inst = self.create_inst_value(Opcode::CondBr(cond, bb1, bb2), Type::Void);
        self.append_inst_to_cur_bb(inst);

        self.with_function(|f| {
            let cur_bb = f.basic_block_ref_mut(cur_bb_id);
            cur_bb.succ.push(bb1);
            cur_bb.succ.push(bb2);

            f.basic_block_ref_mut(bb1).pred.push(cur_bb_id);
            f.basic_block_ref_mut(bb2).pred.push(cur_bb_id);
        });

        inst
    }

    pub fn build_phi(&mut self, pairs: Vec<(Value, BasicBlockId)>) -> Value {
        let ty = pairs.get(0).unwrap().0.get_type(self.module).clone();
        let inst = self.create_inst_value(Opcode::Phi(pairs), ty);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_call(&mut self, f: Value, args: Vec<Value>) -> Value {
        let ret_ty = self
            .module
            .types
            .as_function_ty(f.get_type(self.module))
            .unwrap()
            .ret_ty;
        let inst = self.create_inst_value(Opcode::Call(f, args), ret_ty);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    pub fn build_ret(&mut self, v: Value) -> Value {
        let inst = self.create_inst_value(Opcode::Ret(v), Type::Void);
        self.append_inst_to_cur_bb(inst);
        inst
    }

    // Utils

    fn create_inst_value(&mut self, opcode: Opcode, ret_ty: Type) -> Value {
        let inst = Instruction::new(opcode, ret_ty, self.cur_bb.unwrap());
        let inst_id = self.function_ref_mut().alloc_inst(inst);
        Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: inst_id,
        })
    }

    fn append_inst_to_cur_bb(&mut self, inst: Value) {
        let bb_id = self.cur_bb.unwrap();
        let insert_point = self.insert_point;
        self.insert_point += 1;

        let bb = self.function_ref().basic_block_ref(bb_id);
        bb.iseq_ref_mut().insert(insert_point, inst);
    }

    fn function_ref(&self) -> &Function {
        self.module.function_ref(self.func_id)
    }

    fn function_ref_mut(&mut self) -> &mut Function {
        self.module.function_ref_mut(self.func_id)
    }

    fn with_function<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Function) -> T,
    {
        let function = self.module.function_ref_mut(self.func_id);
        f(function)
    }
}
