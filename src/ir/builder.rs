use super::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};

#[derive(Debug)]
pub struct Builder<'a> {
    pub module: &'a mut Module,
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
        let instr = self.create_instr_value(Opcode::Alloca(ty), ptr_ty);
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_load(&mut self, v: Value) -> Value {
        some_then!(
            id,
            v.get_instr_id(),
            self.propagate_instr(self.cur_bb.unwrap(), id)
        );

        let instr = self.create_instr_value(
            Opcode::Load(v),
            v.get_type(self.module).get_element_ty().unwrap().clone(),
        );
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_store(&mut self, src: Value, dst: Value) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, src.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, dst.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::Store(src, dst), Type::Void);
        self.append_instr_to_cur_bb(instr, false);
        instr
    }

    pub fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_add(&v2) {
            return konst;
        }

        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, v1.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, v2.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::Add(v1, v2), v1.get_type(self.module).clone());
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_sub(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_sub(&v2) {
            return konst;
        }

        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, v1.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, v2.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::Sub(v1, v2), v1.get_type(self.module).clone());
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_mul(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_mul(&v2) {
            return konst;
        }

        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, v1.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, v2.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::Mul(v1, v2), v1.get_type(self.module).clone());
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_rem(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_rem(&v2) {
            return konst;
        }

        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, v1.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, v2.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::Rem(v1, v2), v1.get_type(self.module).clone());
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_icmp(&mut self, kind: ICmpKind, v1: Value, v2: Value) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, v1.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        some_then!(id, v2.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::ICmp(kind, v1, v2), Type::Int1);
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_br(&mut self, dst_id: BasicBlockId) -> Value {
        let instr = self.create_instr_value(Opcode::Br(dst_id), Type::Void);
        self.append_instr_to_cur_bb(instr, false);

        let cur_bb_id = self.cur_bb.unwrap();
        self.function_ref_mut()
            .basic_block_ref_mut(cur_bb_id)
            .succ
            .push(dst_id);
        self.function_ref_mut()
            .basic_block_ref_mut(dst_id)
            .pred
            .push(cur_bb_id);

        instr
    }

    pub fn build_cond_br(&mut self, cond: Value, bb1: BasicBlockId, bb2: BasicBlockId) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, cond.get_instr_id(), self.propagate_instr(cur_bb_id, id));

        let instr = self.create_instr_value(Opcode::CondBr(cond, bb1, bb2), Type::Void);
        self.append_instr_to_cur_bb(instr, false);

        let cur_bb = self.function_ref_mut().basic_block_ref_mut(cur_bb_id);
        cur_bb.succ.push(bb1);
        cur_bb.succ.push(bb2);

        self.function_ref_mut()
            .basic_block_ref_mut(bb1)
            .pred
            .push(cur_bb_id);
        self.function_ref_mut()
            .basic_block_ref_mut(bb2)
            .pred
            .push(cur_bb_id);

        instr
    }

    pub fn build_phi(&mut self, pairs: Vec<(Value, BasicBlockId)>) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        for (val, _) in &pairs {
            some_then!(id, val.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        }

        let ty = pairs.get(0).unwrap().0.get_type(self.module).clone();
        let instr = self.create_instr_value(Opcode::Phi(pairs), ty);
        self.append_instr_to_cur_bb(instr, true);
        instr
    }

    pub fn build_call(&mut self, f: Value, args: Vec<Value>) -> Value {
        let cur_bb_id = self.cur_bb.unwrap();
        some_then!(id, f.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        for arg in &args {
            some_then!(id, arg.get_instr_id(), self.propagate_instr(cur_bb_id, id));
        }

        let ret_ty = f
            .get_type(&self.module)
            .get_function_ty()
            .unwrap()
            .ret_ty
            .clone();
        let is_void = ret_ty == Type::Void;
        let instr = self.create_instr_value(Opcode::Call(f, args), ret_ty);
        self.append_instr_to_cur_bb(instr, !is_void);
        instr
    }

    pub fn build_ret(&mut self, v: Value) -> Value {
        some_then!(
            id,
            v.get_instr_id(),
            self.propagate_instr(self.cur_bb.unwrap(), id)
        );

        let instr = self.create_instr_value(Opcode::Ret(v), Type::Void);
        self.append_instr_to_cur_bb(instr, false);
        instr
    }

    // Utils

    fn create_instr_value(&mut self, opcode: Opcode, ret_ty: Type) -> Value {
        let instr = Instruction::new(opcode, ret_ty, self.function_ref_mut().next_vreg());
        let instr_id = self.function_ref_mut().instr_id(instr);
        Value::Instruction(InstructionValue {
            func_id: self.func_id,
            id: instr_id,
        })
    }

    fn append_instr_to_cur_bb(&mut self, instr: Value, set_def: bool) {
        let bb_id = self.cur_bb.unwrap();
        let bb = self.function_ref_mut().basic_block_ref_mut(bb_id);
        bb.iseq_ref_mut().push_back(instr);
        if set_def {
            if let Value::Instruction(InstructionValue { id, .. }) = &instr {
                bb.liveness.borrow_mut().def.insert(*id);
            }
        }
    }

    fn propagate_instr(&self, bb_id: BasicBlockId, instr_id: InstructionId) {
        let bb = &self.function_ref().basic_blocks[bb_id];

        {
            let mut bb_liveness = bb.liveness.borrow_mut();

            if bb_liveness.def.contains(&instr_id) {
                return;
            }

            if !bb_liveness.live_in.insert(instr_id) {
                // live_in already had the value instr_id
                return;
            }
        }

        for pred_id in &bb.pred {
            let pred = &self.function_ref().basic_blocks[*pred_id];
            if pred.liveness.borrow_mut().live_out.insert(instr_id) {
                // live_out didn't have the value instr_id
                self.propagate_instr(*pred_id, instr_id);
            }
        }
    }
}
