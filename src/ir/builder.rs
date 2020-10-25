use super::{basic_block::*, function::*, module::Module, opcode::*, types::*, value::*};

pub struct IRBuilderWithFunction<'a> {
    func: &'a mut Function,
    block: Option<BasicBlockId>,
    insert_point: usize,
}

pub struct IRBuilderWithModuleAndFuncId<'a> {
    module: &'a mut Module,
    func_id: FunctionId,
    block: Option<BasicBlockId>,
    insert_point: usize,
}

impl<'a> IRBuilderWithFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self {
            func,
            block: None,
            insert_point: 0,
        }
    }
}

impl<'a> IRBuilderWithModuleAndFuncId<'a> {
    pub fn new(module: &'a mut Module, func_id: FunctionId) -> Self {
        Self {
            module,
            func_id,
            block: None,
            insert_point: 0,
        }
    }
}

impl<'a> IRBuilder for IRBuilderWithFunction<'a> {
    fn module(&self) -> Option<&Module> {
        None
    }

    fn module_mut(&mut self) -> Option<&mut Module> {
        None
    }

    fn func_ref(&self) -> &Function {
        self.func
    }

    fn func_ref_mut(&mut self) -> &mut Function {
        self.func
    }

    fn insert_point(&self) -> usize {
        self.insert_point
    }

    fn insert_point_mut(&mut self) -> &mut usize {
        &mut self.insert_point
    }

    fn block(&self) -> Option<BasicBlockId> {
        self.block
    }

    fn block_mut(&mut self) -> &mut Option<BasicBlockId> {
        &mut self.block
    }
}

impl<'a> IRBuilder for IRBuilderWithModuleAndFuncId<'a> {
    fn module(&self) -> Option<&Module> {
        Some(self.module)
    }

    fn module_mut(&mut self) -> Option<&mut Module> {
        Some(&mut self.module)
    }

    fn func_ref(&self) -> &Function {
        self.module.function_ref(self.func_id)
    }

    fn func_ref_mut(&mut self) -> &mut Function {
        self.module.function_ref_mut(self.func_id)
    }

    fn insert_point(&self) -> usize {
        self.insert_point
    }

    fn insert_point_mut(&mut self) -> &mut usize {
        &mut self.insert_point
    }

    fn block(&self) -> Option<BasicBlockId> {
        self.block
    }

    fn block_mut(&mut self) -> &mut Option<BasicBlockId> {
        &mut self.block
    }
}

pub trait IRBuilder {
    fn module(&self) -> Option<&Module>;
    fn module_mut(&mut self) -> Option<&mut Module>;
    fn func_ref(&self) -> &Function;
    fn func_ref_mut(&mut self) -> &mut Function;
    fn insert_point(&self) -> usize;
    fn insert_point_mut(&mut self) -> &mut usize;
    fn block(&self) -> Option<BasicBlockId>;
    fn block_mut(&mut self) -> &mut Option<BasicBlockId>;

    fn get_param(&self, idx: usize) -> Option<Value> {
        self.func_ref().get_param_value(idx)
    }

    fn append_basic_block(&mut self) -> BasicBlockId {
        self.func_ref_mut().append_basic_block()
    }

    fn set_insert_point_at_end_of_current_block(&mut self) {
        let len = self
            .func_ref()
            .basic_block_ref(self.block().unwrap())
            .iseq_ref()
            .len();
        *self.insert_point_mut() = len;
    }

    fn set_insert_point(&mut self, id: BasicBlockId) {
        *self.block_mut() = Some(id);
        let iseq_len = self.func_ref_mut().basic_block_ref(id).iseq_ref().len();
        *self.insert_point_mut() = iseq_len;
    }

    fn set_insert_point_at(&mut self, pt: usize, id: BasicBlockId) {
        *self.block_mut() = Some(id);
        *self.insert_point_mut() = pt;
    }

    fn set_insert_point_before_inst(&mut self, inst_id: InstructionId) -> Option<()> {
        let (bb_id, inst_pos) = self.func_ref_mut().find_inst_pos(inst_id)?;
        self.set_insert_point_at(inst_pos, bb_id);
        Some(())
    }

    fn set_insert_point_before_terminator(&mut self, id: BasicBlockId) {
        *self.block_mut() = Some(id);
        *self.insert_point_mut() = {
            let func = self.func_ref();
            let iseq_ref = func.basic_block_ref(id).iseq_ref();
            let mut pt = iseq_ref.len();
            for inst in iseq_ref.iter().rev().map(|&id| &func.inst_table[id]) {
                if !inst.opcode.is_terminator() {
                    break;
                }
                pt -= 1;
            }
            pt
        }
    }

    fn build_alloca(&mut self, ty: Type) -> Value {
        let ptr_ty = self.func_ref_mut().types.new_pointer_ty(ty);
        let inst = self.create_inst_value2(Opcode::Alloca, InstOperand::Type { ty }, ptr_ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_gep(&mut self, v: Value, indices: Vec<Value>) -> Value {
        let elem_ty = self
            .func_ref()
            .types
            .get_element_ty_with_indices(v.get_type(), &indices)
            .unwrap();
        let ptr_ty = self.func_ref_mut().types.new_pointer_ty(elem_ty);
        let mut operands = vec![Operand::Value(v)];
        operands.extend(indices.iter().map(|v| Operand::Value(*v)));
        let inst = self.create_inst_value(Opcode::GetElementPtr, operands, ptr_ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_load(&mut self, arg: Value) -> Value {
        let inst = self.create_inst_value2(
            Opcode::Load,
            InstOperand::Load { arg },
            self.func_ref()
                .types
                .get_element_ty(arg.get_type(), None)
                .unwrap()
                .clone(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_store(&mut self, src: Value, dst: Value) -> Value {
        let inst = self.create_inst_value2(
            Opcode::Store,
            InstOperand::Store { args: [src, dst] },
            Type::Void,
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_add(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_add(&v2) {
            return konst;
        }

        let inst = self.create_inst_value2(
            Opcode::Add,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_sub(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_sub(&v2) {
            return konst;
        }

        let inst = self.create_inst_value2(
            Opcode::Sub,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_mul(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_mul(&v2) {
            return konst;
        }

        let inst = self.create_inst_value2(
            Opcode::Mul,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_div(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_div(&v2) {
            return konst;
        }

        let inst = self.create_inst_value2(
            Opcode::Div,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_rem(&mut self, v1: Value, v2: Value) -> Value {
        if let Some(konst) = v1.const_rem(&v2) {
            return konst;
        }

        let inst = self.create_inst_value2(
            Opcode::Rem,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_shl(&mut self, v1: Value, v2: Value) -> Value {
        // if let Some(konst) = v1.const_shl(&v2) {
        //     return konst;
        // }

        let inst = self.create_inst_value2(
            Opcode::Shl,
            InstOperand::Binary { args: [v1, v2] },
            v1.get_type(),
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_sitofp(&mut self, v: Value, ty: Type) -> Value {
        let inst = self.create_inst_value(Opcode::SIToFP, vec![Operand::Value(v)], ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_fptosi(&mut self, v: Value, ty: Type) -> Value {
        let inst = self.create_inst_value(Opcode::FPToSI, vec![Operand::Value(v)], ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_sext(&mut self, v: Value, ty: Type) -> Value {
        let inst = self.create_inst_value(Opcode::Sext, vec![Operand::Value(v)], ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_icmp(&mut self, kind: ICmpKind, v1: Value, v2: Value) -> Value {
        let inst = self.create_inst_value(
            Opcode::ICmp,
            vec![
                Operand::ICmpKind(kind),
                Operand::Value(v1),
                Operand::Value(v2),
            ],
            Type::i1,
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_fcmp(&mut self, kind: FCmpKind, v1: Value, v2: Value) -> Value {
        let inst = self.create_inst_value(
            Opcode::FCmp,
            vec![
                Operand::FCmpKind(kind),
                Operand::Value(v1),
                Operand::Value(v2),
            ],
            Type::i1,
        );
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_br(&mut self, dst_id: BasicBlockId) -> Value {
        let inst =
            self.create_inst_value(Opcode::Br, vec![Operand::BasicBlock(dst_id)], Type::Void);
        self.append_inst_to_current_block(inst.as_instruction().id);

        let cur_bb_id = self.block().unwrap();
        self.with_function(|f| {
            f.basic_block_ref_mut(cur_bb_id).succ.insert(dst_id);
            f.basic_block_ref_mut(dst_id).pred.insert(cur_bb_id);
        });

        inst
    }

    fn build_cond_br(&mut self, cond: Value, bb1: BasicBlockId, bb2: BasicBlockId) -> Value {
        let cur_bb_id = self.block().unwrap();
        let inst = self.create_inst_value(
            Opcode::CondBr,
            vec![
                Operand::Value(cond),
                Operand::BasicBlock(bb1),
                Operand::BasicBlock(bb2),
            ],
            Type::Void,
        );
        self.append_inst_to_current_block(inst.as_instruction().id);

        self.with_function(|f| {
            let cur_bb = f.basic_block_ref_mut(cur_bb_id);
            cur_bb.succ.insert(bb1);
            cur_bb.succ.insert(bb2);

            f.basic_block_ref_mut(bb1).pred.insert(cur_bb_id);
            f.basic_block_ref_mut(bb2).pred.insert(cur_bb_id);
        });

        inst
    }

    fn build_phi(&mut self, pairs: Vec<(Value, BasicBlockId)>) -> Value {
        let ty = pairs.get(0).unwrap().0.get_type();
        let mut operands = vec![];
        for (v, bb) in pairs {
            operands.push(Operand::Value(v));
            operands.push(Operand::BasicBlock(bb));
        }
        let inst = self.create_inst_value(Opcode::Phi, operands, ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_call(&mut self, f: Value, args: Vec<Value>) -> Value {
        let ret_ty = self
            .func_ref()
            .types
            .compound_ty(f.get_type())
            .as_function()
            .ret_ty;
        let mut operands = vec![Operand::Value(f)];
        operands.extend(args.iter().map(|&v| Operand::Value(v)));
        let inst = self.create_inst_value(Opcode::Call, operands, ret_ty);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn build_ret(&mut self, v: Value) -> Value {
        let inst = self.create_inst_value(Opcode::Ret, vec![Operand::Value(v)], Type::Void);
        self.append_inst_to_current_block(inst.as_instruction().id);
        inst
    }

    fn is_last_inst_terminator(&self) -> bool {
        let bb = self.func_ref().basic_block_ref(self.block().unwrap());
        bb.iseq_ref().last().map_or(false, |&id| {
            self.func_ref().inst_table[id].opcode.is_terminator()
        })
    }

    fn insert(&mut self, inst: InstructionId) {
        self.append_inst_to_current_block(inst)
    }

    // ----- utils ------

    fn create_inst_value(&mut self, opcode: Opcode, operands: Vec<Operand>, ret_ty: Type) -> Value {
        let inst = Instruction::new(opcode, operands, ret_ty, self.block().unwrap());
        let inst_id = self.func_ref_mut().alloc_inst(inst);
        Value::Instruction(InstructionValue {
            func_id: self.func_ref().id.unwrap(),
            id: inst_id,
            ty: ret_ty,
        })
    }

    fn create_inst_value2(&mut self, opcode: Opcode, operand: InstOperand, ret_ty: Type) -> Value {
        let mut inst = Instruction::new(opcode, vec![], ret_ty, self.block().unwrap());
        inst.operand = operand;
        let inst_id = self.func_ref_mut().alloc_inst(inst);
        Value::Instruction(InstructionValue {
            func_id: self.func_ref().id.unwrap(),
            id: inst_id,
            ty: ret_ty,
        })
    }

    fn append_inst_to_current_block(&mut self, inst_id: InstructionId) {
        let bb_id = self.block().unwrap();
        let insert_point = self.insert_point();
        *self.insert_point_mut() += 1;
        let bb = self.func_ref().basic_block_ref(bb_id);
        bb.iseq_ref_mut().insert(insert_point, inst_id);
    }

    fn with_function<Func, T>(&mut self, f: Func) -> T
    where
        Func: FnOnce(&mut Function) -> T,
    {
        let function = self.func_ref_mut();
        f(function)
    }
}
