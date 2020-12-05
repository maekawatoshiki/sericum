use super::{
    basic_block::*, builder::IRBuilderWithFunction, module::Module, opcode::*, types::*, value::*,
};
use crate::analysis::Analysis;
use crate::codegen::is_internal_function;
use crate::traits::function::FunctionTrait;
use id_arena::*;

pub type FunctionId = Id<Function>;

pub struct FunctionName<'a>(pub &'a str);

impl<'a> From<&'a Function> for FunctionName<'a> {
    fn from(f: &'a Function) -> Self {
        FunctionName(&f.name)
    }
}

impl<'a> From<&'a str> for FunctionName<'a> {
    fn from(s: &'a str) -> Self {
        FunctionName(s)
    }
}

#[derive(Clone)]
pub struct Function {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// Basic blocks
    pub basic_blocks: BasicBlocks,

    /// Instruction arena
    pub inst_table: Arena<Instruction>,

    pub id: Option<FunctionId>,

    pub analyses: Vec<Box<dyn Analysis>>,

    pub types: Types,

    pub is_internal: bool,
}

impl Function {
    pub fn new(module: &mut Module, name: &str, ret_ty: Type, params_ty: Vec<Type>) -> FunctionId {
        let ty = module.types.new_function_ty(ret_ty, params_ty);
        module.add_function(Self {
            name: name.to_string(),
            ty,
            basic_blocks: BasicBlocks::new(),
            inst_table: Arena::new(),
            id: None,
            analyses: vec![],
            types: module.types.clone(),
            is_internal: is_internal_function(name),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.basic_blocks.order.len() == 0
    }

    pub fn get_entry_block(&self) -> Option<BasicBlockId> {
        self.basic_blocks.order.get(0).map(|bb| *bb)
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.arena.alloc(BasicBlock::new());
        self.basic_blocks.order.push(id);
        id
    }

    pub fn append_basic_block_before(&mut self, block: BasicBlockId) -> BasicBlockId {
        let id = self.basic_blocks.arena.alloc(BasicBlock::new());
        if let Some(pos) = self
            .basic_blocks
            .order
            .iter()
            .position(|&block_| block == block_)
        {
            self.basic_blocks.order.insert(pos, id);
        } else {
            self.basic_blocks.order.push(id);
        }
        id
    }

    // pub fn append_existing_basic_block(&mut self, bb_id: BasicBlockId) {
    //     self.basic_blocks.order.push(bb_id);
    // }

    pub fn basic_block_ref(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_blocks.arena[id]
    }

    pub fn basic_block_ref_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks.arena[id]
    }

    pub fn ir_builder<'a>(&'a mut self) -> IRBuilderWithFunction<'a> {
        IRBuilderWithFunction::new(self)
    }

    pub fn get_return_type(&self) -> Type {
        self.types.compound_ty(self.ty).as_function().ret_ty
    }

    pub fn get_param_value(&self, idx: usize) -> Option<Value> {
        self.types
            .compound_ty(self.ty)
            .as_function()
            .params_ty
            .get(idx)
            .map_or(None, |&ty| {
                Some(Value::Argument(ArgumentValue {
                    func_id: self.id.unwrap(),
                    index: idx,
                    ty,
                }))
            })
    }

    pub fn get_param_type(&self, idx: usize) -> Option<Type> {
        self.types
            .compound_ty(self.ty)
            .as_function()
            .params_ty
            .get(idx)
            .map_or(None, |&ty| Some(ty))
    }

    pub fn get_param_attr(&self, idx: usize) -> Option<ParamAttribute> {
        self.types
            .compound_ty(self.ty)
            .as_function()
            .params_attr
            .get(&idx)
            .map_or(None, |&a| Some(a))
    }

    pub fn get_params_len(&self) -> usize {
        self.types
            .compound_ty(self.ty)
            .as_function()
            .params_ty
            .len()
    }

    pub fn find_inst_pos(&self, inst_id: InstructionId) -> Option<(BasicBlockId, usize)> {
        let parent = self.inst_table[inst_id].parent;
        self.basic_blocks.arena[parent]
            .find_inst_pos(inst_id)
            .map(|pos| (parent, pos))
    }

    pub fn remove_inst(&self, inst_id: InstructionId) {
        let (bb_id, pos) = self.find_inst_pos(inst_id).unwrap();
        self.inst_table[inst_id].remove(&self.inst_table);
        self.basic_blocks.arena[bb_id].iseq_ref_mut().remove(pos);
    }

    fn remove_inst_left_in_bb(&self, inst_id: InstructionId) {
        self.inst_table[inst_id].remove(&self.inst_table);
    }

    pub fn remove_inst_from_block(&self, inst_id: InstructionId) -> InstructionId {
        let (block_id, pos) = self.find_inst_pos(inst_id).unwrap();
        self.basic_blocks.arena[block_id].iseq_ref_mut().remove(pos)
    }

    pub fn alloc_inst(&mut self, inst: Instruction) -> InstructionId {
        // TODO
        let id = self.inst_table.alloc(inst);
        {
            let inst = &mut self.inst_table[id];
            inst.set_id(id);
        }
        let inst = &self.inst_table[id];
        inst.set_users(&self.inst_table);
        id
    }

    pub fn change_inst(&mut self, id: InstructionId, mut inst: Instruction) {
        inst.set_id(id);
        let users = self.inst_table[id].users.clone();
        inst.users = users;
        self.remove_inst_left_in_bb(id);
        self.inst_table[id] = inst;
        let inst = &self.inst_table[id];
        inst.set_users(&self.inst_table);
    }

    pub fn get_analysis<T: 'static>(&self) -> Option<&T> {
        self.analyses
            .iter()
            .find_map(|analysis| analysis.as_any().downcast_ref::<T>())
    }

    pub fn get_analysis_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.analyses
            .iter_mut()
            .find_map(|analysis| analysis.as_any_mut().downcast_mut::<T>())
    }

    pub fn add_analysis<T: 'static + Analysis>(&mut self, a: T) {
        if let Some(analysis) = self.get_analysis_mut::<T>() {
            *analysis = a;
            return;
        }

        self.analyses.push(Box::new(a))
    }

    pub fn remove_analysis<T: Analysis + 'static>(&mut self) -> Option<Box<dyn Analysis>> {
        if let Some(i) = self
            .analyses
            .iter()
            .position(|a| a.as_any().downcast_ref::<T>().is_some())
        {
            return Some(self.analyses.remove(i));
        }
        None
    }

    pub fn get_value_type(&self, val: &Value) -> Type {
        match val {
            Value::Argument(ArgumentValue { func_id, index, .. }) => {
                assert_eq!(self.id, Some(*func_id));
                self.get_param_type(*index).unwrap()
            }
            Value::Instruction(InstructionValue { func_id, id, .. }) => {
                assert_eq!(self.id, Some(*func_id));
                self.inst_table[*id].ty
            }
            Value::Function(FunctionValue { func_id, .. }) => {
                self.types.base.borrow().func_ptr_types[func_id]
            }
            Value::Global(GlobalValue { id, .. }) => self.types.base.borrow().gblvar_ptr_types[id],
            Value::Immediate(ref im) => *im.get_type(),
            Value::Constant(ConstantValue { id, .. }) => {
                self.types.base.borrow().const_ptr_types[id]
            }
            Value::None => Type::Void,
        }
    }
}

impl FunctionTrait for Function {
    type BBS = BasicBlocks;

    fn get_basic_blocks(&self) -> &Self::BBS {
        &self.basic_blocks
    }
}

impl Function {
    pub fn dump(&self, module: &Module) -> String {
        let base = module.types.base.borrow();
        let ty = base.as_function_ty(self.ty).unwrap();
        format!(
            "define {} {}({}) {}",
            base.to_string(ty.ret_ty),
            self.name,
            ty.params_ty
                .iter()
                .enumerate()
                .fold("".to_string(), |mut s, (i, p)| {
                    s += &(base.to_string(*p)
                        + ty.params_attr
                            .get(&i)
                            .map_or("", |a| if a.byval { " byval" } else { "" })
                        + ", ");
                    s
                })
                .trim_matches(&[',', ' '][0..]),
            if self.is_internal {
                "internal;".to_owned()
            } else {
                format!("{{\n{}}}", self.basic_blocks.dump(module, self))
            },
        )
    }
}

// impl FunctionId {
//     fn dump(&self, module: &Module) -> String {
//         module.function_ref(*self).dump(module)
//     }
// }

impl BasicBlocks {
    fn dump(&self, module: &Module, f: &Function) -> String {
        self.order.iter().fold("".to_string(), |s, &id| {
            let b = &self.arena[id];
            // let liveness = &self
            //     .liveness
            //     .get(&id)
            //     .unwrap_or_else(|| &LivenessInfo::new());
            format!(
                // "{}label.{}:\t// pred({}), succ({}), def({}), in({}), out({})\n{}\n",
                "{}label.{}:\t// pred({}), succ({})\n{}\n",
                s,
                id.index(),
                &b.pred
                    .iter()
                    .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                    .trim_matches(','),
                &b.succ
                    .iter()
                    .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                    .trim_matches(','),
                // &liveness
                //     .def
                //     .iter()
                //     .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                //     .trim_matches(','),
                // &liveness
                //     .live_in
                //     .iter()
                //     .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                //     .trim_matches(','),
                // &liveness
                //     .live_out
                //     .iter()
                //     .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                //     .trim_matches(','),
                b.dump2(module, f)
            )
        })
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
