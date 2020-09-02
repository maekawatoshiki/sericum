use super::{basic_block::*, module::Module, opcode::*, types::*, value::*, DumpToString};
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

#[derive(Debug, Clone)]
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
            types: module.types.clone(),
            is_internal: is_internal_function(name),
        })
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

    pub fn get_return_type(&self) -> Type {
        let base = self.types.base.borrow();
        base.as_function_ty(self.ty).unwrap().ret_ty
    }

    pub fn get_param_value(&self, idx: usize) -> Option<Value> {
        let base = self.types.base.borrow();
        let params_ty = &base.as_function_ty(self.ty).unwrap().params_ty;
        params_ty.get(idx).map_or(None, |&ty| {
            Some(Value::Argument(ArgumentValue {
                func_id: self.id.unwrap(),
                index: idx,
                ty,
            }))
        })
    }

    pub fn get_param_type(&self, idx: usize) -> Option<Type> {
        let base = self.types.base.borrow();
        let params_ty = &base.as_function_ty(self.ty).unwrap().params_ty;
        params_ty.get(idx).map_or(None, |&ty| Some(ty))
    }

    pub fn get_param_attr(&self, idx: usize) -> Option<ParamAttribute> {
        let base = self.types.base.borrow();
        let params_attr = &base.as_function_ty(self.ty).unwrap().params_attr;
        params_attr.get(&idx).map_or(None, |&a| Some(a))
    }

    pub fn get_params_len(&self) -> usize {
        let base = self.types.base.borrow();
        base.as_function_ty(self.ty).unwrap().params_ty.len()
    }

    pub fn find_inst_pos(&self, inst_id: InstructionId) -> Option<(BasicBlockId, usize)> {
        let parent = self.inst_table[inst_id].parent;
        self.basic_blocks.arena[parent]
            .find_inst_pos(inst_id)
            .map(|pos| (parent, pos))
    }

    pub fn remove_inst(&self, inst_id: InstructionId) {
        if let Some((bb_id, pos)) = self.find_inst_pos(inst_id) {
            self.inst_table[inst_id].remove(&self.inst_table);
            self.basic_blocks.arena[bb_id].iseq_ref_mut().remove(pos);
        }
    }

    fn remove_inst_left_in_bb(&self, inst_id: InstructionId) {
        self.inst_table[inst_id].remove(&self.inst_table);
    }

    pub fn remove_inst_from_block(&self, inst_id: InstructionId) -> Value {
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
}

impl FunctionTrait for Function {
    type BBS = BasicBlocks;

    fn get_basic_blocks(&self) -> &Self::BBS {
        &self.basic_blocks
    }
}

impl DumpToString for &Function {
    fn dump(&self, module: &Module) -> String {
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
                format!("{{\n{}}}", self.basic_blocks.dump(module))
            },
        )
    }
}

impl DumpToString for FunctionId {
    fn dump(&self, module: &Module) -> String {
        module.function_ref(*self).dump(module)
    }
}

impl DumpToString for BasicBlocks {
    fn dump(&self, module: &Module) -> String {
        self.order.iter().fold("".to_string(), |s, &id| {
            let b = &self.arena[id];
            format!(
                "{}label.{}:\t// pred({}), succ({}), def({}), in({}), out({})\n{}\n",
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
                &b.liveness
                    .borrow()
                    .def
                    .iter()
                    .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                    .trim_matches(','),
                &b.liveness
                    .borrow()
                    .live_in
                    .iter()
                    .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                    .trim_matches(','),
                &b.liveness
                    .borrow()
                    .live_out
                    .iter()
                    .fold("".to_string(), |s, x| format!("{}{},", s, x.index()))
                    .trim_matches(','),
                b.dump(module)
            )
        })
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
