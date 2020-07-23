use super::{basic_block::*, module::Module, opcode::*, types::*, value::*, DumpToString};
use crate::codegen::is_internal_function;
use crate::traits::function::FunctionTrait;
use id_arena::*;

/// FunctionId that indicates a Function uniquely is given by [id_arena::Arena](https://docs.rs/id-arena).
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

/// A representation of Module.
/// Module has its name, basic_blocks and etc.
/// Type definitions that a function has are same as a module's that has the function.
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
    /// Creates an empty function.
    /// A cilk function belongs to a [Module](../module/struct.Module.html), 
    /// so you must give a module when you want to create a function.
    /// See The implementation of [add_function()](../module/struct.Module.html#method.add_function)
    ///
    /// # Arguments
    ///
    /// * `module` ... A module that owns created function.
    /// * `name` ... Function's name
    /// * `ret_ty` ... A [Type](../types/enum.Type.html) the function returns.
    /// * `params_ty` ... A sequence of [Type](../types/enum.Type.html) that are passed to the function.
    ///
    /// # Examples
    ///
    /// ```
    /// use cilk::ir::{module::Module, function::Function, types::Type};
    /// 
    /// let mut m = Module::new("sample");
    /// assert_eq!(0, m.functions.len());
    /// 
    /// let f1_id = Function::new(&mut m, "f1", Type::Void, vec![]);
    /// 
    /// assert_eq!(1, m.functions.len());
    /// ```
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

    /// Append a [BasicBlock](../basic_block/struct.BasicBlock.html) to the function.
    /// Appended bb is empty, so use [basic_block_ref_mut()](../basic_block/struct.BasicBlock.html#method.basic_block_ref_mut) if you want to modify the bb.
    pub fn append_basic_block(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.arena.alloc(BasicBlock::new());
        self.basic_blocks.order.push(id);
        id
    }

    // pub fn append_existing_basic_block(&mut self, bb_id: BasicBlockId) {
    //     self.basic_blocks.order.push(bb_id);
    // }

    /// Get a reference to the corresponding basicblock with the given id
    pub fn basic_block_ref(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_blocks.arena[id]
    }

    /// Get a mutable reference to the corresponding basicblock with the given id
    pub fn basic_block_ref_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks.arena[id]
    }

    /// Get a return type the receiver of this method has.
    pub fn get_return_type(&self) -> Type {
        let base = self.types.base.borrow();
        base.as_function_ty(self.ty).unwrap().ret_ty
    }

    /// Get a parameter of the position that's specified by `idx`.
    /// this method returns None if idx specifies out of parameters list's range.
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

    /// Get a parameter's type of the position that's specified by `idx`.
    /// this method returns None if idx specifies out of parameters list's range.
    pub fn get_param_type(&self, idx: usize) -> Option<Type> {
        let base = self.types.base.borrow();
        let params_ty = &base.as_function_ty(self.ty).unwrap().params_ty;
        params_ty.get(idx).map_or(None, |&ty| Some(ty))
    }

    /// Get a length of the parameters list.
    pub fn get_params_len(&self) -> usize {
        let base = self.types.base.borrow();
        base.as_function_ty(self.ty).unwrap().params_ty.len()
    }

    /// Find the position of given instruction in the instruction's parent.
    pub fn find_inst_pos(&self, inst_id: InstructionId) -> Option<(BasicBlockId, usize)> {
        let parent = self.inst_table[inst_id].parent;
        self.basic_blocks.arena[parent]
            .find_inst_pos(inst_id)
            .map(|pos| (parent, pos))
    }

    /// Remove a instruction from the receiver of this method.
    pub fn remove_inst(&self, inst_id: InstructionId) {
        let (bb_id, pos) = self.find_inst_pos(inst_id).unwrap();
        self.inst_table[inst_id].remove(&self.inst_table);
        self.basic_blocks.arena[bb_id].iseq_ref_mut().remove(pos);
    }

    fn remove_inst_left_in_bb(&self, inst_id: InstructionId) {
        self.inst_table[inst_id].remove(&self.inst_table);
    }

    /// Allocate a instruction to the function(receiver).
    /// Note that the instruction doesn't belong to any basic-blocks.
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

    /// change given id's instruction to the `inst`.
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
                .fold("".to_string(), |mut s, p| {
                    s += &(base.to_string(*p) + ", ");
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
