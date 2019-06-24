use super::{basic_block::*, opcode::*, types::*, value::*};
use id_arena::*;

pub type FunctionId = Id<Function>;

#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,

    /// Function returning type
    pub ret_ty: Type,

    /// Function parameters type
    pub params_ty: Vec<Type>,

    /// Basic blocks
    pub basic_blocks: Arena<BasicBlock>,

    /// Value id
    pub value_id: ValueId,

    /// Instruction arena
    pub instr_table: Arena<Instruction>,
}

impl Function {
    pub fn new(name: &str, ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Self {
            value_id: params_ty.len(),
            name: name.to_string(),
            ret_ty,
            params_ty,
            basic_blocks: Arena::new(),
            instr_table: Arena::new(),
        }
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        self.basic_blocks.alloc(BasicBlock::new())
    }

    pub fn basic_block_ref(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_blocks[id]
    }

    pub fn basic_block_ref_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks[id]
    }

    pub fn get_param_value(&self, idx: usize) -> Option<Value> {
        if idx >= self.params_ty.len() {
            return None;
        }
        Some(Value::Argument(idx))
    }

    pub fn get_param_type(&self, idx: usize) -> Option<&Type> {
        if idx >= self.params_ty.len() {
            return None;
        }
        Some(&self.params_ty[idx])
    }

    pub fn next_value_id(&mut self) -> ValueId {
        let id = self.value_id;
        self.value_id += 1;
        id
    }

    pub fn instr_id(&mut self, instr: Instruction) -> InstructionId {
        self.instr_table.alloc(instr)
    }
}

impl Function {
    pub fn to_string(&self) -> String {
        format!(
            "define {} {}({}) {{\n{}}}",
            self.ret_ty.to_string(),
            self.name,
            self.params_ty.iter().fold("".to_string(), |mut s, p| {
                s += &(p.to_string() + ", ");
                s
            }),
            self.basic_blocks_to_string()
        )
    }

    fn basic_blocks_to_string(&self) -> String {
        self.basic_blocks.iter().fold("".to_string(), |s, (id, b)| {
            format!("{}label{}:\n{}\n", s, id.index(), b.to_string(self))
        })
    }
}
