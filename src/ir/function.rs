use super::{basic_block::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;

pub type FunctionId = Id<Function>;

#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,

    /// Function type
    pub ty: Type,

    /// Basic blocks
    pub basic_block_arena: Arena<BasicBlock>,

    pub basic_blocks: Vec<BasicBlockId>,

    /// Instruction arena
    pub instr_table: Arena<Instruction>,

    /// True if internal function
    pub internal: bool,
}

impl Function {
    pub fn new(name: &str, ret_ty: Type, params_ty: Vec<Type>) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::func_ty(ret_ty, params_ty),
            basic_block_arena: Arena::new(),
            basic_blocks: vec![],
            instr_table: Arena::new(),
            // TODO
            internal: match name {
                "cilk.println.i32" => true,
                _ => false,
            },
        }
    }

    pub fn append_basic_block(&mut self) -> BasicBlockId {
        let id = self.basic_block_arena.alloc(BasicBlock::new());
        // self.basic_blocks.push(id);
        id
    }

    pub fn append_existing_basic_block(&mut self, bb_id: BasicBlockId) {
        self.basic_blocks.push(bb_id);
    }

    pub fn basic_block_ref(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_block_arena[id]
    }

    pub fn basic_block_ref_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_block_arena[id]
    }

    pub fn get_param_value(&self, func_id: FunctionId, idx: usize) -> Option<Value> {
        if idx >= self.ty.get_function_ty().unwrap().params_ty.len() {
            return None;
        }
        Some(Value::Argument(ArgumentValue {
            func_id,
            index: idx,
        }))
    }

    pub fn get_param_type(&self, idx: usize) -> Option<&Type> {
        let params_ty = &self.ty.get_function_ty().unwrap().params_ty;
        if idx >= params_ty.len() {
            return None;
        }
        Some(&params_ty[idx])
    }

    pub fn instr_id(&mut self, instr: Instruction) -> InstructionId {
        self.instr_table.alloc(instr)
    }

    pub fn renumber_vreg(&self) {
        let mut n = 1;
        for (_, bb) in &self.basic_block_arena {
            for instr_val in &mut *bb.iseq_ref_mut() {
                let id = instr_val.get_instr_id().unwrap();
                self.instr_table[id].set_vreg(n);
                n += 1
            }
        }
    }
}

impl Function {
    pub fn to_string(&self, m: &Module) -> String {
        let fty = self.ty.get_function_ty().unwrap();
        format!(
            "define {} {}({}) {{\n{}}}",
            fty.ret_ty.to_string(),
            self.name,
            fty.params_ty
                .iter()
                .fold("".to_string(), |mut s, p| {
                    s += &(p.to_string() + ", ");
                    s
                })
                .trim_matches(&[',', ' '][0..]),
            self.basic_blocks_to_string(m)
        )
    }

    fn basic_blocks_to_string(&self, m: &Module) -> String {
        self.basic_block_arena
            .iter()
            .fold("".to_string(), |s, (id, b)| {
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
                    b.to_string(m)
                )
            })
    }
}
