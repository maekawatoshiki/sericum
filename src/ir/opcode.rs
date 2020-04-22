use super::{basic_block::BasicBlockId, function::FunctionId, module::Module, types::*, value::*};
use id_arena::{Arena, Id};
use std::cell::RefCell;

pub type InstructionId = Id<Instruction>;
pub type VirtualRegister = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register(usize);

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: RefCell<Vec<Operand>>,
    pub ty: Type,
    pub id: Option<InstructionId>,
    pub parent: BasicBlockId,
    pub users: RefCell<Vec<InstructionId>>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Opcode {
    Alloca,
    Load,
    Store,
    GetElementPtr, // ptr val, indices
    Add,
    Sub,
    Mul,
    Rem,
    ICmp,
    Br,
    CondBr,
    Phi,
    Call,
    Ret,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Operand {
    Type(Type),
    Value(Value),
    BasicBlock(BasicBlockId),
    ICmpKind(ICmpKind),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ICmpKind {
    Eq,
    Le,
    Lt,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, operands: Vec<Operand>, ty: Type, parent: BasicBlockId) -> Self {
        Self {
            opcode,
            operands: RefCell::new(operands),
            ty,
            id: None,
            parent,
            users: RefCell::new(vec![]),
        }
    }

    pub fn set_id(&mut self, id: InstructionId) {
        self.id = Some(id);
    }

    pub fn set_users(&self, inst_arena: &Arena<Instruction>) {
        for operand in &*self.operands.borrow() {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) => {
                    inst_arena[*id].users.borrow_mut().push(self.id.unwrap());
                }
                _ => {}
            }
        }
    }

    pub fn set_user(&self, inst_arena: &Arena<Instruction>, new: InstructionId) {
        inst_arena[self.id.unwrap()].users.borrow_mut().push(new);
    }

    pub fn replace_operand(&self, inst_arena: &Arena<Instruction>, from: &Operand, to: Operand) {
        let self_id = self.id.unwrap();
        for operand in &mut *self.operands.borrow_mut() {
            if *operand == *from {
                // remove self from operand's users if necessary
                operand.remove_from_users(inst_arena, self_id);
                *operand = to;
                // add self to operand's users if necessary
                operand.set_user(&inst_arena, self_id);
            }
        }
    }

    pub fn remove(&self, inst_arena: &Arena<Instruction>) {
        for operand in &*self.operands.borrow() {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) => {
                    inst_arena[*id]
                        .users
                        .borrow_mut()
                        .retain(|&use_id| use_id != self.id.unwrap());
                }
                _ => {}
            }
        }
    }

    pub fn fold_const(&self) -> Option<Value> {
        let operands = self.operands.borrow();
        match self.opcode {
            Opcode::Add => operands[0].as_value().const_add(&operands[1].as_value()),
            Opcode::Sub => operands[0].as_value().const_sub(&operands[1].as_value()),
            Opcode::Mul => operands[0].as_value().const_mul(&operands[1].as_value()),
            Opcode::Rem => operands[0].as_value().const_rem(&operands[1].as_value()),
            _ => None,
        }
    }

    pub fn to_string(&self, parent: &Module) -> String {
        let mut output = self.opcode.to_string().to_owned();
        for (i, operand) in self.operands.borrow().iter().enumerate() {
            output = format!(
                "{}{}{}",
                output,
                if i == 0 { " " } else { ", " },
                operand.to_string(parent)
            );
        }

        format!(
            "{} // (self:{}, users:{:?})",
            output,
            self.id.unwrap().index(),
            self.users.borrow()
        )
    }
}

impl Opcode {
    pub fn returns_value(&self) -> bool {
        match self {
            Opcode::Br | Opcode::CondBr | Opcode::Ret | Opcode::Store | Opcode::Call|
                /* alloca doesn't return value = */ Opcode::Alloca => false,
            _ => true,
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Opcode::Alloca => "alloca",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::GetElementPtr => "getelementptr",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::Rem => "rem",
            Opcode::ICmp => "icmp",
            Opcode::Br => "br",
            Opcode::CondBr => "br",
            Opcode::Phi => "phi",
            Opcode::Call => "call",
            Opcode::Ret => "ret",
        }
    }
}

impl Operand {
    pub fn new_inst(func_id: FunctionId, id: InstructionId) -> Self {
        Operand::Value(Value::Instruction(InstructionValue { func_id, id }))
    }

    // TODO: should return cow?
    pub fn to_string(&self, parent: &Module) -> String {
        match self {
            Self::BasicBlock(id) => format!("%label.{}", id.index()),
            Self::ICmpKind(kind) => kind.as_str().to_owned(),
            Self::Type(ty) => parent.types.to_string(*ty),
            Self::Value(v) => v.to_string(parent, false),
        }
    }

    pub fn as_basic_block(&self) -> &BasicBlockId {
        match self {
            Self::BasicBlock(id) => id,
            _ => panic!(),
        }
    }
    pub fn as_icmp_kind(&self) -> &ICmpKind {
        match self {
            Self::ICmpKind(kind) => kind,
            _ => panic!(),
        }
    }
    pub fn as_type(&self) -> &Type {
        match self {
            Self::Type(ty) => ty,
            _ => panic!(),
        }
    }
    pub fn as_value(&self) -> &Value {
        match self {
            Self::Value(v) => v,
            _ => panic!(),
        }
    }

    pub fn remove_from_users(&self, inst_arena: &Arena<Instruction>, remove: InstructionId) {
        if let Operand::Value(Value::Instruction(InstructionValue { id, .. })) = self {
            inst_arena[*id]
                .users
                .borrow_mut()
                .retain(|&use_id| use_id != remove);
        }
    }

    pub fn set_user(&self, inst_arena: &Arena<Instruction>, new: InstructionId) {
        if let Operand::Value(Value::Instruction(InstructionValue { id, .. })) = self {
            inst_arena[*id].set_user(inst_arena, new);
        }
    }
}

impl ICmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ICmpKind::Eq => "eq",
            ICmpKind::Lt => "lt",
            ICmpKind::Le => "le",
        }
    }
}

impl Register {
    pub fn shift(self, n: usize) -> Register {
        Register(self.0 + n)
    }

    pub fn as_u8(&self) -> u8 {
        self.0 as u8
    }
}
