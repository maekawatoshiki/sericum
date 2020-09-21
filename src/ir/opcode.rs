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
    pub operands: Vec<Operand>,
    pub ty: Type,
    pub id: Option<InstructionId>,
    pub parent: BasicBlockId,
    pub users: RefCell<Vec<InstructionId>>,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    Alloca,
    Load,
    Store,
    GetElementPtr, // ptr val, indices
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    SIToFP,
    FPToSI,
    Sext,
    ICmp,
    FCmp,
    Br,
    CondBr,
    Phi,
    Call,
    Ret,
}

#[derive(Debug, Clone, PartialEq, Copy, Hash, Eq)]
pub enum Operand {
    Type(Type),
    Value(Value),
    BasicBlock(BasicBlockId),
    ICmpKind(ICmpKind),
    FCmpKind(FCmpKind),
}

#[derive(Clone, Debug, Copy, PartialEq, Hash, Eq)]
pub enum ICmpKind {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
    // Ne,
}

#[derive(Clone, Debug, Copy, PartialEq, Hash, Eq)]
pub enum FCmpKind {
    UEq,
    UNe,
    ULe,
    ULt,
    UGe,
    UGt,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, operands: Vec<Operand>, ty: Type, parent: BasicBlockId) -> Self {
        Self {
            opcode,
            operands: operands,
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
        let self_id = self.id.unwrap();
        for operand in &self.operands {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) => {
                    let mut users = inst_arena[*id].users.borrow_mut();
                    if users.contains(&self_id) {
                        continue;
                    }
                    users.push(self_id);
                }
                _ => {}
            }
        }
    }

    pub fn set_user(&self, inst_arena: &Arena<Instruction>, new: InstructionId) {
        inst_arena[self.id.unwrap()].users.borrow_mut().push(new);
    }

    pub fn add_operand(arena: &mut Arena<Instruction>, self_id: InstructionId, operand: Operand) {
        if let Operand::Value(Value::Instruction(InstructionValue { id, .. })) = &operand {
            let mut users = arena[*id].users.borrow_mut();
            if !users.contains(&self_id) {
                users.push(self_id);
            }
        }
        arena[self_id].operands.push(operand);
    }

    // TODO: using two loops is inefficient
    pub fn replace_operand(
        arena: &mut Arena<Instruction>,
        self_id: InstructionId,
        from: &Operand,
        to: Operand,
    ) {
        for operand in &arena[self_id].operands {
            if *operand == *from {
                // remove self from operand's users if necessary
                operand.remove_from_users(arena, self_id);
                // add self to operand's users if necessary
                to.set_user(arena, self_id);
            }
        }
        for operand in &mut arena[self_id].operands {
            // actually replace from with to
            if *operand == *from {
                *operand = to;
            }
        }
    }

    // TODO: using two loops is inefficient
    pub fn replace_operand_inst(
        arena: &mut Arena<Instruction>,
        self_id: InstructionId,
        from: InstructionId,
        to: Operand,
    ) {
        for operand in &arena[self_id].operands {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) if *id == from => {}
                _ => continue,
            };
            // remove self from operand's users if necessary
            operand.remove_from_users(arena, self_id);
            // add self to operand's users if necessary
            to.set_user(arena, self_id);
        }
        for operand in &mut arena[self_id].operands {
            match operand {
                Operand::Value(Value::Instruction(InstructionValue { id, .. })) if *id == from => {}
                _ => continue,
            };
            // replace 'from' with 'to'
            *operand = to;
        }
    }

    pub fn replace_all_uses(arena: &mut Arena<Instruction>, self_id: InstructionId, to: Operand) {
        let users = arena[self_id].users.borrow().clone();
        for u in users {
            Self::replace_operand_inst(arena, u, self_id, to);
        }
        assert!(arena[self_id].users.borrow().len() == 0);
    }

    pub fn remove(&self, inst_arena: &Arena<Instruction>) {
        for operand in &self.operands {
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
        let operands = &self.operands;
        match self.opcode {
            Opcode::Add => operands[0].as_value().const_add(&operands[1].as_value()),
            Opcode::Sub => operands[0].as_value().const_sub(&operands[1].as_value()),
            Opcode::Mul => operands[0].as_value().const_mul(&operands[1].as_value()),
            Opcode::Div => operands[0].as_value().const_div(&operands[1].as_value()),
            Opcode::Rem => operands[0].as_value().const_rem(&operands[1].as_value()),
            _ => None,
        }
    }

    pub fn has_one_use(&self) -> bool {
        self.users.borrow().len() == 1
    }

    pub fn to_string(&self, parent: &Module) -> String {
        let mut output = self.opcode.to_string().to_owned();
        for (i, operand) in self.operands.iter().enumerate() {
            output = format!(
                "{}{}{}",
                output,
                if i == 0 { " " } else { ", " },
                operand.to_string(parent)
            );
        }

        format!(
            "{} ",
            output,
            // self.id.unwrap().index(),
            // self.users.borrow().iter().take(10).collect::<Vec<_>>()
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

    pub fn is_terminator(&self) -> bool {
        matches!(self, Opcode::Br | Opcode::CondBr | Opcode::Ret)
    }

    pub fn access_memory(&self) -> bool {
        matches!(self, Opcode::Store | Opcode::Load)
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
            Opcode::Div => "div",
            Opcode::Rem => "rem",
            Opcode::Shl => "shl",
            Opcode::SIToFP => "sitofp",
            Opcode::FPToSI => "fptosi",
            Opcode::Sext => "sext",
            Opcode::ICmp => "icmp",
            Opcode::FCmp => "fcmp",
            Opcode::Br => "br",
            Opcode::CondBr => "br",
            Opcode::Phi => "phi",
            Opcode::Call => "call",
            Opcode::Ret => "ret",
        }
    }
}

impl Operand {
    pub fn new_inst(func_id: FunctionId, id: InstructionId, ty: Type) -> Self {
        Operand::Value(Value::Instruction(InstructionValue { func_id, id, ty }))
    }

    // TODO: should return cow?
    pub fn to_string(&self, parent: &Module) -> String {
        match self {
            Self::BasicBlock(id) => format!("%label.{}", id.index()),
            Self::ICmpKind(kind) => kind.as_str().to_owned(),
            Self::FCmpKind(kind) => kind.as_str().to_owned(),
            Self::Type(ty) => parent.types.to_string(*ty),
            Self::Value(v) => v.to_string(parent, false),
        }
    }

    pub fn get_value(&self) -> Option<&Value> {
        match self {
            Self::Value(val) => Some(val),
            _ => None,
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
    pub fn as_fcmp_kind(&self) -> &FCmpKind {
        match self {
            Self::FCmpKind(kind) => kind,
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
            ICmpKind::Ne => "ne",
            ICmpKind::Lt => "lt",
            ICmpKind::Le => "le",
            ICmpKind::Gt => "gt",
            ICmpKind::Ge => "ge",
        }
    }
}

impl FCmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            FCmpKind::UEq => "ueq",
            FCmpKind::UNe => "une",
            FCmpKind::ULt => "ult",
            FCmpKind::ULe => "ule",
            FCmpKind::UGt => "ugt",
            FCmpKind::UGe => "uge",
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
