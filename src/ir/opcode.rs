use super::{basic_block::BasicBlockId, module::Module, types::*, value::*};
use id_arena::{Arena, Id};
use std::cell::RefCell;

pub type InstructionId = Id<Instruction>;
pub type VirtualRegister = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register(usize);

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    // pub operands: Vec<Operand>,
    pub operand: InstOperand,
    pub ty: Type,
    pub id: Option<InstructionId>,
    pub parent: BasicBlockId,
    pub users: RefCell<Vec<InstructionId>>,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    Alloca, //
    Load,   //
    Store,  //
    GetElementPtr,
    Add,    //
    Sub,    //
    Mul,    //
    Div,    //
    Rem,    //
    Shl,    //
    SIToFP, //
    FPToSI, //
    Sext,   //
    ICmp,   //
    FCmp,   //
    Br,     //
    CondBr, //
    Phi,    //
    Call,   //
    Ret,    //
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstOperand {
    Type {
        ty: Type,
    },
    Store {
        args: [Value; 2],
    },
    Load {
        arg: Value,
    },
    Binary {
        args: [Value; 2],
    },
    Cast {
        arg: Value,
    },
    IntCmp {
        cond: ICmpKind,
        args: [Value; 2],
    },
    FloatCmp {
        cond: FCmpKind,
        args: [Value; 2],
    },
    Branch {
        dst: BasicBlockId,
    },
    BranchArgs {
        dst: BasicBlockId,
        args: Vec<Value>,
    },
    CondBranch {
        arg: Value,
        dsts: [BasicBlockId; 2],
    },
    Call {
        args: Vec<Value>,
    },
    Phi {
        blocks: Vec<BasicBlockId>,
        args: Vec<Value>,
    },
    Gep {
        args: Vec<Value>,
    },
    Ret {
        arg: Value,
    },
    None,
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
    pub fn new(opcode: Opcode, operand: InstOperand, ty: Type, parent: BasicBlockId) -> Self {
        Self {
            opcode,
            operand,
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
        for operand in self.operand.args() {
            if let Value::Instruction(InstructionValue { id, .. }) = operand {
                let mut users = inst_arena[*id].users.borrow_mut();
                if users.contains(&self_id) {
                    continue;
                }
                users.push(self_id);
            }
        }
    }

    pub fn set_user(&self, inst_arena: &Arena<Instruction>, new: InstructionId) {
        inst_arena[self.id.unwrap()].users.borrow_mut().push(new);
    }

    pub fn add_value_operand(arena: &mut Arena<Instruction>, self_id: InstructionId, value: Value) {
        if let Value::Instruction(InstructionValue { id, .. }) = &value {
            let mut users = arena[*id].users.borrow_mut();
            if !users.contains(&self_id) {
                users.push(self_id);
            }
        }
        arena[self_id].operand.phi_args_mut().push(value)
    }

    pub fn replace_block_operand(
        arena: &mut Arena<Instruction>,
        self_id: InstructionId,
        from: &BasicBlockId,
        to: BasicBlockId,
    ) {
        for block in arena[self_id].operand.blocks_mut() {
            if from == block {
                *block = to;
            }
        }
    }

    pub fn replace_operand_value(
        arena: &mut Arena<Instruction>,
        self_id: InstructionId,
        from: &Value,
        to: Value,
    ) {
        for val in arena[self_id].operand.args() {
            if val == from {
                val.remove_from_users(arena, self_id);
                to.set_user(arena, self_id)
            }
        }
        for val in arena[self_id].operand.args_mut() {
            if val == from {
                *val = to;
            }
        }
    }

    pub fn replace_inst_operand(
        arena: &mut Arena<Instruction>,
        self_id: InstructionId,
        from: InstructionId,
        to: Value,
    ) {
        for val in arena[self_id].operand.args() {
            match val {
                Value::Instruction(InstructionValue { id, .. }) if *id == from => {
                    val.remove_from_users(arena, self_id);
                    to.set_user(arena, self_id);
                }
                _ => continue,
            };
        }
        for val in arena[self_id].operand.args_mut() {
            match val {
                Value::Instruction(InstructionValue { id, .. }) if *id == from => {
                    // replace 'from' with 'to'
                    *val = to
                }
                _ => continue,
            };
        }
    }

    pub fn replace_all_uses(arena: &mut Arena<Instruction>, self_id: InstructionId, to: Value) {
        let users = arena[self_id].users.borrow().clone();
        for u in users {
            Self::replace_inst_operand(arena, u, self_id, to);
        }
        assert!(arena[self_id].users.borrow().len() == 0);
    }

    pub fn remove(&self, inst_arena: &Arena<Instruction>) {
        for val in self.operand.args() {
            if let Value::Instruction(InstructionValue { id, .. }) = val {
                inst_arena[*id]
                    .users
                    .borrow_mut()
                    .retain(|&use_id| use_id != self.id.unwrap());
            }
        }
    }

    pub fn fold_const(&self) -> Option<Value> {
        match self.opcode {
            Opcode::Add => self.operand.args()[0].const_add(&self.operand.args()[1]),
            Opcode::Sub => self.operand.args()[0].const_sub(&self.operand.args()[1]),
            Opcode::Mul => self.operand.args()[0].const_mul(&self.operand.args()[1]),
            Opcode::Div => self.operand.args()[0].const_div(&self.operand.args()[1]),
            Opcode::Rem => self.operand.args()[0].const_rem(&self.operand.args()[1]),
            _ => None,
        }
    }

    pub fn has_one_use(&self) -> bool {
        self.users.borrow().len() == 1
    }

    pub fn to_string(&self, parent: &Module) -> String {
        let output = self.opcode.to_string().to_owned();
        let ty2str = |ty: &Type| -> String { parent.types.to_string(*ty) };
        match &self.operand {
            InstOperand::Type { ty } => format!("{} {}", output, ty2str(ty)),
            InstOperand::Store { args } | InstOperand::Binary { args } => format!(
                "{} {}, {}",
                output,
                args[0].to_string(parent, false),
                args[1].to_string(parent, false)
            ),
            InstOperand::Ret { arg } | InstOperand::Load { arg } | InstOperand::Cast { arg } => {
                format!("{} {}", output, arg.to_string(parent, false))
            }
            InstOperand::IntCmp { cond, args } => format!(
                "{} {} {}, {}",
                output,
                cond.as_str(),
                args[0].to_string(parent, false),
                args[1].to_string(parent, false),
            ),
            InstOperand::FloatCmp { cond, args } => format!(
                "{} {} {}, {}",
                output,
                cond.as_str(),
                args[0].to_string(parent, false),
                args[1].to_string(parent, false),
            ),
            InstOperand::Branch { dst } => format!("{} %label.{} ", output, dst.index(),),
            InstOperand::BranchArgs { dst, .. } => format!("{} %label.{} ", output, dst.index(),),
            InstOperand::CondBranch { arg, dsts } => format!(
                "{} {}, %label.{}, %label.{}",
                output,
                arg.to_string(parent, false),
                dsts[0].index(),
                dsts[1].index(),
            ),
            InstOperand::Call { args } | InstOperand::Gep { args } => {
                args.iter().enumerate().fold(output, |acc, (i, v)| {
                    format!(
                        "{}{}{}",
                        acc,
                        if i == 0 { " " } else { ", " },
                        v.to_string(parent, false)
                    )
                })
            }
            InstOperand::Phi { blocks, args } => {
                blocks
                    .iter()
                    .zip(args.iter())
                    .enumerate()
                    .fold(output, |acc, (i, (b, a))| {
                        format!(
                            "{}{}(%label.{}, {})",
                            acc,
                            if i == 0 { " " } else { ", " },
                            b.index(),
                            a.to_string(parent, false)
                        )
                    })
            }
            InstOperand::None => output,
        }
    }
}

impl Opcode {
    pub fn returns_value(&self) -> bool {
        match self {
            Opcode::Br | Opcode::CondBr | Opcode::Ret | Opcode::Store | Opcode::Call |
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

impl InstOperand {
    pub fn types(&self) -> &[Type] {
        match self {
            Self::Type { ty } => ::core::slice::from_ref(ty),
            _ => &[],
        }
    }

    pub fn types_mut(&mut self) -> &mut [Type] {
        match self {
            Self::Type { ty } => ::core::slice::from_mut(ty),
            _ => &mut [],
        }
    }

    pub fn int_cmp(&self) -> &[ICmpKind] {
        match self {
            Self::IntCmp { cond, .. } => ::core::slice::from_ref(cond),
            _ => &[],
        }
    }

    pub fn float_cmp(&self) -> &[FCmpKind] {
        match self {
            Self::FloatCmp { cond, .. } => ::core::slice::from_ref(cond),
            _ => &[],
        }
    }

    pub fn blocks(&self) -> &[BasicBlockId] {
        match self {
            Self::BranchArgs { dst, .. } | Self::Branch { dst } => ::core::slice::from_ref(dst),
            Self::CondBranch { dsts, .. } => dsts,
            Self::Phi { blocks, .. } => blocks.as_ref(),
            _ => &[],
        }
    }

    pub fn blocks_mut(&mut self) -> &mut [BasicBlockId] {
        match self {
            Self::BranchArgs { dst, .. } | Self::Branch { dst } => ::core::slice::from_mut(dst),
            Self::CondBranch { dsts, .. } => dsts,
            Self::Phi { blocks, .. } => blocks.as_mut(),
            _ => &mut [],
        }
    }

    pub fn args(&self) -> &[Value] {
        match self {
            Self::Store { args } => args,
            Self::Cast { arg }
            | Self::Ret { arg }
            | Self::CondBranch { arg, .. }
            | Self::Load { arg } => ::core::slice::from_ref(arg),
            Self::Binary { args } | Self::IntCmp { args, .. } | Self::FloatCmp { args, .. } => {
                args.as_ref()
            }
            Self::BranchArgs { args, .. }
            | Self::Gep { args }
            | Self::Phi { args, .. }
            | Self::Call { args } => args.as_ref(),
            _ => &[],
        }
    }

    pub fn args_mut(&mut self) -> &mut [Value] {
        match self {
            Self::Store { args } => args,
            Self::Cast { arg }
            | Self::Ret { arg }
            | Self::CondBranch { arg, .. }
            | Self::Load { arg } => ::core::slice::from_mut(arg),
            Self::Binary { args } | Self::IntCmp { args, .. } | Self::FloatCmp { args, .. } => {
                args.as_mut()
            }
            Self::BranchArgs { args, .. }
            | Self::Gep { args }
            | Self::Phi { args, .. }
            | Self::Call { args } => args.as_mut(),
            _ => &mut [],
        }
    }

    pub fn phi_args_mut(&mut self) -> &mut Vec<Value> {
        match self {
            Self::Phi { args, .. } => args,
            _ => panic!(),
        }
    }

    pub fn phi_blocks_mut(&mut self) -> &mut Vec<BasicBlockId> {
        match self {
            Self::Phi { blocks, .. } => blocks,
            _ => panic!(),
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
