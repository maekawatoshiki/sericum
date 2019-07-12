use super::{basic_block::*, module::*, types::*, value::*};
use id_arena::*;
use std::{cell::RefCell, rc::Rc};
// use rustc_hash::FxHashMap;

pub type InstructionId = Id<Instruction>;
pub type VirtualRegister = usize;
pub type RegisterAllocInfoRef = Rc<RefCell<RegisterAllocInfo>>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register(usize);

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub ty: Type,
    pub vreg: VirtualRegister,
    pub reg: RegisterAllocInfoRef,
}

#[derive(Debug, Clone)]
pub struct RegisterAllocInfo {
    pub reg: Option<Register>,
    pub spill: bool,
    pub last_use: Option<InstructionId>,
}

#[derive(Clone, Debug)]
pub enum Opcode {
    Alloca(Type),
    Load(Value),
    Store(Value, Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Rem(Value, Value),
    ICmp(ICmpKind, Value, Value),
    Br(BasicBlockId),
    CondBr(Value, BasicBlockId, BasicBlockId),
    Phi(Vec<(Value, BasicBlockId)>),
    Call(Value, Vec<Value>),
    Ret(Value),
}

#[derive(Clone, Debug)]
pub enum ICmpKind {
    Eq,
    Le,
    // Ne,
}

impl Instruction {
    pub fn new(opcode: Opcode, ty: Type, vreg: VirtualRegister) -> Self {
        Self {
            opcode,
            ty,
            vreg,
            reg: Rc::new(RefCell::new(RegisterAllocInfo::new())),
        }
    }

    pub fn to_string(&self, m: &Module) -> String {
        self.opcode.to_string(m)
    }

    pub fn set_last_use(&self, last_use: Option<InstructionId>) {
        self.reg.borrow_mut().last_use = last_use;
    }

    pub fn set_phy_reg(&self, reg: usize, spill: bool) {
        let mut reg_info = self.reg.borrow_mut();
        reg_info.reg = Some(Register(reg));
        reg_info.spill = spill;
    }

    pub fn can_be_eliminated(&self) -> bool {
        let no_phy_reg = self.reg.borrow().reg.is_none();
        self.opcode.must_return_value() && no_phy_reg
    }
}

impl RegisterAllocInfo {
    pub fn new() -> Self {
        Self {
            reg: None,
            spill: false,
            last_use: None,
        }
    }
}

impl Opcode {
    pub fn must_return_value(&self) -> bool {
        match self {
            Opcode::Br(_) | Opcode::CondBr(_, _, _) | Opcode::Ret(_) | Opcode::Store(_, _) |
                /* alloca doesn't occupy register = */ Opcode::Alloca(_)=> false,
            _ => true,
        }
    }

    pub fn to_string(&self, m: &Module) -> String {
        match self {
            Opcode::Alloca(ty) => format!("alloca {}", ty.to_string()),
            Opcode::Load(v) => format!("load {}", v.to_string(m, false)),
            Opcode::Store(src, dst) => format!(
                "store {}, {}",
                src.to_string(m, false),
                dst.to_string(m, false)
            ),
            Opcode::Add(v1, v2) => {
                format!("add {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::Sub(v1, v2) => {
                format!("sub {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::Mul(v1, v2) => {
                format!("mul {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::Rem(v1, v2) => {
                format!("rem {}, {}", v1.to_string(m, false), v2.to_string(m, false))
            }
            Opcode::ICmp(kind, v1, v2) => format!(
                "icmp {} {}, {}",
                kind.as_str(),
                v1.to_string(m, false),
                v2.to_string(m, false)
            ),
            Opcode::Br(id) => format!("br %label.{}", id.index()),
            Opcode::CondBr(v, id1, id2) => format!(
                "br {} %label.{}, %label.{}",
                v.to_string(m, false),
                id1.index(),
                id2.index()
            ),
            Opcode::Phi(pairs) => pairs.iter().fold("phi".to_string(), |s, (val, bb)| {
                format!("{} [{}, %label.{}]", s, val.to_string(m, false), bb.index())
            }),
            Opcode::Call(v, args) => format!(
                "call {}({})",
                v.to_string(m, false),
                args.iter().fold("".to_string(), |s, val| format!(
                    "{}{}, ",
                    s,
                    val.to_string(m, false)
                ))
            ),
            Opcode::Ret(v) => format!("ret {}", v.to_string(m, false)),
        }
    }
}

impl ICmpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ICmpKind::Eq => "eq",
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
