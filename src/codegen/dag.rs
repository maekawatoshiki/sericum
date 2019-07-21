use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::{FxHashMap, FxHashSet};

pub type DAGNodeId = Id<DAGNode>;

#[derive(Debug, Clone, PartialEq)]
pub struct DAGNode {
    pub kind: DAGNodeKind,
    pub ty: Option<Type>,
    pub next: Option<DAGNodeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DAGNodeKind {
    Entry,
    Load(DAGNodeId),
    Store(DAGNodeId, DAGNodeId), // dst, src
    Add(DAGNodeId, DAGNodeId),
    Setcc(CondKind, DAGNodeId, DAGNodeId),
    BrCond(DAGNodeId, BasicBlockId),
    Br(BasicBlockId),
    Ret(DAGNodeId),

    FrameIndex(i32),
    Register(RegisterKind),
    Constant(ConstantKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegisterKind {
    VReg(VirtualRegister),
    Reg(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantKind {
    Int32(i32),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CondKind {
    Eq,
    Le,
}

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub dag_arena: Arena<DAGNode>,
    pub instr_id_to_dag_node_id: FxHashMap<InstructionId, DAGNodeId>,
    // func_id: FunctionId,
    // cur_bb: Option<BasicBlockId>,
    // insert_point: usize,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            dag_arena: Arena::new(),
            instr_id_to_dag_node_id: FxHashMap::default(),
        }
    }

    pub fn construct_dag(&mut self, func_id: FunctionId) {
        let func = self.module.function_ref(func_id);

        for (_, bb) in &func.basic_blocks {
            let id = self.construct_dag_from_basic_block(func, bb);
            println!("{}", self.dag_arena[id].to_dot(id, &self.dag_arena));
        }
    }

    pub fn get_dag_id_from_value(&mut self, v: &Value) -> DAGNodeId {
        match v {
            Value::Instruction(iv) => self.instr_id_to_dag_node_id[&iv.id],
            Value::Immediate(ImmediateValue::Int32(i)) => self.dag_arena.alloc(DAGNode {
                kind: DAGNodeKind::Constant(ConstantKind::Int32(*i)),
                ty: Some(Type::Int32),
                next: None,
            }),
            Value::Argument(av) => self.dag_arena.alloc(DAGNode {
                kind: DAGNodeKind::FrameIndex(-(av.index as i32 + 1)),
                ty: Some(
                    self.module
                        .function_ref(av.func_id)
                        .get_param_type(av.index)
                        .unwrap()
                        .clone(),
                ),
                next: None,
            }),
            _ => unimplemented!(),
        }
    }

    pub fn construct_dag_from_basic_block(
        &mut self,
        func: &Function,
        bb: &BasicBlock,
    ) -> DAGNodeId {
        let mut local_count = 0i32;
        let entry_node = self.dag_arena.alloc(DAGNode {
            kind: DAGNodeKind::Entry,
            ty: None,
            next: None,
        });
        let mut last_dag_id = Some(entry_node);

        macro_rules! make_chain {
            ($dag_id:expr) => {
                if let Some(dag_id) = last_dag_id {
                    self.dag_arena[dag_id].next = Some($dag_id);
                    last_dag_id = Some($dag_id);
                }
            };
        }

        for instr_val in bb.iseq_ref().iter() {
            let instr_id = instr_val.get_instr_id().unwrap();
            let instr = &func.instr_table[instr_id];

            match instr.opcode {
                Opcode::Alloca(ref ty) => {
                    local_count += 1;
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::FrameIndex(local_count),
                        ty: Some(ty.clone()),
                        next: None,
                    });
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Load(ref v) => {
                    let vid = self.get_dag_id_from_value(v);
                    let load_id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Load(vid),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    make_chain!(load_id);
                    self.instr_id_to_dag_node_id.insert(instr_id, load_id);
                }
                Opcode::Store(ref src, ref dst) => {
                    let dst_id = self.get_dag_id_from_value(dst);
                    let src_id = self.get_dag_id_from_value(src);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Store(dst_id, src_id),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
                }
                Opcode::Add(ref v1, ref v2) => {
                    let v1_id = self.get_dag_id_from_value(v1);
                    let v2_id = self.get_dag_id_from_value(v2);
                    let add_id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Add(v1_id, v2_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    self.instr_id_to_dag_node_id.insert(instr_id, add_id);
                }
                Opcode::Br(bb) => {
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Br(bb),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
                }
                Opcode::CondBr(ref v, then_, else_) => {
                    let v_id = self.get_dag_id_from_value(v);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::BrCond(v_id, then_),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Br(else_),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
                }
                Opcode::ICmp(ref c, ref v1, ref v2) => {
                    let v1_id = self.get_dag_id_from_value(v1);
                    let v2_id = self.get_dag_id_from_value(v2);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Setcc((*c).into(), v1_id, v2_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Ret(ref v) => {
                    let v_id = self.get_dag_id_from_value(v);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Ret(v_id),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
                }
                ref e => unimplemented!("{:?}", e),
            }
        }

        entry_node
    }
}

impl Into<CondKind> for ICmpKind {
    fn into(self) -> CondKind {
        match self {
            ICmpKind::Eq => CondKind::Eq,
            ICmpKind::Le => CondKind::Le,
        }
    }
}

impl DAGNode {
    pub fn to_dot(&self, self_id: DAGNodeId, arena: &Arena<DAGNode>) -> String {
        let mut head = FxHashSet::default();
        let mut mark = FxHashSet::default();
        self.to_dot_sub(&mut head, &mut mark, self_id, arena);
        format!(
            "digraph g {{ {} }}",
            head.iter().fold("".to_string(), |mut s, x| {
                s += x;
                s.push('\n');
                s
            })
        )
    }

    fn to_dot_sub(
        &self,
        head: &mut FxHashSet<String>,
        mark: &mut FxHashSet<DAGNodeId>,
        self_id: DAGNodeId,
        arena: &Arena<DAGNode>,
    ) {
        if !mark.insert(self_id) {
            return;
        }

        match self.kind {
            DAGNodeKind::Entry => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Entry}}\"];",
                    self_id.index()
                ));
            }
            DAGNodeKind::Load(dagid) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Load|{}}}\"];",
                    self_id.index(),
                    self.ty.as_ref().unwrap().to_string(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];",
                    self_id.index(),
                    dagid.index(),
                ));
                arena[dagid].to_dot_sub(head, mark, dagid, arena);
            }
            DAGNodeKind::Store(op1, op2) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Store}}\"];",
                    self_id.index(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];
                    instr{} -> instr{} [label=\"1\" color=\"#1E92FF\"];",
                    self_id.index(),
                    op1.index(),
                    self_id.index(),
                    op2.index(),
                ));
                arena[op1].to_dot_sub(head, mark, op1, arena);
                arena[op2].to_dot_sub(head, mark, op2, arena);
            }
            DAGNodeKind::Add(op1, op2) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Add|{}}}\"];",
                    self_id.index(),
                    self.ty.as_ref().unwrap().to_string(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];
                    instr{} -> instr{} [label=\"1\" color=\"#1E92FF\"];",
                    self_id.index(),
                    op1.index(),
                    self_id.index(),
                    op2.index(),
                ));
                arena[op1].to_dot_sub(head, mark, op1, arena);
                arena[op2].to_dot_sub(head, mark, op2, arena);
            }
            DAGNodeKind::Setcc(c, op1, op2) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Setcc|{:?}|{}}}\"];",
                    self_id.index(),
                    c,
                    self.ty.as_ref().unwrap().to_string(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];
                    instr{} -> instr{} [label=\"1\" color=\"#1E92FF\"];",
                    self_id.index(),
                    op1.index(),
                    self_id.index(),
                    op2.index(),
                ));
                arena[op1].to_dot_sub(head, mark, op1, arena);
                arena[op2].to_dot_sub(head, mark, op2, arena);
            }
            DAGNodeKind::BrCond(v, bb) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{BrCond}}\"];",
                    self_id.index(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];",
                    self_id.index(),
                    v.index()
                ));
                head.insert(format!(
                    "instr{} -> branch{} [color=\"#fe8833\"];",
                    self_id.index(),
                    bb.index()
                ));
                arena[v].to_dot_sub(head, mark, v, arena);
            }
            DAGNodeKind::Br(bb) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Br}}\"];",
                    self_id.index(),
                ));
                head.insert(format!(
                    "instr{} -> branch{} [color=\"#fe8833\"];",
                    self_id.index(),
                    bb.index()
                ));
            }
            DAGNodeKind::Ret(v) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Ret}}\"];",
                    self_id.index(),
                ));
                head.insert(format!(
                    "instr{} -> instr{} [label=\"0\" color=\"#1E92FF\"];",
                    self_id.index(),
                    v.index()
                ));
                arena[v].to_dot_sub(head, mark, v, arena);
            }
            DAGNodeKind::FrameIndex(i) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{FrameIndex:{}}}\"];",
                    self_id.index(),
                    i
                ));
            }
            DAGNodeKind::Register(ref r) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Register:{:?}}}\"];",
                    self_id.index(),
                    r
                ));
            }
            DAGNodeKind::Constant(ref c) => {
                head.insert(format!(
                    "instr{} [shape=record,shape=Mrecord,label=\"{{Constant:{:?}}}\"];",
                    self_id.index(),
                    c
                ));
            }
        }

        some_then!(next, self.next, {
            head.insert(format!(
                "instr{} -> instr{} [label=\"chain\"];",
                self_id.index(),
                next.index(),
            ));
            arena[next].to_dot_sub(head, mark, next, arena)
        });
    }
}
