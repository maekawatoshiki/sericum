use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

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
    Br(BasicBlockId),
    Ret(DAGNodeId),
    CopyToReg(DAGNodeId, DAGNodeId), // reg, val
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
            self.construct_dag_from_basic_block(func, bb);
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
                }
            };
        }

        for instr_val in bb.iseq_ref().iter() {
            let instr_id = instr_val.get_instr_id().unwrap();
            let instr = &func.instr_table[instr_id];

            match instr.opcode {
                Opcode::Alloca(ref ty) => {
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::FrameIndex(local_count),
                        ty: Some(ty.clone()),
                        next: None,
                    });
                    local_count += 1;
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Load(ref v) => {
                    let vid = self.get_dag_id_from_value(v);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Load(vid),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    make_chain!(id);
                    last_dag_id = Some(id);
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
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
                    last_dag_id = Some(id);
                }
                Opcode::Add(ref v1, ref v2) => {
                    let v1_id = self.get_dag_id_from_value(v1);
                    let v2_id = self.get_dag_id_from_value(v2);
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Add(v1_id, v2_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    make_chain!(id);
                    last_dag_id = Some(id);
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Br(bb) => {
                    let id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Br(bb),
                        ty: None,
                        next: None,
                    });
                    make_chain!(id);
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

        println!("{:?}", self.dag_arena);

        entry_node
    }
}

