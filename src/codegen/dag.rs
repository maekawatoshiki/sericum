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
    CopyToReg(RegisterKind, DAGNodeId), // reg, val
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
            let _id = self.construct_dag_from_basic_block(func, bb);
            // println!("{}", self.dag_arena[id].to_dot(id, &self.dag_arena));
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
                    let vreg_id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::CopyToReg(RegisterKind::VReg(1), load_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    make_chain!(load_id);
                    last_dag_id = Some(load_id);
                    self.instr_id_to_dag_node_id.insert(instr_id, vreg_id);
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
                    let add_id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::Add(v1_id, v2_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    let vreg_id = self.dag_arena.alloc(DAGNode {
                        kind: DAGNodeKind::CopyToReg(RegisterKind::VReg(1), add_id),
                        ty: Some(instr.ty.clone()),
                        next: None,
                    });
                    make_chain!(add_id);
                    last_dag_id = Some(add_id);
                    self.instr_id_to_dag_node_id.insert(instr_id, vreg_id);
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

// impl DAGNode {
//     pub fn to_dot(&self, self_id: DAGNodeId, arena: &Arena<DAGNode>) -> String {
//         format!("digraph graph {{ {} }}", self.to_dot_sub(self_id, arena))
//     }
//
//     fn to_dot_sub(&self, self_id: DAGNodeId, arena: &Arena<DAGNode>) -> String {
//         match self.kind {
//             DAGNodeKind::Entry => format!(
//                 "\nEntry [shape=record,shape=Mrecord,label=\"{{Entry}}\"];\n{}",
//                 match self.next {
//                     Some(next) => {
//                         format!("Entry -> {}"
//                         arena[next].to_dot_sub(next, arena)
//                     }
//                     None => "".to_string(),
//                 }
//             ),
//             DAGNodeKind::Load(dagid) => format!(
//                 "\nLoad{} [shape=record,shape=Mrecord,label=\"{{Load|{}}}\"];\n{}",
//                 self_id.index(),
//                 self.ty.as_ref().unwrap().to_string(),
//                 match self.next {
//                     Some(next) => arena[next].to_dot_sub(next, arena),
//                     None => "".to_string(),
//                 }
//             ),
//             _ => "".to_string(),
//         }
//     }
// }
