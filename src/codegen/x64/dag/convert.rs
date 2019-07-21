use super::node::*;
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct DAGFunction {
    pub func_id: FunctionId,
    pub bb_to_node: FxHashMap<BasicBlockId, DAGNodeId>,
    pub dag_arena: Arena<DAGNode>,
}

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub instr_id_to_dag_node_id: FxHashMap<InstructionId, DAGNodeId>,
}

impl DAGFunction {
    pub fn new(
        func_id: FunctionId,
        bb_to_node: FxHashMap<BasicBlockId, DAGNodeId>,
        dag_arena: Arena<DAGNode>,
    ) -> Self {
        Self {
            func_id,
            bb_to_node,
            dag_arena,
        }
    }
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            instr_id_to_dag_node_id: FxHashMap::default(),
        }
    }

    pub fn construct_dag(&mut self, func_id: FunctionId) -> DAGFunction {
        let func = self.module.function_ref(func_id);
        // let mut dag_func = DAGFunction::new(func_id);
        let mut bb_to_node: FxHashMap<BasicBlockId, DAGNodeId> = FxHashMap::default();
        let mut dag_arena: Arena<DAGNode> = Arena::new();

        for (bb_id, bb) in &func.basic_blocks {
            let id = self.construct_dag_from_basic_block(&mut dag_arena, func, bb);
            bb_to_node.insert(bb_id, id);
            println!("{}", dag_arena[id].to_dot(id, &dag_arena));
        }

        DAGFunction::new(func_id, bb_to_node, dag_arena)
    }

    pub fn get_dag_id_from_value(
        &mut self,
        dag_arena: &mut Arena<DAGNode>,
        v: &Value,
    ) -> DAGNodeId {
        match v {
            Value::Instruction(iv) => self.instr_id_to_dag_node_id[&iv.id],
            Value::Immediate(ImmediateValue::Int32(i)) => dag_arena.alloc(DAGNode::new(
                DAGNodeKind::Constant(ConstantKind::Int32(*i)),
                Some(Type::Int32),
            )),
            Value::Argument(av) => dag_arena.alloc(DAGNode::new(
                DAGNodeKind::FrameIndex(-(av.index as i32 + 1)),
                Some(
                    self.module
                        .function_ref(av.func_id)
                        .get_param_type(av.index)
                        .unwrap()
                        .clone(),
                ),
            )),
            _ => unimplemented!(),
        }
    }

    pub fn construct_dag_from_basic_block(
        &mut self,
        dag_arena: &mut Arena<DAGNode>,
        func: &Function,
        bb: &BasicBlock,
    ) -> DAGNodeId {
        let mut local_count = 0i32;
        let entry_node = dag_arena.alloc(DAGNode::new(DAGNodeKind::Entry, None));
        let mut last_dag_id = Some(entry_node);

        macro_rules! make_chain {
            ($dag_id:expr) => {
                if let Some(dag_id) = last_dag_id {
                    dag_arena[dag_id].next = Some($dag_id);
                    last_dag_id = Some($dag_id);
                }
            };
        }

        for instr_val in bb.iseq_ref().iter() {
            let instr_id = instr_val.get_instr_id().unwrap();
            let instr = &func.instr_table[instr_id];

            match instr.opcode {
                Opcode::Alloca(ref ty) => {
                    local_count += ty.size_in_byte() as i32;
                    let id = dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::FrameIndex(local_count),
                        Some(ty.clone()),
                    ));
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Load(ref v) => {
                    let vid = self.get_dag_id_from_value(dag_arena, v);
                    let load_id = dag_arena
                        .alloc(DAGNode::new(DAGNodeKind::Load(vid), Some(instr.ty.clone())));
                    make_chain!(load_id);
                    self.instr_id_to_dag_node_id.insert(instr_id, load_id);
                }
                Opcode::Store(ref src, ref dst) => {
                    let dst_id = self.get_dag_id_from_value(dag_arena, dst);
                    let src_id = self.get_dag_id_from_value(dag_arena, src);
                    let id =
                        dag_arena.alloc(DAGNode::new(DAGNodeKind::Store(dst_id, src_id), None));
                    make_chain!(id);
                }
                Opcode::Add(ref v1, ref v2) => {
                    let v1_id = self.get_dag_id_from_value(dag_arena, v1);
                    let v2_id = self.get_dag_id_from_value(dag_arena, v2);
                    let add_id = dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Add(v1_id, v2_id),
                        Some(instr.ty.clone()),
                    ));
                    self.instr_id_to_dag_node_id.insert(instr_id, add_id);
                }
                Opcode::Br(bb) => {
                    let id = dag_arena.alloc(DAGNode::new(DAGNodeKind::Br(bb), None));
                    make_chain!(id);
                }
                Opcode::CondBr(ref v, then_, else_) => {
                    let v_id = self.get_dag_id_from_value(dag_arena, v);
                    make_chain!(
                        dag_arena.alloc(DAGNode::new(DAGNodeKind::BrCond(v_id, then_), None,))
                    );
                    make_chain!(dag_arena.alloc(DAGNode::new(DAGNodeKind::Br(else_), None,)))
                }
                Opcode::ICmp(ref c, ref v1, ref v2) => {
                    let v1_id = self.get_dag_id_from_value(dag_arena, v1);
                    let v2_id = self.get_dag_id_from_value(dag_arena, v2);
                    let id = dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Setcc((*c).into(), v1_id, v2_id),
                        Some(instr.ty.clone()),
                    ));
                    self.instr_id_to_dag_node_id.insert(instr_id, id);
                }
                Opcode::Ret(ref v) => {
                    let v_id = self.get_dag_id_from_value(dag_arena, v);
                    let id = dag_arena.alloc(DAGNode::new(DAGNodeKind::Ret(v_id), None));
                    make_chain!(id);
                }
                ref e => unimplemented!("{:?}", e),
            }
        }

        entry_node
    }
}
