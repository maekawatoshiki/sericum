use super::{basic_block::*, function::*, module::*, node::*};
use crate::ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub instr_id_to_dag_node_id: FxHashMap<InstructionId, DAGNodeId>,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            instr_id_to_dag_node_id: FxHashMap::default(),
        }
    }

    pub fn convert_module(&mut self) -> DAGModule {
        let mut dag_module = DAGModule::new(self.module.name.as_str());
        for (f_id, _) in &self.module.functions {
            dag_module.add_function(self.construct_dag(f_id));
        }
        dag_module
    }

    pub fn construct_dag(&mut self, func_id: FunctionId) -> DAGFunction {
        let func = self.module.function_ref(func_id);
        let mut dag_arena: Arena<DAGNode> = Arena::new();
        let mut dag_bb_arena: Arena<DAGBasicBlock> = Arena::new();
        let mut bb_to_dag_bb: FxHashMap<BasicBlockId, DAGBasicBlockId> = FxHashMap::default();

        for (bb_id, _) in &func.basic_blocks {
            bb_to_dag_bb.insert(bb_id, dag_bb_arena.alloc(DAGBasicBlock::new()));
        }

        for (bb, dag_bb) in &bb_to_dag_bb {
            dag_bb_arena[*dag_bb].pred = func.basic_blocks[*bb]
                .pred
                .iter()
                .map(|bb| *bb_to_dag_bb.get(bb).unwrap())
                .collect();
            dag_bb_arena[*dag_bb].succ = func.basic_blocks[*bb]
                .succ
                .iter()
                .map(|bb| *bb_to_dag_bb.get(bb).unwrap())
                .collect();
        }

        for (bb_id, bb) in &func.basic_blocks {
            let id = self.construct_dag_from_basic_block(&mut dag_arena, &bb_to_dag_bb, func, bb);
            dag_bb_arena[*bb_to_dag_bb.get(&bb_id).unwrap()].set_entry(id);
            when_debug!(println!("{}", dag_arena[id].to_dot(id, &dag_arena)));
        }

        DAGFunction::new(func, dag_arena, dag_bb_arena)
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
            Value::Argument(av) => {
                let ty = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_type(av.index)
                    .unwrap();
                dag_arena.alloc(DAGNode::new(
                    DAGNodeKind::FrameIndex(-(av.index as i32 + 1), ty.clone()),
                    Some(ty.clone()),
                ))
            }
            _ => unimplemented!(),
        }
    }

    pub fn construct_dag_from_basic_block(
        &mut self,
        dag_arena: &mut Arena<DAGNode>,
        bb_to_dag_bb: &FxHashMap<BasicBlockId, DAGBasicBlockId>,
        func: &Function,
        bb: &BasicBlock,
    ) -> DAGNodeId {
        let mut local_count = 0i32;
        let entry_node = dag_arena.alloc(DAGNode::new(DAGNodeKind::Entry, None));
        let mut last_dag_id = Some(entry_node);

        macro_rules! make_chain {
            ($dag_id:expr) => {
                if let Some(id_) = last_dag_id {
                    dag_arena[id_].next = Some($dag_id);
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
                    let id = dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::FrameIndex(local_count, ty.clone()),
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
                    let id = dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Br(*bb_to_dag_bb.get(&bb).unwrap()),
                        None,
                    ));
                    make_chain!(id);
                }
                Opcode::CondBr(ref v, then_, else_) => {
                    let v_id = self.get_dag_id_from_value(dag_arena, v);
                    make_chain!(dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::BrCond(v_id, *bb_to_dag_bb.get(&then_).unwrap()),
                        None,
                    )));
                    make_chain!(dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Br(*bb_to_dag_bb.get(&else_).unwrap()),
                        None,
                    )))
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
