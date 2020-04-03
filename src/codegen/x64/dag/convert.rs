// Convert IR to architecture-independent DAG form

use super::super::{frame_object::*, register::*};
use super::{basic_block::*, function::*, module::*, node::*};
use crate::ir::{
    basic_block::*, function::*, liveness::*, module::*, opcode::*, types::*, value::*,
};
use crate::util::allocator::{Raw, RawAllocator};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub types: Types, // copied from module
    pub inst_id_node_id: FxHashMap<InstructionId, Raw<DAGNode>>,
    pub inst_id_to_copy_node: FxHashMap<InstructionId, Raw<DAGNode>>,
    pub cur_conversion_info: Option<ConversionInfo>,
}

pub struct ConversionInfo {
    pub dag_heap: RawAllocator<DAGNode>,
    // pub dag_arena: Arena<DAGNode>,
    pub local_mgr: LocalVariables,
    pub bb_to_dag_bb: FxHashMap<BasicBlockId, DAGBasicBlockId>,
    pub last_chain_node: Option<Raw<DAGNode>>,
    pub vreg_gen: VirtRegGen,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        IRLivenessAnalyzer::new(&module).analyze();

        Self {
            module,
            types: module.types.clone(),
            inst_id_node_id: FxHashMap::default(),
            inst_id_to_copy_node: FxHashMap::default(),
            cur_conversion_info: None,
        }
    }

    pub fn convert_module(&mut self) -> DAGModule {
        let mut dag_module = DAGModule::new(self.module.name.as_str());
        for (f_id, _) in &self.module.functions {
            dag_module.add_function(self.construct_dag(f_id));
        }
        dag_module.types = self.types.clone();
        dag_module
    }

    pub fn construct_dag(&mut self, func_id: FunctionId) -> DAGFunction {
        self.cur_conversion_info = Some(ConversionInfo::new());
        self.inst_id_node_id.clear();

        let func = self.module.function_ref(func_id);

        debug!(println!(
            "{}: dump function: \n{}",
            file!(),
            self.module.dump(func),
        ));

        let mut dag_bb_arena: Arena<DAGBasicBlock> = Arena::new();
        let mut dag_bb_list: Vec<DAGBasicBlockId> = vec![];

        for bb_id in &func.basic_blocks {
            let dag_bb_id = dag_bb_arena.alloc(DAGBasicBlock::new());
            self.cur_conv_info_mut()
                .bb_to_dag_bb
                .insert(*bb_id, dag_bb_id);
            dag_bb_list.push(dag_bb_id);
        }

        self.set_dag_bb_pred_and_succ(func, &mut dag_bb_arena);

        for bb_id in &func.basic_blocks {
            self.inst_id_to_copy_node.clear();

            let bb = &func.basic_block_arena[*bb_id];
            let id = self.construct_dag_from_basic_block(func, bb);
            dag_bb_arena[self.cur_conv_info_ref().get_dag_bb(*bb_id)].set_entry(id);
        }

        let conv_info = ::std::mem::replace(&mut self.cur_conversion_info, None).unwrap();
        DAGFunction::new(
            func,
            conv_info.dag_heap,
            dag_bb_arena,
            dag_bb_list,
            conv_info.local_mgr,
            conv_info.vreg_gen,
        )
    }

    pub fn get_dag_id_from_value(&mut self, v: &Value, arg_load: bool) -> Raw<DAGNode> {
        match v {
            Value::Instruction(iv) => self.inst_id_node_id[&iv.id],
            Value::Immediate(imm) => {
                let imm = match imm {
                    ImmediateValue::Int32(i) => ConstantKind::Int32(*i),
                    ImmediateValue::Int64(i) => ConstantKind::Int64(*i),
                    ImmediateValue::F64(f) => ConstantKind::F64(*f),
                };
                self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Constant(imm)),
                    vec![],
                    imm.get_type(),
                ))
            }
            Value::Argument(av) => {
                let ty = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_type(&self.module.types, av.index)
                    .unwrap();
                let fi = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::FrameIndex(FrameIndexInfo::new(
                        ty.clone(),
                        FrameIndexKind::Arg(av.index),
                    ))),
                    vec![],
                    ty.clone(),
                ));
                if arg_load {
                    let load_id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Load),
                        vec![fi],
                        ty.clone(),
                    ));
                    // self.make_chain(load_id);
                    load_id
                } else {
                    fi
                }
            }
            Value::Function(FunctionValue { func_id }) => {
                let f = self.module.function_ref(*func_id);
                self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Address(AddressKind::FunctionName(
                        f.name.to_string(),
                    ))),
                    vec![],
                    Type::Void, // TODO
                ))
            }
            Value::None => self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                NodeKind::None,
                vec![],
                Type::Void,
            )),
        }
    }

    pub fn construct_dag_from_basic_block(
        &mut self,
        func: &Function,
        bb: &BasicBlock,
    ) -> Raw<DAGNode> {
        let entry_node = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
            NodeKind::IR(IRNodeKind::Entry),
            vec![],
            Type::Void,
        ));
        self.cur_conv_info_mut().last_chain_node = Some(entry_node);

        macro_rules! make_chain {
            ($dag_id:expr) => {{
                let dag = $dag_id;
                let conv_info = self.cur_conv_info_mut();
                if let Some(last_node) = &mut conv_info.last_chain_node {
                    last_node.next = Some(dag);
                    *last_node = dag;
                }
            }};
        }

        for inst_val in bb.iseq_ref().iter() {
            let inst_id = inst_val.get_inst_id().unwrap();
            let inst = &func.inst_table[inst_id];

            match inst.opcode {
                Opcode::Alloca(ref ty) => {
                    let fi = self.cur_conv_info_mut_with(|c| {
                        let frinfo = c.local_mgr.alloc(ty);
                        c.dag_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::FrameIndex(frinfo.clone())), // TODO
                            vec![],
                            ty.clone(),
                        ))
                    });
                    self.inst_id_node_id.insert(inst_id, fi);
                }
                Opcode::Load(ref v) => {
                    let v = self.get_dag_id_from_value(v, true);
                    let load_id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Load),
                        vec![v],
                        inst.ty.clone(),
                    ));

                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(load_id);
                        self.inst_id_to_copy_node.insert(inst_id, copy_from_reg);
                        self.inst_id_node_id.insert(inst_id, load_id);
                    } else {
                        // make_chain!(load_id);
                        self.inst_id_node_id.insert(inst_id, load_id);
                    }
                }
                Opcode::Store(ref src, ref dst) => {
                    let dst = self.get_dag_id_from_value(dst, true);
                    let src = self.get_dag_id_from_value(src, true);
                    let id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Store),
                        vec![dst, src],
                        Type::Void,
                    ));
                    make_chain!(id);
                }
                Opcode::GetElementPtr(ref ptr, ref indices) => {
                    let gep = self.construct_dag_for_gep(inst, ptr, indices);
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        make_chain!(gep);
                    }
                    self.inst_id_node_id.insert(inst_id, gep);
                }
                Opcode::Call(ref f, ref args) => {
                    let mut operands: Vec<Raw<DAGNode>> = args
                        .iter()
                        .map(|a| self.get_dag_id_from_value(a, true))
                        .collect();
                    operands.insert(0, self.get_dag_id_from_value(f, true));
                    let id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Call),
                        operands,
                        inst.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_to_copy_node.insert(inst_id, copy_from_reg);
                        self.inst_id_node_id.insert(inst_id, id);
                    } else {
                        if inst.ty == Type::Void {
                            make_chain!(id);
                        }
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Add(ref v1, ref v2)
                | Opcode::Sub(ref v1, ref v2)
                | Opcode::Mul(ref v1, ref v2)
                | Opcode::Rem(ref v1, ref v2) => {
                    let v1 = self.get_dag_id_from_value(v1, true);
                    let v2 = self.get_dag_id_from_value(v2, true);
                    let bin_id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        match inst.opcode {
                            Opcode::Add(_, _) => NodeKind::IR(IRNodeKind::Add),
                            Opcode::Sub(_, _) => NodeKind::IR(IRNodeKind::Sub),
                            Opcode::Mul(_, _) => NodeKind::IR(IRNodeKind::Mul),
                            Opcode::Rem(_, _) => NodeKind::IR(IRNodeKind::Rem),
                            _ => unreachable!(),
                        },
                        vec![v1, v2],
                        inst.ty.clone(),
                    ));

                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(bin_id);
                        self.inst_id_to_copy_node.insert(inst_id, copy_from_reg);
                        self.inst_id_node_id.insert(inst_id, bin_id);
                    } else {
                        self.inst_id_node_id.insert(inst_id, bin_id);
                    }
                }
                Opcode::Br(bb) => make_chain!(self.cur_conv_info_mut_with(|c| {
                    let bb = c.dag_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::BasicBlock(c.get_dag_bb(bb))),
                        vec![],
                        Type::Void,
                    ));
                    c.dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Br),
                        vec![bb],
                        Type::Void,
                    ))
                })),
                Opcode::CondBr(ref v, then_, else_) => {
                    let v = self.get_dag_id_from_value(v, true);
                    make_chain!({
                        let c = self.cur_conv_info_mut();
                        let bb = c.dag_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::BasicBlock(c.get_dag_bb(then_))),
                            vec![],
                            Type::Void,
                        ));
                        c.dag_heap.alloc(DAGNode::new(
                            NodeKind::IR(IRNodeKind::BrCond),
                            vec![v, bb],
                            Type::Void,
                        ))
                    });
                    make_chain!(self.cur_conv_info_mut_with(|c| {
                        let bb = c.dag_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::BasicBlock(c.get_dag_bb(else_))),
                            vec![],
                            Type::Void,
                        ));
                        c.dag_heap.alloc(DAGNode::new(
                            NodeKind::IR(IRNodeKind::Br),
                            vec![bb],
                            Type::Void,
                        ))
                    }));
                }
                Opcode::ICmp(ref c, ref v1, ref v2) => {
                    let v1 = self.get_dag_id_from_value(v1, true);
                    let v2 = self.get_dag_id_from_value(v2, true);
                    let cond = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::CondKind((*c).into())),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Setcc),
                        vec![cond, v1, v2],
                        inst.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_to_copy_node.insert(inst_id, copy_from_reg);
                        self.inst_id_node_id.insert(inst_id, id);
                    } else {
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Phi(ref pairs) => {
                    let mut operands = vec![];
                    for (val, bb) in pairs {
                        // Remove CopyFromReg if necessary
                        let val = self.get_dag_id_from_value(val, true);
                        operands.push(match val.kind {
                            NodeKind::IR(IRNodeKind::CopyFromReg) => val.operand[0],
                            _ => val,
                        });

                        operands.push(self.cur_conv_info_mut_with(|c| {
                            c.dag_heap.alloc(DAGNode::new(
                                NodeKind::Operand(OperandNodeKind::BasicBlock(c.get_dag_bb(*bb))),
                                vec![],
                                Type::Void,
                            ))
                        }))
                    }
                    let id = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Phi),
                        operands,
                        inst.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_to_copy_node.insert(inst_id, copy_from_reg);
                        self.inst_id_node_id.insert(inst_id, id);
                    } else {
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Ret(ref v) => {
                    let v = self.get_dag_id_from_value(v, true);
                    make_chain!(self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Ret),
                        vec![v],
                        Type::Void
                    )))
                }
            }
        }

        for inst_liveout in &bb.liveness.borrow().live_out {
            if !self.inst_id_to_copy_node.contains_key(inst_liveout) {
                continue;
            }

            let copy_to_live_out = *self.inst_id_to_copy_node.get(inst_liveout).unwrap();
            self.inst_id_node_id.insert(*inst_liveout, copy_to_live_out);
        }

        entry_node
    }

    fn construct_dag_for_gep(
        &mut self,
        _inst: &Instruction,
        ptr: &Value,
        indices: &[Value],
    ) -> Raw<DAGNode> {
        let mut gep = self.get_dag_id_from_value(ptr, false);
        let mut ty = ptr.get_type(self.module);

        for idx in indices {
            ty = self.types.get_element_ty(ty, Some(idx)).unwrap();
            // ty = ty.get_element_ty(Some(idx)).unwrap();

            let idx = self.get_dag_id_from_value(idx, true);
            let heap = &mut self.cur_conversion_info.as_mut().unwrap().dag_heap;
            let idx = match idx.kind {
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(i))) => {
                    heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            i * ty.size_in_byte(&self.module.types) as i32,
                        ))),
                        vec![],
                        Type::Int32,
                    ))
                }
                NodeKind::Operand(OperandNodeKind::CondKind(_))
                | NodeKind::Operand(OperandNodeKind::FrameIndex(_))
                | NodeKind::Operand(OperandNodeKind::Address(_))
                | NodeKind::Operand(OperandNodeKind::BasicBlock(_)) => idx,
                _ => {
                    let tysz = heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            ty.size_in_byte(&self.module.types) as i32,
                        ))),
                        vec![],
                        Type::Int32,
                    ));
                    let cast = sext_if_necessary(&self.module.types, heap, idx, Type::Int64);
                    heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Mul),
                        vec![cast, tysz],
                        Type::Int64, // TODO
                    ))
                }
            };

            let ptr_ty = self.types.new_pointer_ty(ty);
            gep = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Add),
                vec![gep, idx],
                ptr_ty, // Type::Pointer(Box::new(ty.clone())),
            ));
        }

        gep
    }

    fn cur_conv_info_mut_with<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut ConversionInfo) -> T,
    {
        f(self.cur_conversion_info.as_mut().unwrap())
    }

    fn cur_conv_info_mut(&mut self) -> &mut ConversionInfo {
        self.cur_conversion_info.as_mut().unwrap()
    }

    fn cur_conv_info_ref(&mut self) -> &ConversionInfo {
        self.cur_conversion_info.as_ref().unwrap()
    }

    fn set_dag_bb_pred_and_succ(
        &mut self,
        func: &Function,
        dag_bb_arena: &mut Arena<DAGBasicBlock>,
    ) {
        let conv_info = self.cur_conv_info_ref();
        for (bb, dag_bb) in &conv_info.bb_to_dag_bb {
            dag_bb_arena[*dag_bb].pred = func.basic_block_arena[*bb]
                .pred
                .iter()
                .map(|bb| conv_info.get_dag_bb(*bb))
                .collect();
            dag_bb_arena[*dag_bb].succ = func.basic_block_arena[*bb]
                .succ
                .iter()
                .map(|bb| conv_info.get_dag_bb(*bb))
                .collect();
        }
    }

    fn make_chain(&mut self, node: Raw<DAGNode>) {
        let conv_info = self.cur_conv_info_mut();
        if let Some(last_node) = &mut conv_info.last_chain_node {
            last_node.next = Some(node);
            *last_node = node;
        }
    }

    fn make_chain_with_copying(&mut self, node: Raw<DAGNode>) -> Raw<DAGNode> {
        let copy_to_live_out = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
            NodeKind::IR(IRNodeKind::CopyToLiveOut),
            vec![node],
            node.ty.clone(),
        ));

        self.make_chain(copy_to_live_out);

        copy_to_live_out
    }
}

fn sext_if_necessary(
    tys: &Types,
    heap: &mut RawAllocator<DAGNode>,
    node: Raw<DAGNode>,
    to: Type,
) -> Raw<DAGNode> {
    if node.ty == to || node.ty.size_in_bits(tys) >= to.size_in_bits(tys) {
        return node;
    }

    heap.alloc(DAGNode::new(NodeKind::IR(IRNodeKind::Sext), vec![node], to))
}

impl ConversionInfo {
    pub fn new() -> Self {
        ConversionInfo {
            dag_heap: RawAllocator::new(),
            // dag_heap: Arena::new(),
            local_mgr: LocalVariables::new(),
            bb_to_dag_bb: FxHashMap::default(),
            last_chain_node: None,
            vreg_gen: VirtRegGen::new(),
        }
    }

    pub fn get_dag_bb(&self, bb_id: BasicBlockId) -> DAGBasicBlockId {
        *self.bb_to_dag_bb.get(&bb_id).unwrap()
    }
}
