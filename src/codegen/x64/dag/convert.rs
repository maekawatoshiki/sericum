// Convert IR to architecture-independent DAG form
// TODO: refactor

use super::super::{frame_object::*, register::*};
use super::{basic_block::*, function::*, module::*, node::*};
use crate::ir::{
    basic_block::*, function::*, liveness::*, module::*, opcode::*, types::*, value::*,
};
use crate::util::allocator::Raw;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::mem;

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub types: Types, // copied from module
    pub inst_id_node_id: FxHashMap<InstructionId, Raw<DAGNode>>,
    pub cur_conversion_info: Option<ConversionInfo>,
}

pub struct ConversionInfo {
    pub dag_heap: DAGHeap,
    pub local_mgr: LocalVariables,
    pub bb_to_dag_bb: FxHashMap<BasicBlockId, DAGBasicBlockId>,
    pub last_chain_node: Option<Raw<DAGNode>>,
    pub vreg_gen: VirtRegGen,
    pub regs_info: RegistersInfo,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        IRLivenessAnalyzer::new(&module).analyze();

        Self {
            module,
            types: module.types.clone(),
            inst_id_node_id: FxHashMap::default(),
            cur_conversion_info: None,
        }
    }

    pub fn convert_module(mut self) -> DAGModule {
        let mut functions = Arena::new();

        for (f_id, _) in &self.module.functions {
            functions.alloc(self.construct_dag(f_id));
        }

        DAGModule {
            name: self.module.name.clone(),
            functions,
            types: self.types,
        }
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

        for bb_id in &func.basic_blocks.order {
            let dag_bb_id = dag_bb_arena.alloc(DAGBasicBlock::new());
            self.cur_conv_info_mut()
                .bb_to_dag_bb
                .insert(*bb_id, dag_bb_id);
            dag_bb_list.push(dag_bb_id);
        }

        self.set_dag_bb_pred_and_succ(func, &mut dag_bb_arena);

        for bb_id in &func.basic_blocks.order {
            let bb = &func.basic_blocks.arena[*bb_id];
            let id = self.construct_dag_from_basic_block(func, bb);
            dag_bb_arena[self.cur_conv_info_ref().get_dag_bb(*bb_id)].set_entry(id);
        }

        let conv_info = mem::replace(&mut self.cur_conversion_info, None).unwrap();
        DAGFunction::new(
            func,
            conv_info.dag_heap,
            dag_bb_arena,
            dag_bb_list,
            conv_info.local_mgr,
            conv_info.vreg_gen,
            conv_info.regs_info,
        )
    }

    pub fn get_dag_id_from_value(&mut self, v: &Value) -> Raw<DAGNode> {
        match v {
            Value::Instruction(iv) => {
                if let Some(node) = self.inst_id_node_id.get(&iv.id) {
                    return *node;
                }
                let empty_node = self.alloc_node(DAGNode::new(NodeKind::None, vec![], Type::Void));
                self.inst_id_node_id.insert(iv.id, empty_node);
                empty_node
            }
            Value::Immediate(imm) => {
                let imm = match imm {
                    ImmediateValue::Int32(i) => ConstantKind::Int32(*i),
                    ImmediateValue::Int64(i) => ConstantKind::Int64(*i),
                    ImmediateValue::F64(f) => ConstantKind::F64(*f),
                };
                self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Constant(imm)),
                    vec![],
                    imm.get_type(),
                ))
            }
            Value::Argument(av) => {
                let ty = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_type(av.index)
                    .unwrap();
                let fi_ty = self.types.new_pointer_ty(ty);
                let fi = self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::FrameIndex(FrameIndexInfo::new(
                        ty.clone(),
                        FrameIndexKind::Arg(av.index),
                    ))),
                    vec![],
                    ty,
                ));
                let fiaddr = self.alloc_node(DAGNode::new(
                    NodeKind::IR(IRNodeKind::FIAddr),
                    vec![fi],
                    fi_ty,
                ));
                let load_id = self.alloc_node(DAGNode::new(
                    NodeKind::IR(IRNodeKind::Load),
                    vec![fiaddr],
                    ty,
                ));
                load_id
            }
            Value::Function(FunctionValue { func_id }) => {
                let f = self.module.function_ref(*func_id);
                self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Address(AddressKind::FunctionName(
                        f.name.to_string(),
                    ))),
                    vec![],
                    Type::Void, // TODO
                ))
            }
            Value::None => self.alloc_node(DAGNode::new(NodeKind::None, vec![], Type::Void)),
        }
    }

    fn construct_dag_from_basic_block(&mut self, func: &Function, bb: &BasicBlock) -> Raw<DAGNode> {
        let entry_node = self.alloc_node(DAGNode::new(
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
                Opcode::Alloca => {
                    let ty = *inst.operands[0].as_type();
                    let fi_ty = self.types.new_pointer_ty(ty);
                    let fi = self.cur_conv_info_mut_with(|c| {
                        let frinfo = c.local_mgr.alloc(&ty);
                        c.dag_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::FrameIndex(frinfo.clone())), // TODO
                            vec![],
                            ty,
                        ))
                    });
                    let fiaddr = self.cur_conv_info_mut_with(|c| {
                        c.dag_heap.alloc(DAGNode::new(
                            NodeKind::IR(IRNodeKind::FIAddr),
                            vec![fi],
                            fi_ty,
                        ))
                    });
                    self.inst_id_node_id.insert(inst_id, fiaddr);
                }
                Opcode::Load => {
                    let v = *inst.operands[0].as_value();
                    let v = self.get_dag_id_from_value(&v);
                    let load_id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Load), vec![v], inst.ty.clone()),
                    );

                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(load_id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        // make_chain!(load_id);
                        self.inst_id_node_id.insert(inst_id, load_id);
                    }
                }
                Opcode::Store => {
                    let src = self.get_dag_id_from_value(inst.operands[0].as_value());
                    let dst = self.get_dag_id_from_value(inst.operands[1].as_value());
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Store), vec![dst, src], Type::Void),
                    );
                    make_chain!(id);
                }
                Opcode::GetElementPtr => {
                    let indices: Vec<Value> =
                        inst.operands[1..].iter().map(|v| *v.as_value()).collect();
                    let gep =
                        self.construct_dag_for_gep(inst, inst.operands[0].as_value(), &indices);
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        make_chain!(gep);
                    }
                    self.inst_id_node_id.insert(inst_id, gep);
                }
                Opcode::Call => {
                    let mut operands: Vec<Raw<DAGNode>> = inst.operands[1..]
                        .iter()
                        .map(|v| self.get_dag_id_from_value(v.as_value()))
                        .collect();
                    operands.insert(0, self.get_dag_id_from_value(inst.operands[0].as_value()));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Call), operands, inst.ty.clone()),
                    );
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        if inst.ty == Type::Void || inst.users.borrow().len() == 0 {
                            make_chain!(id);
                        }
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Rem => {
                    let v1 = self.get_dag_id_from_value(inst.operands[0].as_value());
                    let v2 = self.get_dag_id_from_value(inst.operands[1].as_value());
                    let bin_id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(
                            match inst.opcode {
                                Opcode::Add => NodeKind::IR(IRNodeKind::Add),
                                Opcode::Sub => NodeKind::IR(IRNodeKind::Sub),
                                Opcode::Mul => NodeKind::IR(IRNodeKind::Mul),
                                Opcode::Div => NodeKind::IR(IRNodeKind::Div),
                                Opcode::Rem => NodeKind::IR(IRNodeKind::Rem),
                                _ => unreachable!(),
                            },
                            vec![v1, v2],
                            inst.ty.clone(),
                        ),
                    );

                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(bin_id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_id_node_id.insert(inst_id, bin_id);
                    }
                }
                Opcode::Br => make_chain!(self.cur_conv_info_mut_with(|c| {
                    let bb = c.dag_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::BasicBlock(
                            c.get_dag_bb(*inst.operands[0].as_basic_block()),
                        )),
                        vec![],
                        Type::Void,
                    ));
                    c.dag_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Br),
                        vec![bb],
                        Type::Void,
                    ))
                })),
                Opcode::CondBr => {
                    let v = *inst.operands[0].as_value();
                    let then_ = *inst.operands[1].as_basic_block();
                    let else_ = *inst.operands[2].as_basic_block();
                    let v = self.get_dag_id_from_value(&v);
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
                Opcode::ICmp => {
                    let c = *inst.operands[0].as_icmp_kind();
                    let v1 = self.get_dag_id_from_value(inst.operands[1].as_value());
                    let v2 = self.get_dag_id_from_value(inst.operands[2].as_value());
                    let cond = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::CondKind((c).into())),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(
                            NodeKind::IR(IRNodeKind::Setcc),
                            vec![cond, v1, v2],
                            inst.ty.clone(),
                        ),
                    );
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::FCmp => {
                    let c = *inst.operands[0].as_fcmp_kind();
                    let v1 = self.get_dag_id_from_value(inst.operands[1].as_value());
                    let v2 = self.get_dag_id_from_value(inst.operands[2].as_value());
                    let cond = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::CondKind((c).into())),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(
                            NodeKind::IR(IRNodeKind::FCmp),
                            vec![cond, v1, v2],
                            inst.ty.clone(),
                        ),
                    );
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Phi => {
                    let mut operands = vec![];
                    for i in (0..inst.operands.len()).step_by(2) {
                        let (val, bb) = (
                            *inst.operands[i].as_value(),
                            *inst.operands[i + 1].as_basic_block(),
                        );
                        // Remove CopyFromReg if necessary
                        let val = self.get_dag_id_from_value(&val);
                        operands.push(match val.kind {
                            NodeKind::IR(IRNodeKind::CopyFromReg) => val.operand[0],
                            _ => val,
                        });

                        operands.push(self.cur_conv_info_mut_with(|c| {
                            c.dag_heap.alloc(DAGNode::new(
                                NodeKind::Operand(OperandNodeKind::BasicBlock(c.get_dag_bb(bb))),
                                vec![],
                                Type::Void,
                            ))
                        }))
                    }
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Phi), operands, inst.ty.clone()),
                    );
                    if bb.liveness.borrow().live_out.contains(&inst_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_id_node_id.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_id_node_id.insert(inst_id, id);
                    }
                }
                Opcode::Ret => {
                    let v = self.get_dag_id_from_value(inst.operands[0].as_value());
                    make_chain!(self.alloc_node(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Ret),
                        vec![v],
                        Type::Void
                    )))
                }
            }
        }

        entry_node
    }

    fn construct_dag_for_gep(
        &mut self,
        _inst: &Instruction,
        ptr: &Value,
        indices: &[Value],
    ) -> Raw<DAGNode> {
        let mut gep = self.get_dag_id_from_value(ptr);
        let mut ty = ptr.get_type(self.module);

        for idx in indices {
            // TODO: we need better way
            let struct_sz = if matches!(ty, Type::Struct(_)) {
                let mut total = 0;
                for i in 0..idx.as_imm().as_int32() {
                    total += self
                        .types
                        .get_element_ty(ty, Some(&Value::new_imm_int32(i)))
                        .unwrap()
                        .size_in_byte(&self.types) as i32;
                }
                Some(total)
            } else {
                None
            };
            ty = self.types.get_element_ty(ty, Some(idx)).unwrap();

            let idx = self.get_dag_id_from_value(idx);
            let heap = &mut self.cur_conversion_info.as_mut().unwrap().dag_heap;
            let idx = match idx.kind {
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(i))) => {
                    heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            struct_sz.unwrap_or(i * ty.size_in_byte(&self.types) as i32),
                        ))),
                        vec![],
                        Type::Int32,
                    ))
                }
                NodeKind::IR(IRNodeKind::FIAddr) => idx.operand[0], // retrieve frame index
                NodeKind::Operand(OperandNodeKind::FrameIndex(_)) => unreachable!(),
                NodeKind::Operand(OperandNodeKind::CondKind(_))
                | NodeKind::Operand(OperandNodeKind::Address(_))
                | NodeKind::Operand(OperandNodeKind::BasicBlock(_)) => idx,
                _ => {
                    let tysz = heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            ty.size_in_byte(&self.types) as i32,
                        ))),
                        vec![],
                        Type::Int32,
                    ));
                    let cast = sext_if_necessary(&self.types, heap, idx, Type::Int64);
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
            dag_bb_arena[*dag_bb].pred = func.basic_blocks.arena[*bb]
                .pred
                .iter()
                .map(|bb| conv_info.get_dag_bb(*bb))
                .collect();
            dag_bb_arena[*dag_bb].succ = func.basic_blocks.arena[*bb]
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

    fn make_chain_with_copying(&mut self, mut node: Raw<DAGNode>) -> Raw<DAGNode> {
        let kind = NodeKind::Operand(OperandNodeKind::Register(
            self.cur_conv_info_mut()
                .regs_info
                .new_virt_reg(ty2rc(&node.ty).unwrap()),
        ));
        let reg = self
            .cur_conv_info_mut()
            .dag_heap
            .alloc(DAGNode::new(kind, vec![], node.ty));
        let old_node = self
            .cur_conv_info_mut()
            .dag_heap
            .alloc(mem::replace(&mut *node, (*reg).clone()));
        let copy = self.cur_conv_info_mut().dag_heap.alloc(DAGNode::new(
            NodeKind::IR(IRNodeKind::CopyToReg),
            vec![reg, old_node],
            Type::Void,
        ));
        self.make_chain(copy);
        node
    }

    fn alloc_node(&mut self, new: DAGNode) -> Raw<DAGNode> {
        self.cur_conv_info_mut().dag_heap.alloc(new)
    }

    fn alloc_node_as_necessary(&mut self, id: InstructionId, new: DAGNode) -> Raw<DAGNode> {
        if let Some(node) = self.inst_id_node_id.get_mut(&id) {
            **node = new;
            *node
        } else {
            self.cur_conv_info_mut().dag_heap.alloc(new)
        }
    }
}

fn sext_if_necessary(
    tys: &Types,
    heap: &mut DAGHeap,
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
            dag_heap: DAGHeap::new(),
            local_mgr: LocalVariables::new(),
            bb_to_dag_bb: FxHashMap::default(),
            last_chain_node: None,
            vreg_gen: VirtRegGen::new(),
            regs_info: RegistersInfo::new(),
        }
    }

    pub fn get_dag_bb(&self, bb_id: BasicBlockId) -> DAGBasicBlockId {
        *self.bb_to_dag_bb.get(&bb_id).unwrap()
    }
}
