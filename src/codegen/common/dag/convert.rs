// Convert IR to architecture-independent DAG form
// TODO: refactor

use super::node::*;
use crate::codegen::arch::{frame_object::*, machine::register::*};
use crate::codegen::common::dag::{basic_block::*, function::*, module::*};
use crate::ir::{
    basic_block::*, function::*, liveness::*, module::*, opcode::*, types::*, value::*,
};
use crate::util::allocator::Raw;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::mem;

pub struct ConvertToDAGFunction<'a> {
    module: &'a Module,
    func: &'a Function,
    node_heap: DAGHeap,
    bb_arena: Arena<DAGBasicBlock>,
    bb_order: Vec<DAGBasicBlockId>,
    bb_map: FxHashMap<BasicBlockId, DAGBasicBlockId>,
    inst_to_node: FxHashMap<InstructionId, Raw<DAGNode>>,
    arg_regs: FxHashMap<usize, Raw<DAGNode>>,
    regs_info: RegistersInfo,
    local_mgr: LocalVariables,
}

pub struct ConvertToDAGNode<'a> {
    pub module: &'a Module,
    pub func: &'a Function,
    pub block: &'a BasicBlock,
    pub block_id: BasicBlockId,
    pub node_heap: &'a mut DAGHeap,
    pub inst_to_node: &'a mut FxHashMap<InstructionId, Raw<DAGNode>>,
    pub regs_info: &'a mut RegistersInfo,
    pub arg_regs: &'a mut FxHashMap<usize, Raw<DAGNode>>,
    pub local_mgr: &'a mut LocalVariables,
    pub bb_map: &'a FxHashMap<BasicBlockId, DAGBasicBlockId>,
    pub entry: bool,
    pub last_chained_node: Option<Raw<DAGNode>>,
}

pub fn convert_to_dag_module(mut module: Module) -> DAGModule {
    IRLivenessAnalyzer::new(&mut module).analyze();

    let mut functions: Arena<DAGFunction> = Arena::new();

    for (_, func) in &module.functions {
        functions.alloc(
            ConvertToDAGFunction {
                module: &module,
                func,
                bb_arena: Arena::new(),
                bb_order: vec![],
                bb_map: FxHashMap::default(),
                node_heap: DAGHeap::new(),
                inst_to_node: FxHashMap::default(),
                regs_info: RegistersInfo::new(),
                arg_regs: FxHashMap::default(),
                local_mgr: LocalVariables::new(),
            }
            .run(),
        );
    }

    DAGModule {
        name: module.name,
        functions,
        types: module.types,
        global_vars: module.global_vars,
        const_pool: module.const_pool,
    }
}

impl<'a> ConvertToDAGFunction<'a> {
    pub fn run(mut self) -> DAGFunction {
        for &bb_id in &self.func.basic_blocks.order {
            let dag_bb_id = self.bb_arena.alloc(DAGBasicBlock::new());
            self.bb_order.push(dag_bb_id);
            self.bb_map.insert(bb_id, dag_bb_id);
        }

        self.set_dag_bb_pred_and_succ();

        for (i, &bb_id) in self.func.basic_blocks.order.iter().enumerate() {
            let block = &self.func.basic_blocks.arena[bb_id];
            let entry = i == 0;
            let (entry, root) = ConvertToDAGNode {
                module: self.module,
                func: self.func,
                block,
                block_id: bb_id,
                node_heap: &mut self.node_heap,
                inst_to_node: &mut self.inst_to_node,
                regs_info: &mut self.regs_info,
                arg_regs: &mut self.arg_regs,
                local_mgr: &mut self.local_mgr,
                bb_map: &self.bb_map,
                entry,
                last_chained_node: None,
            }
            .run();
            self.bb_arena[self.bb_map[&bb_id]].set_entry(entry);
            self.bb_arena[self.bb_map[&bb_id]].set_root(root);
        }

        DAGFunction {
            name: self.func.name.clone(),
            ty: self.func.ty,
            dag_basic_block_arena: self.bb_arena,
            dag_basic_blocks: self.bb_order,
            dag_heap: self.node_heap,
            local_mgr: self.local_mgr,
            regs_info: self.regs_info,
            is_internal: self.func.is_internal,
            types: self.func.types.clone(),
        }
    }

    fn set_dag_bb_pred_and_succ(&mut self) {
        for (&bb, &dag_bb) in &self.bb_map {
            self.bb_arena[dag_bb].pred = self.func.basic_blocks.arena[bb]
                .pred
                .iter()
                .map(|bb| self.bb_map[bb])
                .collect();
            self.bb_arena[dag_bb].succ = self.func.basic_blocks.arena[bb]
                .succ
                .iter()
                .map(|bb| self.bb_map[bb])
                .collect();
        }
    }
}

impl<'a> ConvertToDAGNode<'a> {
    pub fn run(mut self) -> (Raw<DAGNode>, Raw<DAGNode>) {
        // basic block entry
        let entry = self.alloc_node(DAGNode::new(
            NodeKind::IR(IRNodeKind::Entry),
            vec![],
            Type::Void,
        ));
        self.last_chained_node = Some(entry);

        // program entry
        if self.entry {
            // Copy physical argument registers to virtual regieters
            self.copy_reg_args();
        }

        for &inst_id in &*self.block.iseq_ref() {
            let inst = &self.func.inst_table[inst_id];
            match inst.opcode {
                Opcode::Alloca => {
                    let ty = inst.operand.types()[0];
                    let fi_ty = self.func.types.new_pointer_ty(ty);
                    let frinfo = self.local_mgr.alloc(&ty);
                    let fi = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::FrameIndex(frinfo)), // TODO
                        vec![],
                        ty,
                    ));
                    let fiaddr = self.alloc_node(DAGNode::new(
                        NodeKind::IR(IRNodeKind::FIAddr),
                        vec![fi],
                        fi_ty,
                    ));
                    self.inst_to_node.insert(inst_id, fiaddr);
                }
                Opcode::Load => {
                    let v = inst.operand.args()[0];
                    let v = self.get_node_from_value(&v);
                    let load_id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Load), vec![v], inst.ty),
                    );
                    // TODO
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(load_id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.make_chain(load_id);
                        self.inst_to_node.insert(inst_id, load_id);
                    }
                }
                Opcode::Store => {
                    let src = self.get_node_from_value(&inst.operand.args()[0]);
                    let dst = self.get_node_from_value(&inst.operand.args()[1]);
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Store), vec![dst, src], Type::Void),
                    );
                    self.make_chain(id);
                }
                Opcode::GetElementPtr => {
                    let indices: Vec<Value> =
                        inst.operand.args()[1..].into_iter().map(|x| *x).collect();
                    let gep = self.construct_node_for_gep(&inst.operand.args()[0], &indices);
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let gep = self.make_chain_with_copying(gep);
                        self.inst_to_node.insert(inst_id, gep);
                    } else {
                        self.inst_to_node.insert(inst_id, gep);
                    }
                }
                Opcode::Call => {
                    let mut operands: Vec<Raw<DAGNode>> = inst.operand.args()[1..]
                        .iter()
                        .map(|v| self.get_node_from_value(v))
                        .collect();
                    operands.insert(0, self.get_node_from_value(&inst.operand.args()[0]));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Call), operands, inst.ty.clone()),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        // if inst.ty == Type::Void || inst.users.borrow().len() == 0 {}
                        self.make_chain(id);
                        self.inst_to_node.insert(inst_id, id);
                    }
                }
                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::Rem
                | Opcode::Shl => {
                    let v1 = self.get_node_from_value(&inst.operand.args()[0]);
                    let v2 = self.get_node_from_value(&inst.operand.args()[1]);
                    let bin_id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(
                            match inst.opcode {
                                Opcode::Add => NodeKind::IR(IRNodeKind::Add),
                                Opcode::Sub => NodeKind::IR(IRNodeKind::Sub),
                                Opcode::Mul => NodeKind::IR(IRNodeKind::Mul),
                                Opcode::Div => NodeKind::IR(IRNodeKind::Div),
                                Opcode::Rem => NodeKind::IR(IRNodeKind::Rem),
                                Opcode::Shl => NodeKind::IR(IRNodeKind::Shl),
                                _ => unreachable!(),
                            },
                            vec![v1, v2],
                            inst.ty,
                        ),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(bin_id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, bin_id);
                    }
                }
                Opcode::SIToFP | Opcode::FPToSI => {
                    let v = self.get_node_from_value(&inst.operand.args()[0]);
                    let inst = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(
                            match inst.opcode {
                                Opcode::SIToFP => NodeKind::IR(IRNodeKind::SIToFP),
                                Opcode::FPToSI => NodeKind::IR(IRNodeKind::FPToSI),
                                _ => unreachable!(),
                            },
                            vec![v],
                            inst.ty,
                        ),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(inst);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, inst);
                    }
                }
                Opcode::Sext => {
                    let x = self.get_node_from_value(&inst.operand.args()[0]);
                    let inst = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Sext), vec![x], inst.ty),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(inst);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, inst);
                    }
                }
                Opcode::Bitcast => {
                    let x = self.get_node_from_value(&inst.operand.args()[0]);
                    let inst = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Bitcast), vec![x], inst.ty),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(inst);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, inst);
                    }
                }
                Opcode::Br => {
                    let bb = self.node_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::BasicBlock(
                            self.bb_map[&inst.operand.blocks()[0]],
                        )),
                        vec![],
                        Type::Void,
                    ));
                    let br = self.node_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Br),
                        vec![bb],
                        Type::Void,
                    ));
                    self.make_chain(br);
                }
                Opcode::CondBr => {
                    let v = inst.operand.args()[0];
                    let then_ = &inst.operand.blocks()[0];
                    let else_ = &inst.operand.blocks()[1];
                    let v = self.get_node_from_value(&v);
                    let brcond = {
                        let bb = self.node_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::BasicBlock(self.bb_map[then_])),
                            vec![],
                            Type::Void,
                        ));
                        self.node_heap.alloc(DAGNode::new(
                            NodeKind::IR(IRNodeKind::BrCond),
                            vec![v, bb],
                            Type::Void,
                        ))
                    };
                    self.make_chain(brcond);
                    let br = {
                        let bb = self.node_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::BasicBlock(self.bb_map[else_])),
                            vec![],
                            Type::Void,
                        ));
                        self.node_heap.alloc(DAGNode::new(
                            NodeKind::IR(IRNodeKind::Br),
                            vec![bb],
                            Type::Void,
                        ))
                    };
                    self.make_chain(br);
                }
                Opcode::ICmp => {
                    let c = inst.operand.int_cmp()[0];
                    let v1 = self.get_node_from_value(&inst.operand.args()[0]);
                    let v2 = self.get_node_from_value(&inst.operand.args()[1]);
                    let cond = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::CondKind((c).into())),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Setcc), vec![cond, v1, v2], inst.ty),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, id);
                    }
                }
                Opcode::FCmp => {
                    let c = inst.operand.float_cmp()[0];
                    let v1 = self.get_node_from_value(&inst.operand.args()[0]);
                    let v2 = self.get_node_from_value(&inst.operand.args()[1]);
                    let cond = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::CondKind((c).into())),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::FCmp), vec![cond, v1, v2], inst.ty),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, id);
                    }
                }
                Opcode::Phi => {
                    let mut operands = vec![];
                    for (i, val) in inst.operand.args().iter().enumerate() {
                        let bb = &inst.operand.blocks()[i];
                        // Remove CopyFromReg if necessary
                        let val = self.get_node_from_value(&val);
                        operands.push(match val.kind {
                            NodeKind::IR(IRNodeKind::CopyFromReg) => val.operand[0],
                            _ => val,
                        });
                        operands.push(self.node_heap.alloc(DAGNode::new(
                            NodeKind::Operand(OperandNodeKind::BasicBlock(self.bb_map[bb])),
                            vec![],
                            Type::Void,
                        )))
                    }
                    let id = self.alloc_node_as_necessary(
                        inst_id,
                        DAGNode::new(NodeKind::IR(IRNodeKind::Phi), operands, inst.ty),
                    );
                    if self.func.basic_blocks.liveness[&self.block_id]
                        .live_out
                        .contains(&inst_id)
                    {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.inst_to_node.insert(inst_id, copy_from_reg);
                    } else {
                        self.inst_to_node.insert(inst_id, id);
                    }
                }
                Opcode::Ret => {
                    let v = self.get_node_from_value(&inst.operand.args()[0]);
                    let ret = self.alloc_node(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Ret),
                        vec![v],
                        Type::Void,
                    ));
                    self.make_chain(ret)
                }
            }
        }

        let root = self.alloc_node(DAGNode::new(
            NodeKind::IR(IRNodeKind::Root),
            vec![],
            Type::Void,
        ));

        if let Some(last_chained_node) = &mut self.last_chained_node {
            last_chained_node.next = Some(root);
        }

        (entry, root)
    }

    pub fn get_node_from_value(&mut self, v: &Value) -> Raw<DAGNode> {
        match v {
            Value::Instruction(iv) => {
                if let Some(node) = self.inst_to_node.get(&iv.id) {
                    return *node;
                }
                let empty_node = self.alloc_node(DAGNode::new(NodeKind::None, vec![], Type::Void));
                self.inst_to_node.insert(iv.id, empty_node);
                empty_node
            }
            Value::Immediate(imm) => {
                let imm = match imm {
                    ImmediateValue::Int8(i) => ConstantKind::Int8(*i),
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
                if let Some(r) = self.arg_regs.get(&av.index) {
                    return *r;
                }
                let ty = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_type(av.index)
                    .unwrap();
                let byval = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_attr(av.index)
                    .map_or(false, |attr| attr.byval);
                if byval {
                    let fi = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::FrameIndex(FrameIndexInfo::new(
                            ty,
                            FrameIndexKind::Arg(av.index),
                        ))),
                        vec![],
                        ty,
                    ));
                    self.alloc_node(DAGNode::new(NodeKind::IR(IRNodeKind::FIAddr), vec![fi], ty))
                } else {
                    let fi_ty = self.func.types.new_pointer_ty(ty);
                    let fi = self.alloc_node(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::FrameIndex(FrameIndexInfo::new(
                            ty,
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
                    self.alloc_node(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Load),
                        vec![fiaddr],
                        ty,
                    ))
                }
            }
            Value::Function(FunctionValue { func_id, ty }) => {
                let f = self.module.function_ref(*func_id);
                self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Address(AddressKind::FunctionName(
                        f.name.to_string(),
                    ))),
                    vec![],
                    *ty,
                ))
            }
            Value::Global(GlobalValue { id, ty }) => {
                let g = self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Address(AddressKind::Global(*id))),
                    vec![],
                    *ty,
                ));
                self.alloc_node(DAGNode::new(
                    NodeKind::IR(IRNodeKind::GlobalAddr),
                    vec![g],
                    *ty,
                ))
            }
            Value::Constant(ConstantValue { id, ty }) => {
                let c = self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Address(AddressKind::Const(*id))),
                    vec![],
                    *ty,
                ));
                self.alloc_node(DAGNode::new(
                    NodeKind::IR(IRNodeKind::ConstAddr),
                    vec![c],
                    *ty,
                ))
            }
            Value::None => self.alloc_node(DAGNode::new(NodeKind::None, vec![], Type::Void)),
        }
    }

    fn construct_node_for_gep(&mut self, ptr: &Value, indices: &[Value]) -> Raw<DAGNode> {
        let mut gep = self.get_node_from_value(ptr);
        let mut ty = ptr.get_type();

        for idx in indices {
            let size = match ty {
                Type::Struct(id) => {
                    let off = *self
                        .module
                        .types
                        .compound_ty(id)
                        .as_struct()
                        .get_elem_offset(idx.as_imm().as_int32() as usize)
                        .unwrap();
                    Some(off as i32)
                }
                _ => None,
            };
            ty = self.module.types.get_element_ty(ty, Some(idx)).unwrap();

            let idx = self.get_node_from_value(idx);
            let idx = match idx.kind {
                NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(i))) => {
                    self.node_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            size.unwrap_or(i * ty.size_in_byte(&self.module.types) as i32),
                        ))),
                        vec![],
                        Type::i32,
                    ))
                }
                NodeKind::IR(IRNodeKind::FIAddr) => idx.operand[0], // retrieve frame index
                NodeKind::Operand(OperandNodeKind::FrameIndex(_)) => unreachable!(),
                NodeKind::Operand(OperandNodeKind::CondKind(_))
                | NodeKind::Operand(OperandNodeKind::Address(_))
                | NodeKind::Operand(OperandNodeKind::BasicBlock(_)) => idx,
                _ => {
                    let tysz = self.node_heap.alloc(DAGNode::new(
                        NodeKind::Operand(OperandNodeKind::Constant(ConstantKind::Int32(
                            ty.size_in_byte(&self.module.types) as i32,
                        ))),
                        vec![],
                        Type::i32,
                    ));
                    let cast = sext_if_necessary(&self.func.types, self.node_heap, idx, Type::i64);
                    // assert!(cast.ty == Type::i64);
                    self.node_heap.alloc(DAGNode::new(
                        NodeKind::IR(IRNodeKind::Mul),
                        vec![cast, tysz],
                        Type::i64, // TODO
                    ))
                }
            };

            let ptr_ty = self.module.types.new_pointer_ty(ty);
            gep = self.node_heap.alloc(DAGNode::new(
                NodeKind::IR(IRNodeKind::Add),
                vec![gep, idx],
                ptr_ty,
            ));
        }

        gep
    }

    pub fn make_chain(&mut self, mut node: Raw<DAGNode>) {
        if let Some(last_chained_node) = &mut self.last_chained_node {
            node.chain = Some(*last_chained_node);
            last_chained_node.next = Some(node);
            *last_chained_node = node;
        }
    }

    fn make_chain_with_copying(&mut self, mut node: Raw<DAGNode>) -> Raw<DAGNode> {
        let kind = NodeKind::Operand(OperandNodeKind::Register(
            self.regs_info.new_virt_reg(ty2rc(&node.ty).unwrap()),
        ));
        let reg = self.node_heap.alloc(DAGNode::new(kind, vec![], node.ty));
        let old_node = self
            .node_heap
            .alloc(mem::replace(&mut *node, (*reg).clone()));
        let copy = self.node_heap.alloc(DAGNode::new(
            NodeKind::IR(IRNodeKind::CopyToReg),
            vec![reg, old_node],
            Type::Void,
        ));
        self.make_chain(copy);
        node
    }

    // fn make_chain_with_copying_without_updating_last_chain(
    //     &mut self,
    //     mut node: Raw<DAGNode>,
    // ) -> Raw<DAGNode> {
    //     let kind = NodeKind::Operand(OperandNodeKind::Register(
    //         self.regs_info.new_virt_reg(ty2rc(&node.ty).unwrap()),
    //     ));
    //     let reg = self.node_heap.alloc(DAGNode::new(kind, vec![], node.ty));
    //     let old_node = self
    //         .node_heap
    //         .alloc(mem::replace(&mut *node, (*reg).clone()));
    //     let mut copy = self.node_heap.alloc(DAGNode::new(
    //         NodeKind::IR(IRNodeKind::CopyToReg),
    //         vec![reg, old_node],
    //         Type::Void,
    //     ));
    //
    //     if let Some(last_chained_node) = &mut self.last_chained_node {
    //         copy.chain = Some(*last_chained_node);
    //     }
    //
    //     node
    // }

    // fn make_chain_without_updating_last_chain(&mut self, mut node: Raw<DAGNode>) -> Raw<DAGNode> {
    //     if let Some(last_chained_node) = &mut self.last_chained_node {
    //         node.chain = Some(*last_chained_node);
    //     }
    //     node
    // }

    pub fn alloc_node(&mut self, new: DAGNode) -> Raw<DAGNode> {
        self.node_heap.alloc(new)
    }

    pub fn alloc_node_as_necessary(&mut self, id: InstructionId, new: DAGNode) -> Raw<DAGNode> {
        if let Some(node) = self.inst_to_node.get_mut(&id) {
            **node = new;
            *node
        } else {
            self.node_heap.alloc(new)
        }
    }
}

fn sext_if_necessary(
    types: &Types,
    heap: &mut DAGHeap,
    node: Raw<DAGNode>,
    to: Type,
) -> Raw<DAGNode> {
    if node.ty.size_in_bits(types) >= to.size_in_bits(types) {
        return node;
    }

    heap.alloc(DAGNode::new(NodeKind::IR(IRNodeKind::Sext), vec![node], to))
}
