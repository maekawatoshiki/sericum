use crate::codegen::arch::{
    dag::convert::copy_reg_args,
    machine::register::{rc2ty, ty2rc},
};
use crate::codegen::common::{
    dag::{
        basic_block::{DAGBasicBlock, DAGBasicBlockId},
        function::DAGFunction,
        module::DAGModule,
        node::{AddressKind, IRNode, IROpcode, ImmediateKind, MINode, Node, NodeId, OperandNode},
    },
    machine::{
        frame_object::{FrameIndexInfo, FrameIndexKind, LocalVariables},
        register::RegistersInfo,
    },
};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId},
    function::Function,
    liveness::IRLivenessAnalyzer,
    module::Module,
    opcode::{InstructionId, Opcode},
    types::{Type, TypeSize},
    value::{
        ArgumentValue, ConstantValue, FunctionValue, GlobalValue, ImmediateValue, InstructionValue,
        Value,
    },
};
use id_arena::Arena;
use rustc_hash::FxHashMap;

impl Into<DAGModule> for Module {
    fn into(self) -> DAGModule {
        convert_module_to_dag_module(self)
    }
}

pub fn convert_module_to_dag_module(mut module: Module) -> DAGModule {
    IRLivenessAnalyzer::new(&mut module).analyze();

    let mut functions: Arena<DAGFunction> = Arena::new();

    for (_, func) in &module.functions {
        let f = convert_function_to_dag_function(FunctionConversionContext {
            module: &module,
            func,
            node_arena: Arena::new(),
            local_vars: LocalVariables::new(),
            node_map: FxHashMap::default(),
            arg_regs: FxHashMap::default(),
            regs: RegistersInfo::new(),
        });
        functions.alloc(f);
    }

    DAGModule {
        name: module.name,
        functions,
        types: module.types,
        global_vars: module.global_vars,
        const_pool: module.const_pool,
    }
}

struct FunctionConversionContext<'a> {
    module: &'a Module,
    func: &'a Function,
    node_arena: Arena<Node>,
    local_vars: LocalVariables,
    node_map: FxHashMap<InstructionId, NodeId>,
    arg_regs: FxHashMap<usize, NodeId>,
    regs: RegistersInfo,
}

fn convert_function_to_dag_function<'a>(mut ctx: FunctionConversionContext<'a>) -> DAGFunction {
    let mut block_order: Vec<DAGBasicBlockId> = vec![];
    let mut block_map: FxHashMap<BasicBlockId, DAGBasicBlockId> = FxHashMap::default();
    let mut block_arena: Arena<DAGBasicBlock> = Arena::new();

    // Create new dag blocks
    for &id in &ctx.func.basic_blocks.order {
        let new_id = block_arena.alloc(DAGBasicBlock::new());
        block_order.push(new_id);
        block_map.insert(id, new_id);
    }

    // Set preds and succs for each new dag block
    for (&id, &new_id) in &block_map {
        block_arena[new_id].pred = ctx.func.basic_blocks.arena[id]
            .pred
            .iter()
            .map(|id| block_map[id])
            .collect();
        block_arena[new_id].succ = ctx.func.basic_blocks.arena[id]
            .succ
            .iter()
            .map(|id| block_map[id])
            .collect();
    }

    for (i, &id) in ctx.func.basic_blocks.order.iter().enumerate() {
        let block = &ctx.func.basic_blocks.arena[id];
        let is_entry = i == 0;
        let entry = convert_block_to_dag_block(BlockConversionContext::new(
            ctx.module,
            ctx.func,
            &mut ctx.node_arena,
            is_entry,
            &mut ctx.local_vars,
            &mut ctx.node_map,
            &mut ctx.arg_regs,
            &mut ctx.regs,
            &block_map,
            block,
            id,
        ));
        block_arena[block_map[&id]].entry = Some(entry);
    }

    DAGFunction {
        name: ctx.func.name.clone(),
        ty: ctx.func.ty,
        dag_basic_blocks: block_order,
        dag_basic_block_arena: block_arena,
        node_arena: ctx.node_arena,
        local_vars: ctx.local_vars,
        regs: ctx.regs,
        is_internal: ctx.func.is_internal,
        types: ctx.func.types.clone(),
    }
}

pub struct BlockConversionContext<'a> {
    module: &'a Module,
    pub func: &'a Function,
    node_arena: &'a mut Arena<Node>,
    is_entry: bool,
    last_chained_node: NodeId,
    entry: NodeId,
    local_vars: &'a mut LocalVariables,
    node_map: &'a mut FxHashMap<InstructionId, NodeId>,
    pub arg_regs: &'a mut FxHashMap<usize, NodeId>,
    pub regs: &'a mut RegistersInfo,
    block_map: &'a FxHashMap<BasicBlockId, DAGBasicBlockId>,
    block: &'a BasicBlock,
    block_id: BasicBlockId,
}

fn convert_block_to_dag_block<'a>(mut ctx: BlockConversionContext<'a>) -> NodeId {
    if ctx.is_entry {
        copy_reg_args(&mut ctx);
    }

    for &id in &*ctx.block.iseq_ref() {
        let inst = &ctx.func.inst_table[id];

        let node = match inst.opcode {
            Opcode::Alloca => {
                let ty = inst.operand.types()[0];
                let slot = ctx.local_vars.alloc(&ty);
                let slot = ctx.node(slot.into());
                let addr_ty = ctx.func.types.new_pointer_ty(ty);
                ctx.node(
                    IRNode::new(IROpcode::FIAddr)
                        .args(vec![slot])
                        .ty(addr_ty)
                        .into(),
                )
            }
            Opcode::Load => {
                let arg = ctx.node_from_value(&inst.operand.args()[0]);
                ctx.node_(
                    id,
                    IRNode::new(IROpcode::Load)
                        .args(vec![arg])
                        .ty(inst.ty)
                        .into(),
                )
            }
            Opcode::Store => {
                let (src, dst) = (
                    ctx.node_from_value(&inst.operand.args()[0]),
                    ctx.node_from_value(&inst.operand.args()[1]),
                );
                ctx.node_(id, IRNode::new(IROpcode::Store).args(vec![dst, src]).into())
            }
            Opcode::GetElementPtr => ctx.gep_node_from_values(inst.operand.args()),
            Opcode::Call => {
                let args: Vec<NodeId> = inst
                    .operand
                    .args()
                    .iter()
                    .map(|arg| ctx.node_from_value(arg))
                    .collect();
                ctx.node_(
                    id,
                    IRNode::new(IROpcode::Call).args(args).ty(inst.ty).into(),
                )
            }
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Rem | Opcode::Shl => {
                let (lhs, rhs) = (
                    ctx.node_from_value(&inst.operand.args()[0]),
                    ctx.node_from_value(&inst.operand.args()[1]),
                );
                ctx.node_(
                    id,
                    IRNode::new(match inst.opcode {
                        Opcode::Add => IROpcode::Add,
                        Opcode::Sub => IROpcode::Sub,
                        Opcode::Mul => IROpcode::Mul,
                        Opcode::Div => IROpcode::Div,
                        Opcode::Rem => IROpcode::Rem,
                        Opcode::Shl => IROpcode::Shl,
                        _ => unreachable!(),
                    })
                    .args(vec![lhs, rhs])
                    .ty(inst.ty)
                    .into(),
                )
            }
            Opcode::SIToFP | Opcode::FPToSI => {
                let arg = ctx.node_from_value(&inst.operand.args()[0]);
                ctx.node_(
                    id,
                    IRNode::new(match inst.opcode {
                        Opcode::SIToFP => IROpcode::SIToFP,
                        Opcode::FPToSI => IROpcode::FPToSI,
                        _ => unreachable!(),
                    })
                    .args(vec![arg])
                    .ty(inst.ty)
                    .into(),
                )
            }
            Opcode::Zext | Opcode::Sext | Opcode::Bitcast => {
                let arg = ctx.node_from_value(&inst.operand.args()[0]);
                ctx.node_(
                    id,
                    IRNode::new(match inst.opcode {
                        Opcode::Sext => IROpcode::Sext,
                        Opcode::Zext => IROpcode::Zext,
                        Opcode::Bitcast => IROpcode::Bitcast,
                        _ => unreachable!(),
                    })
                    .args(vec![arg])
                    .ty(inst.ty)
                    .into(),
                )
            }
            Opcode::Br => {
                let block = ctx.node(ctx.block_map[&inst.operand.blocks()[0]].into());
                ctx.node(IRNode::new(IROpcode::Br).args(vec![block]).into())
            }
            Opcode::CondBr => {
                let (arg, then_, else_) = (
                    ctx.node_from_value(&inst.operand.args()[0]),
                    ctx.node(ctx.block_map[&inst.operand.blocks()[0]].into()),
                    ctx.node(ctx.block_map[&inst.operand.blocks()[1]].into()),
                );
                let brcond = ctx.node(IRNode::new(IROpcode::BrCond).args(vec![arg, then_]).into());
                ctx.make_chain(brcond);
                let br = ctx.node(IRNode::new(IROpcode::Br).args(vec![else_]).into());
                br
            }
            Opcode::ICmp | Opcode::FCmp => {
                let c = if inst.opcode == Opcode::ICmp {
                    ctx.node(inst.operand.int_cmp()[0].into())
                } else {
                    ctx.node(inst.operand.float_cmp()[0].into())
                };
                let (lhs, rhs) = (
                    ctx.node_from_value(&inst.operand.args()[0]),
                    ctx.node_from_value(&inst.operand.args()[1]),
                );
                ctx.node_(
                    id,
                    IRNode::new(match inst.opcode {
                        Opcode::ICmp => IROpcode::Setcc,
                        Opcode::FCmp => IROpcode::FCmp,
                        _ => unreachable!(),
                    })
                    .args(vec![c, lhs, rhs])
                    .ty(inst.ty)
                    .into(),
                )
            }
            Opcode::Phi => {
                let mut args = vec![];
                for (block, val) in inst.operand.blocks().iter().zip(inst.operand.args().iter()) {
                    let id = ctx.node_from_value(val);
                    args.push(match &ctx.node_arena[id] {
                        Node::IR(IRNode {
                            opcode: IROpcode::CopyFromReg,
                            args,
                            ..
                        }) => args[0],
                        _ => id,
                    });
                    args.push(ctx.node(ctx.block_map[block].into()));
                }
                ctx.node_(id, IRNode::new(IROpcode::Phi).args(args).ty(inst.ty).into())
            }
            Opcode::Ret => {
                let arg = ctx.node_from_value(&inst.operand.args()[0]);
                ctx.node(IRNode::new(IROpcode::Ret).args(vec![arg]).into())
            }
        };

        ctx.node_map.insert(id, node);

        let may_live_out = !matches!(
            inst.opcode,
            Opcode::Alloca | Opcode::Store | Opcode::Br | Opcode::CondBr | Opcode::Ret
        );
        let must_make_chain = matches!(
            inst.opcode,
            Opcode::Load | Opcode::Store | Opcode::Call | Opcode::Br | Opcode::CondBr | Opcode::Ret
        );
        let mut chain_made = false;

        if may_live_out {
            let do_live_out = ctx.func.basic_blocks.liveness[&ctx.block_id]
                .live_out
                .contains(&id);
            if do_live_out {
                let copied = ctx.make_chain_with_copying(node);
                ctx.node_map.insert(id, copied);
                chain_made = true;
            } else {
                if must_make_chain {
                    ctx.make_chain(node);
                    chain_made = true;
                }
                ctx.node_map.insert(id, node);
            }
        }

        if must_make_chain && !chain_made {
            ctx.make_chain(node);
        }
    }

    ctx.entry
}

impl<'a> BlockConversionContext<'a> {
    pub fn new(
        module: &'a Module,
        func: &'a Function,
        node_arena: &'a mut Arena<Node>,
        is_entry: bool,
        local_vars: &'a mut LocalVariables,
        node_map: &'a mut FxHashMap<InstructionId, NodeId>,
        arg_regs: &'a mut FxHashMap<usize, NodeId>,
        regs: &'a mut RegistersInfo,
        block_map: &'a FxHashMap<BasicBlockId, DAGBasicBlockId>,
        block: &'a BasicBlock,
        block_id: BasicBlockId,
    ) -> Self {
        let last_chained_node = node_arena.alloc(IRNode::new(IROpcode::Entry).into());
        Self {
            module,
            func,
            node_arena,
            is_entry,
            last_chained_node,
            entry: last_chained_node,
            local_vars,
            node_map,
            arg_regs,
            regs,
            block_map,
            block,
            block_id,
        }
    }

    pub fn node(&mut self, node: Node) -> NodeId {
        self.node_arena.alloc(node)
    }

    pub fn node_(&mut self, id: InstructionId, node: Node) -> NodeId {
        if let Some(id) = self.node_map.get_mut(&id) {
            self.node_arena[*id] = node;
            *id
        } else {
            self.node(node)
        }
    }

    pub fn node_from_value(&mut self, val: &Value) -> NodeId {
        match val {
            Value::Instruction(InstructionValue { id, .. }) => {
                if let Some(node) = self.node_map.get(id) {
                    return *node;
                }
                let empty_node = self.node(Node::None);
                self.node_map.insert(*id, empty_node);
                empty_node
            }
            Value::Immediate(imm) => {
                let imm = match imm {
                    ImmediateValue::Int8(i) => ImmediateKind::Int8(*i),
                    ImmediateValue::Int32(i) => ImmediateKind::Int32(*i),
                    ImmediateValue::Int64(i) => ImmediateKind::Int64(*i),
                    ImmediateValue::F64(f) => ImmediateKind::F64(*f),
                };
                self.node(imm.into())
            }
            Value::Argument(ArgumentValue { func_id, index, .. }) => {
                if let Some(r) = self.arg_regs.get(index) {
                    return *r;
                }
                let (ty, byval) = (
                    self.module
                        .function_ref(*func_id)
                        .get_param_type(*index)
                        .unwrap(),
                    self.module
                        .function_ref(*func_id)
                        .get_param_attr(*index)
                        .map_or(false, |attr| attr.byval),
                );
                let slot_ty = self.func.types.new_pointer_ty(ty);
                let slot = self.node(FrameIndexInfo::new(ty, FrameIndexKind::Arg(*index)).into());
                let addr = self.node(
                    IRNode::new(IROpcode::FIAddr)
                        .args(vec![slot])
                        .ty(slot_ty)
                        .into(),
                );
                if byval {
                    addr
                } else {
                    self.node(IRNode::new(IROpcode::Load).args(vec![addr]).ty(ty).into())
                }
            }
            Value::Function(FunctionValue { func_id, .. }) => {
                let f = self.module.function_ref(*func_id);
                self.node(AddressKind::FunctionName(f.name.to_string()).into())
            }
            Value::Global(GlobalValue { id, ty }) => {
                let gbl = self.node(AddressKind::Global(*id).into());
                let addr_ty = self.func.types.new_pointer_ty(*ty);
                self.node(
                    IRNode::new(IROpcode::GlobalAddr)
                        .args(vec![gbl])
                        .ty(addr_ty)
                        .into(),
                )
            }
            Value::Constant(ConstantValue { id, ty }) => {
                let cnst = self.node(AddressKind::Const(*id).into());
                let addr_ty = self.func.types.new_pointer_ty(*ty);
                self.node(
                    IRNode::new(IROpcode::ConstAddr)
                        .args(vec![cnst])
                        .ty(addr_ty)
                        .into(),
                )
            }
            Value::None => self.node(Node::None),
        }
    }

    pub fn gep_node_from_values(&mut self, vals: &[Value]) -> NodeId {
        let mut base = self.node_from_value(&vals[0]);
        let mut ty = vals[0].get_type();

        for idx in &vals[1..] {
            let (is_struct, size) = match ty {
                Type::Struct(id) => {
                    let off = *self
                        .func
                        .types
                        .compound_ty(id)
                        .as_struct()
                        .get_elem_offset(idx.as_imm().as_int32() as usize)
                        .unwrap();
                    ty = self.func.types.get_element_ty(ty, Some(idx)).unwrap();
                    (true, off as i32)
                }
                _ => {
                    ty = self.func.types.get_element_ty(ty, Some(idx)).unwrap();
                    (false, ty.size_in_byte(&self.func.types) as i32)
                }
            };
            let idx = self.node_from_value(idx);
            let idx = match &self.node_arena[idx] {
                Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) if is_struct => {
                    self.node(size.into())
                }
                Node::Operand(OperandNode::Imm(ImmediateKind::Int32(i))) => {
                    let i = *i; // to disable warning TODO
                    self.node((i * size).into())
                }
                Node::IR(IRNode {
                    opcode: IROpcode::FIAddr,
                    args,
                    ..
                }) => args[0],
                Node::Operand(OperandNode::Imm(_))
                | Node::Operand(OperandNode::Slot(_))
                | Node::Operand(OperandNode::CC(_))
                | Node::Operand(OperandNode::Addr(_))
                | Node::Operand(OperandNode::Block(_))
                | Node::Operand(OperandNode::Mem(_))
                | Node::None => unreachable!(),
                Node::Operand(OperandNode::Reg(_))
                | Node::MI(MINode { .. })
                | Node::IR(IRNode { .. }) => {
                    let size = self.node(size.into());
                    let cast = self.sext_if_necessary(idx, Type::i64);
                    self.node(
                        IRNode::new(IROpcode::Mul)
                            .args(vec![cast, size])
                            .ty(Type::i64)
                            .into(),
                    )
                }
            };

            let ptr_ty = self.func.types.new_pointer_ty(ty);
            base = self.node(
                IRNode::new(IROpcode::Add)
                    .args(vec![base, idx])
                    .ty(ptr_ty)
                    .into(),
            )
        }

        base
    }

    fn sext_if_necessary(&mut self, id: NodeId, to: Type) -> NodeId {
        if self.node_ty(id).unwrap().size_in_byte(&self.func.types)
            >= to.size_in_byte(&self.func.types)
        {
            return id;
        }

        self.node(IRNode::new(IROpcode::Sext).args(vec![id]).ty(to).into())
    }

    fn node_ty(&self, id: NodeId) -> Option<Type> {
        match &self.node_arena[id] {
            Node::IR(IRNode { ty, .. }) => Some(*ty),
            Node::MI(_) => None,
            Node::Operand(OperandNode::Imm(ImmediateKind::Int8(_))) => Some(Type::i8),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int32(_))) => Some(Type::i32),
            Node::Operand(OperandNode::Imm(ImmediateKind::Int64(_))) => Some(Type::i64),
            Node::Operand(OperandNode::Reg(id)) => {
                Some(rc2ty(self.regs.arena_ref()[*id].reg_class))
            }
            Node::Operand(_) => None,
            Node::None => None,
        }
    }

    pub fn make_chain(&mut self, id: NodeId) {
        *match &mut self.node_arena[self.last_chained_node] {
            Node::IR(IRNode { next, .. }) => next,
            Node::MI(MINode { next, .. }) => next,
            _ => panic!(),
        } = Some(id);
        self.last_chained_node = id;
    }

    fn make_chain_with_copying(&mut self, node: NodeId) -> NodeId {
        let reg: Node = self
            .regs
            .new_virt_reg(ty2rc(&self.node_ty(node).unwrap()).unwrap())
            .into();
        let old_node = ::std::mem::replace(&mut self.node_arena[node], reg.clone());
        let old_node = self.node(old_node);
        let reg = self.node(reg);
        let copy = self.node(
            IRNode::new(IROpcode::CopyToReg)
                .args(vec![reg, old_node])
                .into(),
        );
        self.make_chain(copy);
        node
    }
}
