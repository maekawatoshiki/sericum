use super::super::register::*;
use super::{basic_block::*, frame_object::*, function::*, module::*, node::*};
use crate::ir::{
    basic_block::*, function::*, liveness::*, module::*, opcode::*, types::*, value::*,
};
use id_arena::*;
use rustc_hash::FxHashMap;

pub struct ConvertToDAG<'a> {
    pub module: &'a Module,
    pub instr_id_node_id: FxHashMap<InstructionId, DAGNodeId>,
    pub instr_id_to_copy_node: FxHashMap<InstructionId, DAGNodeId>,
    pub cur_conversion_info: Option<ConversionInfo>,
}

pub struct ConversionInfo {
    pub dag_arena: Arena<DAGNode>,
    pub local_mgr: LocalVariableManager,
    pub bb_to_dag_bb: FxHashMap<BasicBlockId, DAGBasicBlockId>,
    pub last_chain_node: Option<DAGNodeId>,
    pub vreg_gen: VirtRegGen,
}

impl<'a> ConvertToDAG<'a> {
    pub fn new(module: &'a Module) -> Self {
        IRLivenessAnalyzer::new(&module).analyze();

        Self {
            module,
            instr_id_node_id: FxHashMap::default(),
            instr_id_to_copy_node: FxHashMap::default(),
            cur_conversion_info: None,
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
        self.cur_conversion_info = Some(ConversionInfo::new());
        self.instr_id_node_id.clear();

        let func = self.module.function_ref(func_id);

        when_debug!(println!(
            "{}: dump function: \n{}",
            file!(),
            func.to_string(&self.module)
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
            self.instr_id_to_copy_node.clear();

            let bb = &func.basic_block_arena[*bb_id];
            let id = self.construct_dag_from_basic_block(func, bb);
            dag_bb_arena[self.cur_conv_info_ref().get_dag_bb(*bb_id)].set_entry(id);
        }

        let conv_info = ::std::mem::replace(&mut self.cur_conversion_info, None).unwrap();
        DAGFunction::new(
            func,
            conv_info.dag_arena,
            dag_bb_arena,
            dag_bb_list,
            conv_info.local_mgr,
            conv_info.vreg_gen,
        )
    }

    pub fn get_dag_id_from_value(&mut self, v: &Value, arg_load: bool) -> DAGNodeId {
        match v {
            Value::Instruction(iv) => self.instr_id_node_id[&iv.id],
            Value::Immediate(ImmediateValue::Int32(i)) => {
                self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                    DAGNodeKind::Constant(ConstantKind::Int32(*i)),
                    vec![],
                    Type::Int32,
                ))
            }
            Value::Argument(av) => {
                let ty = self
                    .module
                    .function_ref(av.func_id)
                    .get_param_type(av.index)
                    .unwrap();
                let fi = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                    DAGNodeKind::FrameIndex(-(av.index as i32 + 1), ty.clone()),
                    vec![],
                    ty.clone(),
                ));
                if arg_load {
                    let load_id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Load,
                        vec![fi],
                        ty.clone(),
                    ));
                    self.make_chain(load_id);
                    load_id
                } else {
                    fi
                }
            }
            Value::Function(fid) => {
                let f = self.module.function_ref(*fid);
                self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                    DAGNodeKind::GlobalAddress(GlobalValueKind::FunctionName(f.name.to_string())),
                    vec![],
                    Type::Void, // TODO
                ))
            }
            Value::None => self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                DAGNodeKind::None,
                vec![],
                Type::Void,
            )),
        }
    }

    pub fn construct_dag_from_basic_block(
        &mut self,
        func: &Function,
        bb: &BasicBlock,
    ) -> DAGNodeId {
        let entry_node = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
            DAGNodeKind::Entry,
            vec![],
            Type::Void,
        ));
        self.cur_conv_info_mut().last_chain_node = Some(entry_node);

        macro_rules! make_chain {
            ($dag_id:expr) => {{
                let dag_id = $dag_id;
                let conv_info = self.cur_conv_info_mut();
                if let Some(last_node_id) = &mut conv_info.last_chain_node {
                    conv_info.dag_arena[*last_node_id].next = Some(dag_id);
                    *last_node_id = dag_id;
                }
            }};
        }

        for instr_val in bb.iseq_ref().iter() {
            let instr_id = instr_val.get_instr_id().unwrap();
            let instr = &func.instr_table[instr_id];

            match instr.opcode {
                Opcode::Alloca(ref ty) => {
                    let fi = self.cur_conv_info_mut_with(|c| {
                        let frinfo = c.local_mgr.alloc(ty);
                        c.dag_arena.alloc(DAGNode::new(
                            DAGNodeKind::FrameIndex(frinfo.idx, frinfo.ty), // TODO
                            vec![],
                            ty.clone(),
                        ))
                    });
                    self.instr_id_node_id.insert(instr_id, fi);
                }
                Opcode::Load(ref v) => {
                    let v = self.get_dag_id_from_value(v, true);
                    let load_id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Load,
                        vec![v],
                        instr.ty.clone(),
                    ));

                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        let copy_from_reg = self.make_chain_with_copying(load_id);
                        self.instr_id_to_copy_node.insert(instr_id, copy_from_reg);
                        self.instr_id_node_id.insert(instr_id, load_id);
                    } else {
                        make_chain!(load_id);
                        self.instr_id_node_id.insert(instr_id, load_id);
                    }
                }
                Opcode::Store(ref src, ref dst) => {
                    let dst = self.get_dag_id_from_value(dst, true);
                    let src = self.get_dag_id_from_value(src, true);
                    let id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Store,
                        vec![dst, src],
                        Type::Void,
                    ));
                    make_chain!(id);
                }
                Opcode::GetElementPtr(ref ptr, ref indices) => {
                    let gep = self.construct_dag_for_gep(instr, ptr, indices);
                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        make_chain!(gep);
                    }
                    self.instr_id_node_id.insert(instr_id, gep);
                }
                Opcode::Call(ref f, ref args) => {
                    let mut operands: Vec<DAGNodeId> = args
                        .iter()
                        .map(|a| self.get_dag_id_from_value(a, true))
                        .collect();
                    operands.insert(0, self.get_dag_id_from_value(f, true));
                    let id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Call,
                        operands,
                        instr.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.instr_id_to_copy_node.insert(instr_id, copy_from_reg);
                        self.instr_id_node_id.insert(instr_id, id);
                    } else {
                        make_chain!(id);
                        self.instr_id_node_id.insert(instr_id, id);
                    }
                }
                Opcode::Add(ref v1, ref v2)
                | Opcode::Sub(ref v1, ref v2)
                | Opcode::Mul(ref v1, ref v2)
                | Opcode::Rem(ref v1, ref v2) => {
                    let v1 = self.get_dag_id_from_value(v1, true);
                    let v2 = self.get_dag_id_from_value(v2, true);
                    let bin_id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        match instr.opcode {
                            Opcode::Add(_, _) => DAGNodeKind::Add,
                            Opcode::Sub(_, _) => DAGNodeKind::Sub,
                            Opcode::Mul(_, _) => DAGNodeKind::Mul,
                            Opcode::Rem(_, _) => DAGNodeKind::Rem,
                            _ => unreachable!(),
                        },
                        vec![v1, v2],
                        instr.ty.clone(),
                    ));

                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        let copy_from_reg = self.make_chain_with_copying(bin_id);
                        self.instr_id_to_copy_node.insert(instr_id, copy_from_reg);
                        self.instr_id_node_id.insert(instr_id, bin_id);
                    } else {
                        self.instr_id_node_id.insert(instr_id, bin_id);
                    }
                }
                Opcode::Br(bb) => make_chain!(self.cur_conv_info_mut_with(|c| {
                    let bb = c.dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::BasicBlock(c.get_dag_bb(bb)),
                        vec![],
                        Type::Void,
                    ));
                    c.dag_arena
                        .alloc(DAGNode::new(DAGNodeKind::Br, vec![bb], Type::Void))
                })),
                Opcode::CondBr(ref v, then_, else_) => {
                    let v = self.get_dag_id_from_value(v, true);
                    make_chain!({
                        let c = self.cur_conv_info_mut();
                        let bb = c.dag_arena.alloc(DAGNode::new(
                            DAGNodeKind::BasicBlock(c.get_dag_bb(then_)),
                            vec![],
                            Type::Void,
                        ));
                        c.dag_arena.alloc(DAGNode::new(
                            DAGNodeKind::BrCond,
                            vec![v, bb],
                            Type::Void,
                        ))
                    });
                    make_chain!(self.cur_conv_info_mut_with(|c| {
                        let bb = c.dag_arena.alloc(DAGNode::new(
                            DAGNodeKind::BasicBlock(c.get_dag_bb(else_)),
                            vec![],
                            Type::Void,
                        ));
                        c.dag_arena
                            .alloc(DAGNode::new(DAGNodeKind::Br, vec![bb], Type::Void))
                    }));
                }
                Opcode::ICmp(ref c, ref v1, ref v2) => {
                    let v1 = self.get_dag_id_from_value(v1, true);
                    let v2 = self.get_dag_id_from_value(v2, true);
                    let cond = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::CondKind((*c).into()),
                        vec![],
                        Type::Void,
                    ));
                    let id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Setcc,
                        vec![cond, v1, v2],
                        instr.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.instr_id_to_copy_node.insert(instr_id, copy_from_reg);
                        self.instr_id_node_id.insert(instr_id, id);
                    } else {
                        self.instr_id_node_id.insert(instr_id, id);
                    }
                }
                Opcode::Phi(ref pairs) => {
                    let mut operands = vec![];
                    for (val, bb) in pairs {
                        // Remove CopyFromReg if necessary
                        let val_id = self.get_dag_id_from_value(val, true);
                        let val = &self.cur_conv_info_ref().dag_arena[val_id];
                        operands.push(match val.kind {
                            DAGNodeKind::CopyFromReg => val.operand[0],
                            _ => val_id,
                        });

                        operands.push(self.cur_conv_info_mut_with(|c| {
                            c.dag_arena.alloc(DAGNode::new(
                                DAGNodeKind::BasicBlock(c.get_dag_bb(*bb)),
                                vec![],
                                Type::Void,
                            ))
                        }))
                    }
                    let id = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Phi,
                        operands,
                        instr.ty.clone(),
                    ));
                    if bb.liveness.borrow().live_out.contains(&instr_id) {
                        let copy_from_reg = self.make_chain_with_copying(id);
                        self.instr_id_to_copy_node.insert(instr_id, copy_from_reg);
                        self.instr_id_node_id.insert(instr_id, id);
                    } else {
                        self.instr_id_node_id.insert(instr_id, id);
                    }
                }
                Opcode::Ret(ref v) => {
                    let v = self.get_dag_id_from_value(v, true);
                    make_chain!(self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                        DAGNodeKind::Ret,
                        vec![v],
                        Type::Void
                    )))
                }
            }
        }

        for instr_liveout in &bb.liveness.borrow().live_out {
            if !self.instr_id_to_copy_node.contains_key(instr_liveout) {
                continue;
            }

            let copy_to_live_out = *self.instr_id_to_copy_node.get(instr_liveout).unwrap();
            self.instr_id_node_id
                .insert(*instr_liveout, copy_to_live_out);
        }

        entry_node
    }

    fn construct_dag_for_gep(
        &mut self,
        _instr: &Instruction,
        ptr: &Value,
        indices: &[Value],
    ) -> DAGNodeId {
        let mut gep = self.get_dag_id_from_value(ptr, false);
        let mut ty = ptr.get_type(self.module);

        for idx in indices {
            ty = ty.get_element_ty().unwrap();

            let idx = self.get_dag_id_from_value(idx, true);
            let arena = &mut self.cur_conv_info_mut().dag_arena;
            let idx = match arena[idx].kind {
                DAGNodeKind::Constant(ConstantKind::Int32(i)) => arena.alloc(DAGNode::new(
                    DAGNodeKind::Constant(ConstantKind::Int32(i * ty.size_in_byte() as i32)),
                    vec![],
                    Type::Int32,
                )),
                DAGNodeKind::CondKind(_)
                | DAGNodeKind::FrameIndex(_, _)
                | DAGNodeKind::GlobalAddress(_)
                | DAGNodeKind::BasicBlock(_) => idx,
                _ => {
                    let tysz = arena.alloc(DAGNode::new(
                        DAGNodeKind::Constant(ConstantKind::Int32(ty.size_in_byte() as i32)),
                        vec![],
                        Type::Int32,
                    ));
                    arena.alloc(DAGNode::new(DAGNodeKind::Mul, vec![idx, tysz], ty.clone()))
                }
            };

            gep = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
                DAGNodeKind::Add,
                vec![gep, idx],
                ty.clone(), // TODO
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

    fn make_chain(&mut self, dag_id: DAGNodeId) {
        let conv_info = self.cur_conv_info_mut();
        if let Some(last_node_id) = &mut conv_info.last_chain_node {
            conv_info.dag_arena[*last_node_id].next = Some(dag_id);
            *last_node_id = dag_id;
        }
    }

    fn make_chain_with_copying(&mut self, node_id: DAGNodeId) -> DAGNodeId {
        let copy_to_live_out = self.cur_conv_info_mut().dag_arena.alloc(DAGNode::new(
            DAGNodeKind::CopyToLiveOut,
            vec![node_id],
            Type::Void,
        ));

        self.make_chain(copy_to_live_out);

        copy_to_live_out
    }
}

impl ConversionInfo {
    pub fn new() -> Self {
        ConversionInfo {
            dag_arena: Arena::new(),
            local_mgr: LocalVariableManager::new(),
            bb_to_dag_bb: FxHashMap::default(),
            last_chain_node: None,
            vreg_gen: VirtRegGen::new(),
        }
    }

    pub fn get_dag_bb(&self, bb_id: BasicBlockId) -> DAGBasicBlockId {
        *self.bb_to_dag_bb.get(&bb_id).unwrap()
    }
}
