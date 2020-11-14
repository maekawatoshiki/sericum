use super::node::*;
use crate::codegen::arch::machine::register::ty2rc;
// use crate::codegen::arch::new_dag::mc_convert::
use crate::codegen::common::machine::inst::*;
use crate::codegen::common::{
    machine::{basic_block::*, const_data::ConstDataArena, function::*, module::*},
    new_dag::{basic_block::*, function::*, module::*},
};
use crate::ir::types::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

pub type FuncTyMap = FxHashMap<String, Type>;

pub struct ScheduleContext<'a> {
    pub func_map: &'a FuncTyMap,
    pub func: &'a DAGFunction,
    pub inst_arena: &'a mut InstructionArena,
    pub node2reg: FxHashMap<NodeId, Option<RegisterOperand>>,
    pub block_id: MachineBasicBlockId,
    pub iseq: &'a mut Vec<MachineInstId>,
    pub bb_map: &'a FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
    pub node2inst: &'a mut FxHashMap<NodeId, MachineInstId>,
}

pub fn convert_module(module: DAGModule) -> MachineModule {
    let mut functions = Arena::new();
    let mut func_map = FuncTyMap::default();
    for (_, func) in &module.functions {
        func_map.insert(func.name.to_string(), func.ty);
    }
    for (_, func) in module.functions {
        // TODO: refine code
        let id = functions.alloc(convert_function(&func_map, func));
        functions[id].id = Some(id);
    }
    MachineModule::new(
        module.name,
        functions,
        module.types,
        module.global_vars,
        module.const_pool,
    )
}

pub fn convert_function(func_map: &FuncTyMap, func: DAGFunction) -> MachineFunction {
    let mut bb_map = FxHashMap::default();
    let mut basic_blocks = MachineBasicBlocks::new();

    for dag_bb_id in &func.dag_basic_blocks {
        let mbb_id = basic_blocks.arena.alloc(MachineBasicBlock::new());
        bb_map.insert(*dag_bb_id, mbb_id);
        basic_blocks.order.push(mbb_id);
    }

    for (dag, machine) in &bb_map {
        let dbb = &func.dag_basic_block_arena[*dag];
        let mbb = &mut basic_blocks.arena[*machine];
        mbb.pred = dbb.pred.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
        mbb.succ = dbb.succ.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
    }

    let mut inst_arena = InstructionArena::new();
    let mut node2inst = FxHashMap::default();

    for dag_bb_id in &func.dag_basic_blocks {
        let node = &func.dag_basic_block_arena[*dag_bb_id];
        let bb_id = *bb_map.get(dag_bb_id).unwrap();
        let mut iseq = vec![];

        let entry = match node.entry {
            Some(entry) => entry,
            None => continue,
        };

        ScheduleContext {
            func_map,
            func: &func,
            inst_arena: &mut inst_arena,
            node2reg: FxHashMap::default(),
            block_id: bb_id,
            iseq: &mut iseq,
            bb_map: &bb_map,
            node2inst: &mut node2inst,
        }
        .convert(entry);

        basic_blocks.arena[bb_id].iseq = RefCell::new(iseq);
    }

    MachineFunction {
        id: None,
        is_internal: func.is_internal,
        name: func.name,
        ty: func.ty,
        body: MachineFunctionBody {
            inst_arena,
            basic_blocks,
        },
        local_mgr: func.local_vars,
        regs_info: func.regs,
        frame_objects: None,
        const_data: ConstDataArena::new(),
        types: func.types.clone(),
    }
}

impl<'a> ScheduleContext<'a> {
    pub fn convert(&mut self, node: NodeId) -> Option<RegisterOperand> {
        if let Some(reg) = self.node2reg.get(&node) {
            return *reg;
        }

        let reg = match &self.func.node_arena[node] {
            Node::IR(IRNode {
                opcode: IROpcode::Entry,
                ..
            })
            | Node::IR(IRNode {
                opcode: IROpcode::Root,
                ..
            }) => None,
            Node::IR(IRNode {
                opcode: IROpcode::RegClass,
                args,
                ty,
                ..
            }) => {
                let val = self.normal_arg(args[0]);
                Some(val.as_register().sub_super(ty2rc(&ty)))
            }
            _ => {
                let inst_id = self.convert_node(node);
                self.inst_arena[inst_id].get_def_reg()
            }
        };
        self.node2reg.insert(node, reg);

        if let Some(next) = self.func.node_arena[node].next() {
            self.convert(next);
        }

        reg
    }

    pub fn append_inst(&mut self, inst: MachineInst) -> MachineInstId {
        let inst_id = self.inst_arena.alloc(&self.func.regs, inst);
        self.iseq.push(inst_id);
        inst_id
    }
}
