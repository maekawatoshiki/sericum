// TODO: dirty code

use super::node::*;
use crate::codegen::arch::machine::register::*;
use crate::codegen::common::machine::inst::*;
use crate::codegen::common::{
    dag::{basic_block::*, function::*, module::*},
    machine::{basic_block::*, function::*, module::*},
};
use crate::ir::types::*;
use crate::util::allocator::*;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

pub struct ScheduleByBlock<'a> {
    pub types: &'a Types,
    pub cur_func: &'a DAGFunction,
    pub inst_arena: &'a mut InstructionArena,
    pub node2reg: FxHashMap<Raw<DAGNode>, Option<RegisterOperand>>,
    pub cur_bb: MachineBasicBlockId,
    pub iseq: &'a mut Vec<MachineInstId>,
    pub bb_map: &'a FxHashMap<DAGBasicBlockId, MachineBasicBlockId>,
    pub node2inst: &'a mut FxHashMap<Raw<DAGNode>, MachineInstId>,
}

pub fn convert_module(module: DAGModule) -> MachineModule {
    let mut functions = Arena::new();
    for (_, func) in module.functions {
        // TODO: refine code
        let id = functions.alloc(convert_function(&module.types, func));
        functions[id].id = Some(id);
    }
    MachineModule::new(module.name, functions, module.types, module.global_vars)
}

pub fn convert_function(types: &Types, dag_func: DAGFunction) -> MachineFunction {
    let mut bb_map = FxHashMap::default();
    let mut mbbs = MachineBasicBlocks::new();

    for dag_bb_id in &dag_func.dag_basic_blocks {
        let mbb_id = mbbs.arena.alloc(MachineBasicBlock::new());
        bb_map.insert(*dag_bb_id, mbb_id);
        mbbs.order.push(mbb_id);
    }

    for (dag, machine) in &bb_map {
        let dbb = &dag_func.dag_basic_block_arena[*dag];
        let mbb = &mut mbbs.arena[*machine];
        mbb.pred = dbb.pred.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
        mbb.succ = dbb.succ.iter().map(|bb| *bb_map.get(bb).unwrap()).collect();
    }

    let mut inst_arena = InstructionArena::new();
    let mut node2inst = FxHashMap::default();

    for dag_bb_id in &dag_func.dag_basic_blocks {
        let node = &dag_func.dag_basic_block_arena[*dag_bb_id];
        let bb_id = *bb_map.get(dag_bb_id).unwrap();
        let mut iseq = vec![];

        let entry = match node.entry {
            Some(entry) => entry,
            None => continue,
        };

        ScheduleByBlock {
            types,
            cur_func: &dag_func,
            inst_arena: &mut inst_arena,
            node2reg: FxHashMap::default(),
            cur_bb: bb_id,
            iseq: &mut iseq,
            bb_map: &bb_map,
            node2inst: &mut node2inst,
        }
        .convert(entry);

        mbbs.arena[bb_id].iseq = RefCell::new(iseq);
    }

    MachineFunction::new(dag_func, mbbs, inst_arena)
}

impl<'a> ScheduleByBlock<'a> {
    pub fn convert(&mut self, node: Raw<DAGNode>) -> Option<RegisterOperand> {
        if let Some(reg) = self.node2reg.get(&node) {
            return *reg;
        }

        let reg = match node.kind {
            NodeKind::IR(IRNodeKind::Entry) => None,
            NodeKind::IR(IRNodeKind::Root) => None,
            NodeKind::IR(IRNodeKind::RegClass) => {
                let val = self.normal_operand(node.operand[0]);
                Some(val.as_register().sub_super(ty2rc(&node.ty)))
            }
            _ => {
                let inst_id = self.convert_node_to_inst(node);
                self.inst_arena[inst_id].get_def_reg()
            }
        };
        self.node2reg.insert(node, reg);

        if let Some(next) = node.next {
            self.convert(next);
        }

        reg
    }

    pub fn get_machine_bb(&self, dag_bb_id: DAGBasicBlockId) -> MachineBasicBlockId {
        *self.bb_map.get(&dag_bb_id).unwrap()
    }

    pub fn append_inst(&mut self, inst: MachineInst) -> MachineInstId {
        let inst_id = self.inst_arena.alloc(&self.cur_func.regs_info, inst);
        self.iseq.push(inst_id);
        inst_id
    }
}
