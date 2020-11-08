use crate::codegen::common::dag::{
    function::DAGFunction,
    module::DAGModule,
    node::{IRNode, IROpcode, Node, NodeId},
};
use crate::codegen::common::new_dag::basic_block::{DAGBasicBlock, DAGBasicBlockId};
use crate::ir::{
    basic_block::{BasicBlock, BasicBlockId},
    function::Function,
    liveness::IRLivenessAnalyzer,
    module::Module,
};
use id_arena::Arena;
use rustc_hash::FxHashMap;

impl Into<DAGModule> for Module {
    fn into(self) -> DAGModule {
        convert_module_to_dag_module(self)
    }
}

fn convert_module_to_dag_module(mut module: Module) -> DAGModule {
    IRLivenessAnalyzer::new(&mut module).analyze();

    let mut functions: Arena<DAGFunction> = Arena::new();

    for (_, func) in &module.functions {
        convert_function_to_dag_function(FunctionConversionContext {
            module: &module,
            func,
            node_arena: Arena::new(),
        });
    }

    todo!()
}

struct FunctionConversionContext<'a> {
    module: &'a Module,
    func: &'a Function,
    node_arena: Arena<Node>,
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
        convert_block_to_dag_block(BlockConversionContext::new(
            ctx.module,
            ctx.func,
            &mut ctx.node_arena,
            is_entry,
            block,
            id,
        ));
    }

    todo!()
}

struct BlockConversionContext<'a> {
    module: &'a Module,
    func: &'a Function,
    node_arena: &'a mut Arena<Node>,
    is_entry: bool,
    block: &'a BasicBlock,
    block_id: BasicBlockId,
    last_chained_node: NodeId,
}

fn convert_block_to_dag_block<'a>(mut ctx: BlockConversionContext<'a>) {
    if ctx.is_entry {
        // self.copy_reg_args();
    }

    for &id in &*ctx.block.iseq_ref() {
        let inst = &ctx.func.inst_table[id];
    }

    todo!()
}

impl<'a> BlockConversionContext<'a> {
    pub fn new(
        module: &'a Module,
        func: &'a Function,
        node_arena: &'a mut Arena<Node>,
        is_entry: bool,
        block: &'a BasicBlock,
        block_id: BasicBlockId,
    ) -> Self {
        let last_chained_node = node_arena.alloc(IRNode::new(IROpcode::Entry).into());
        Self {
            module,
            func,
            node_arena,
            is_entry,
            block,
            block_id,
            last_chained_node,
        }
    }

    pub fn node(&mut self, node: Node) -> NodeId {
        self.node_arena.alloc(node)
    }
}
