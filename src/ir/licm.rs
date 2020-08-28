use crate::{
    analysis::{
        dom_tree::{DominatorTree, DominatorTreeConstructor},
        loops::{Loop, Loops, LoopsConstructor},
    },
    ir::{
        basic_block::{BasicBlock, BasicBlockId},
        builder::{Builder, FunctionEntity},
        function::Function,
        liveness::LivenessAnalyzerOnFunction,
        module::Module,
        opcode::{Instruction, Operand},
    },
};

pub struct LoopInvariantCodeMotion {}

struct LoopInvariantCodeMotionOnFunction<'a> {
    func: &'a mut Function,
}

impl LoopInvariantCodeMotion {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            LoopInvariantCodeMotionOnFunction::new(func).run();
        }
    }
}

impl<'a> LoopInvariantCodeMotionOnFunction<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }

    pub fn run(&mut self) {
        let dom_tree = DominatorTreeConstructor::new(&self.func.basic_blocks).construct();
        let loops = LoopsConstructor::new(&dom_tree, &self.func.basic_blocks).analyze();

        LivenessAnalyzerOnFunction::new(self.func).analyze();

        self.insert_pre_headers(loops);
    }

    fn insert_pre_headers(&mut self, loops: Loops<BasicBlock>) {
        for (_, loop_) in &loops.arena {
            self.insert_pre_header(loop_);
        }
    }

    fn insert_pre_header(&mut self, loop_: &Loop<BasicBlock>) {
        let pre_header = self.func.append_basic_block_before(loop_.header);

        let mut preds = self.func.basic_blocks.arena[loop_.header].pred.clone();
        preds.retain(|p| !loop_.contains(p));
        let preds_not_in_loop = preds;

        let header_preds = &mut self.func.basic_blocks.arena[loop_.header].pred;
        header_preds.retain(|p| preds_not_in_loop.contains(p));
        header_preds.insert(pre_header);

        for &pred in &preds_not_in_loop {
            let block = &mut self.func.basic_blocks.arena[pred];
            block.succ.retain(|&s| s != loop_.header);
            block.succ.insert(pre_header);
            for &id in block.iseq_ref().iter().rev() {
                let id = id.as_instruction().id;
                if !self.func.inst_table[id].opcode.is_terminator() {
                    break;
                }
                Instruction::replace_operand(
                    &mut self.func.inst_table,
                    id,
                    &Operand::BasicBlock(loop_.header),
                    Operand::BasicBlock(pre_header),
                );
            }
        }

        self.func.basic_blocks.arena[pre_header]
            .pred
            .extend(preds_not_in_loop);

        let mut builder = Builder::new(FunctionEntity(self.func));
        builder.set_insert_point(pre_header);
        builder.build_br(loop_.header);
    }
}
