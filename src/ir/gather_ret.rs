use crate::ir::{
    builder::FunctionBuilder, function::Function, module::Module, opcode::Opcode, types::Type,
    value::Value,
};

pub struct GatherReturns {}

struct GatherReturnsOnFunction<'a> {
    func: &'a mut Function,
}

impl GatherReturns {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut Module) {
        for (_, func) in &mut module.functions {
            if func.is_internal {
                continue;
            }

            GatherReturnsOnFunction { func }.run();
        }
    }
}

impl<'a> GatherReturnsOnFunction<'a> {
    pub fn run(&mut self) {
        let ret_void = self
            .func
            .types
            .base
            .borrow()
            .as_function_ty(self.func.ty)
            .unwrap()
            .ret_ty
            == Type::Void;

        let mut returns = vec![];
        for &id in &self.func.basic_blocks.order {
            let bb = &self.func.basic_blocks.arena[id];
            let iseq = bb.iseq.borrow();
            let val = match iseq.last() {
                Some(val) => val,
                None => continue,
            };
            let inst_id = val.get_inst_id().unwrap();
            let inst = &self.func.inst_table[inst_id];
            if inst.opcode == Opcode::Ret {
                returns.push(inst_id)
            }
        }

        if returns.len() <= 1 {
            return;
        }

        let mut builder = FunctionBuilder::new(self.func);
        let mut pairs = vec![];
        let ret_bb = builder.append_basic_block();

        for ret_id in returns {
            let ret = &builder.func.inst_table[ret_id];
            let parent = ret.parent;
            if !ret_void {
                pairs.push((*ret.operands[0].as_value(), parent));
            }
            builder.func.remove_inst(ret_id);
            builder.set_insert_point(parent);
            builder.build_br(ret_bb);
        }

        builder.set_insert_point(ret_bb);
        let val = if !ret_void {
            builder.build_phi(pairs)
        } else {
            Value::None
        };
        builder.build_ret(val);
    }
}
