use super::node::*;
use crate::codegen::arch::{frame_object::*, machine::register::*};
use crate::codegen::common::dag::{
    basic_block::*, convert::ConvertToDAGNode, function::*, module::*,
};
use crate::ir::{
    basic_block::*, function::*, liveness::*, module::*, opcode::*, types::*, value::*,
};
use crate::util::allocator::Raw;
use id_arena::*;
use rustc_hash::FxHashMap;
use std::mem;

impl<'a> ConvertToDAGNode<'a> {
    pub fn copy_reg_args(&mut self) {
        for i in 0..self.func.get_params_len() {
            if let Some(ty) = self.func.get_param_type(i) {
                let arg_reg_class = match ty2rc(&ty) {
                    Some(rc) => rc,
                    None => continue,
                };
                let arg_reg = match arg_reg_class.get_nth_arg_reg(i) {
                    Some(reg) => reg,
                    None => continue,
                };
                let arg_reg = self.alloc_node(DAGNode::new_phys_reg(&self.regs_info, arg_reg));
                let vreg = self.regs_info.new_virt_reg(arg_reg_class);
                let vreg = self.alloc_node(DAGNode::new(
                    NodeKind::Operand(OperandNodeKind::Register(vreg)),
                    vec![],
                    ty,
                ));
                let copy = self.alloc_node(DAGNode::new(
                    NodeKind::IR(IRNodeKind::CopyToReg),
                    vec![vreg, arg_reg],
                    ty,
                ));
                self.make_chain(copy);
                self.arg_regs.insert(i, vreg);
            }
        }
    }
}
