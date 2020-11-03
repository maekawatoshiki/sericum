use super::node::*;
use crate::codegen::arch::{machine::abi::AAPCS64, machine::register::*};
use crate::codegen::common::{
    dag::convert::ConvertToDAGNode, machine::calling_conv::ArgumentRegisterOrder,
};

impl<'a> ConvertToDAGNode<'a> {
    pub fn copy_reg_args(&mut self) {
        let abi = AAPCS64::new();
        let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);

        for i in 0..self.func.get_params_len() {
            if let Some(ty) = self.func.get_param_type(i) {
                let arg_reg_class = match ty2rc(&ty) {
                    Some(rc) => rc,
                    None => continue,
                };
                let arg_reg = match arg_regs_order.next(arg_reg_class) {
                    Some(reg) => reg,
                    None => continue,
                };
                let arg_reg = self.alloc_node(DAGNode::new_phys_reg(&self.regs_info, arg_reg));
                let vreg = self.regs_info.new_virt_reg(arg_reg_class);
                let vreg = self.alloc_node(
                    DAGNode::new(NodeKind::Operand(OperandNodeKind::Register(vreg))).with_ty(ty),
                );
                let copy = self.alloc_node(
                    DAGNode::new(NodeKind::IR(IRNodeKind::CopyToReg))
                        .with_operand(vec![vreg, arg_reg])
                        .with_ty(ty),
                );
                self.make_chain(copy);
                self.arg_regs.insert(i, vreg);
            }
        }
    }
}
