use super::node::*;
use crate::codegen::{
    arch::machine::{abi::SystemV, register::*},
    common::{
        dag::convert::ConvertToDAGNode,
        machine::calling_conv::{ArgumentRegisterOrder, CallingConv},
    },
};

impl<'a> ConvertToDAGNode<'a> {
    // TODO: Refine
    pub fn copy_reg_args(&mut self) {
        let abi = SystemV::new();
        let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);

        for i in 0..self.func.get_params_len() {
            let byval = self.func.get_param_attr(i).map_or(false, |attr| attr.byval);
            if let Some(ty) = self.func.get_param_type(i) {
                if byval {
                    let base = &self.func.types.base.borrow();
                    let struct_ty = base
                        .as_struct_ty(base.get_element_ty(ty, None).unwrap())
                        .unwrap();
                    let sz = struct_ty.size();
                    let regs_classes = SystemV::reg_classes_used_for_passing_byval(struct_ty);
                    if sz <= 16 && arg_regs_order.regs_available_for(&regs_classes) {
                        for rc in regs_classes {
                            assert!(arg_regs_order.next(rc).is_some())
                        }
                    }
                    continue;
                }

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
