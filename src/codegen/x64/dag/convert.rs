use super::node::*;
use crate::codegen::arch::machine::register::*;
use crate::codegen::common::dag::convert::ConvertToDAGNode;
use crate::ir::types::TypeSize;
// use crate::util::allocator::Raw;
// use id_arena::*;
// use rustc_hash::FxHashMap;
// use std::mem;

impl<'a> ConvertToDAGNode<'a> {
    pub fn copy_reg_args(&mut self) {
        let mut arg_regs_order = RegisterClassKind::arg_regs();
        for i in 0..self.func.get_params_len() {
            let byval = self.func.get_param_attr(i).map_or(false, |attr| attr.byval);
            if let Some(ty) = self.func.get_param_type(i) {
                if byval {
                    let struct_ty = self.func.types.get_element_ty(ty, None).unwrap();
                    let sz = struct_ty.size_in_byte(&self.func.types);

                    let mov8 = sz / 8;
                    let mov4 = (sz - 8 * mov8) / 4;
                    assert!((sz - 8 * mov8) % 4 == 0);

                    for (c, rc) in vec![
                        (mov8, RegisterClassKind::GR64),
                        (mov4, RegisterClassKind::GR32),
                    ] {
                        for _ in 0..c {
                            arg_regs_order.next(rc);
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
