use crate::codegen::{
    arch::machine::{abi::AAPCS64, register::*},
    common::{
        machine::calling_conv::{ArgumentRegisterOrder, CallingConv},
        new_dag::{
            convert::BlockConversionContext,
            node::{IRNode, IROpcode},
        },
    },
};

pub fn copy_reg_args<'a>(ctx: &mut BlockConversionContext<'a>) {
    let abi = AAPCS64::new();
    let mut arg_regs_order = ArgumentRegisterOrder::new(&abi);

    for i in 0..ctx.func.get_params_len() {
        if let Some(ty) = ctx.func.get_param_type(i) {
            let arg_reg_class = match ty2rc(&ty) {
                Some(rc) => rc,
                None => continue,
            };
            let arg_reg = match arg_regs_order.next(arg_reg_class) {
                Some(reg) => reg,
                None => continue,
            };
            let arg_reg = ctx.node(ctx.regs.get_phys_reg(arg_reg).into());
            let vreg = ctx.node(ctx.regs.new_virt_reg(arg_reg_class).into());
            let copy = ctx.node(
                IRNode::new(IROpcode::CopyToReg)
                    .args(vec![vreg, arg_reg])
                    .ty(ty)
                    .into(),
            );
            ctx.make_chain(copy);
            ctx.arg_regs.insert(i, vreg);
        }
    }
}
