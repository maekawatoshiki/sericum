use sericum::{
    codegen::arch::{asm::print::MachineAsmPrinter, standard_conversion_into_machine_module},
    ir::{builder::IRBuilder, module::Module, opcode::ICmpKind, types::Type},
};

#[test]
fn demo() {
    let mut module = Module::new("demo");
    let fibo = module.create_function("fibo", Type::i32, vec![Type::i32]);
    let mut builder = module.ir_builder(fibo);

    let entry = builder.append_basic_block();
    let block1 = builder.append_basic_block();
    let block2 = builder.append_basic_block();

    builder.set_insert_point(entry);
    let arg0 = builder.get_param(0).unwrap();
    let eq = builder.build_icmp(ICmpKind::Le, arg0, 1);
    builder.build_cond_br(eq, block1, block2);

    builder.set_insert_point(block1);
    builder.build_ret(1);

    builder.set_insert_point(block2);
    let arg1 = builder.build_sub(arg0, 1);
    let ret0 = builder.build_call(builder.new_func_value(fibo).unwrap(), vec![arg1]);
    let arg2 = builder.build_sub(arg0, 2);
    let ret1 = builder.build_call(builder.new_func_value(fibo).unwrap(), vec![arg2]);
    let add = builder.build_add(ret0, ret1);
    builder.build_ret(add);

    println!("IR:\n{:?}", module);

    let machine_module = standard_conversion_into_machine_module(module);
    let mut printer = MachineAsmPrinter::new();
    printer.run_on_module(&machine_module);

    println!("Assembly:\n{}", printer.output);
}
