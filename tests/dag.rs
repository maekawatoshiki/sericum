use cilk::{
    codegen::x64::{dag, exec, machine},
    // exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn dag1() {
    let mut m = module::Module::new("cilk");

    // Internal function must be defined when you use it
    let cilk_println_i32 = m.add_function(ir::function::Function::new(
        "cilk.println.i32",
        ir::types::Type::Void,
        vec![ir::types::Type::Int32],
    ));

    let func = cilk_ir!(m; define [i32] func (i32) {
        // entry:
        //     i = alloca i32;
        //     store (i32 10), (%i);
        //     x1 = load (%i);
        //     x2 = add (%x1), (%arg.0);
        //     br l1;
        // l1:
        //     ret (%x2);

        // entry:
        //     i = alloca i32;
        //     store (i32 1), (%i);
        //     c = icmp eq (%i), (i32 1);
        //     br (%c) l1, l2;
        // l1:
        //     x = load (%i);
        //     x2 = load (%i);
        //     y = add (%x), (%x2);
        //     ret (%y);
        // l2:
        //     ret (i32 1);

        // entry:
        //     // i = alloca i32;
        //     // store (i32 10), (%i);
        //     // li = load (%i);
        //     // c = icmp eq (%li), (%arg.0);
        //     x = sub (%arg.0), (i32 3);
        //     c = icmp eq (%x), (i32 10);
        //     br (%c) l1, l2;
        // l1:
        //     ret (i32 0);
        // l2:
        //     ret (i32 1);

        // entry:
        //     c = icmp eq (%arg.0), (i32 8);
        //     br (%c) l1, l2;
        // l1:
        //     a = add (%arg.0), (i32 2);
        //     br merge;
        // l2:
        //     s = sub (%arg.0), (i32 1);
        //     br merge;
        // merge:
        //     p = phi [ [(%a), l1], [(%s), l2] ];
        //     ret (%p);

        // entry:
        //     i = alloca i32;
        //     cond = icmp eq (%arg.0), (i32 2);
        //     br (%cond) l1, l2;
        // l1:
        //     ret (i32 1);
        // l2:
        //     r = rem (%arg.0), (i32 2);
        //     cond = icmp eq (%r), (i32 0);
        //     br (%cond) l3, l4;
        // l3:
        //     ret (i32 0);
        // l4:
        //     store (i32 3), (%i);
        //     br l5;
        // l5:
        //     li = load (%i);
        //     m = mul (%li), (%li);
        //     cond = icmp le (%m), (%arg.0);
        //     br (%cond) l6, l7;
        // l6:
        //     li = load (%i);
        //     r = rem (%arg.0), (%li);
        //     cond = icmp eq (%r), (i32 0);
        //     br (%cond) l8, l9;
        // l8:
        //     ret (i32 0);
        // l9:
        //     a = add (%li), (i32 2);
        //     store (%a), (%i);
        //     br l5;
        // l7:
        //     ret (i32 1);

        // entry:
        //     i = alloca i32;
        //     store (i32 0), (%i);
        //     li = load (%i);
        //     __ = call (->cilk_println_i32) [(i32 0)];
        //     li2 = load (%i);
        //     __ = call (->cilk_println_i32) [(%li)];
        //     ret (%li2);

        // for (int i = 0; i < 2; i++)
        //   for (int k = 0; k < 2; k++)
        //     a[i][k] = i + k;
        // entry:
        //     a = alloca_ ([2; [2; i32]]);
        //     i = alloca i32;
        //     k = alloca i32;
        //     store (i32 0), (%i);
        //     store (i32 0), (%k);
        //     br l1;
        // l1:
        //     li = load (%i);
        //     c = icmp lt (%li), (i32 2);
        //     br (%c) l2, l3;
        // l2:
        //     lk = load (%k);
        //     c = icmp lt (%lk), (i32 2);
        //     br (%c) l4, l5;
        // l4:
        //     g = gep (%a), [(i32 0), (%li), (%lk)];
        //     a = add (%li), (%lk);
        //     store (%a), (%g);
        //     a = add (%lk), (i32 1);
        //     store (%a), (%k);
        //     lg = load (%g);
        //     __ = call (->cilk_println_i32) [(%lg)];
        //     br l2;
        // l5:
        //     store (i32 0), (%k);
        //     a = add (%li), (i32 1);
        //     store (%a), (%i);
        //     br l1;
        // l3:
        //     ret (i32 0);

        entry:
            a = add (%arg.0), (i32 2);
            i = rem (%arg.0), (i32 3);
            a = add (%a), (%i);
            ret (%a);

        // entry:
        //     i = alloca i32;
        //     store (i32 2), (%i);
        //     li = load (%i);
        //     c = icmp eq (%li), (i32 2);
        //     br (%c) l1, l2;
        // l1:
        //     a = add (%li), (i32 3);
        //     br l3;
        // l2:
        //     b = add (%li), (i32 2);
        //     br l3;
        // l3:
        //     p = phi [ [(%a), l1], [(%b), l2] ];
        //     __ = call (->cilk_println_i32) [(%p)];
        //     ret (i32 0);

        // entry:
        //     cond = icmp le (%arg.0), (i32 2);
        //     br (%cond) l1, l2;
        // l1:
        //     ret (i32 1);
        // l2:
        //     a1 = sub (%arg.0), (i32 1);
        //     r1 = call func [(%a1)];
        //     a2 = sub (%arg.0), (i32 2);
        //     r2 = call func [(%a2)];
        //     r3 = add (%r1), (%r2);
        //     ret (%r3);
    });

    // let func = cilk_ir!(m; define [i32] func () {
    //     entry:
    //         a = alloca_ ([8; i32]);
    //
    //         idx = gep (%a), [(i32 0), (i32 3)];
    //         store (i32 123), (%idx);
    //
    //         // idx = gep (%a), [(i32 0), (i32 3)];
    //         // l = load (%idx);
    //         ret (i32 0);
    // });

    println!("{}", m.function_ref(func).to_string(&m));
    // println!("{}", m.function_ref(main).to_string(&m));

    let mut dag_module = dag::convert::ConvertToDAG::new(&m).convert_module();
    dag::combine::Combine::new().combine_module(&mut dag_module);
    println!("DAG:");
    for (_, dag_func) in &dag_module.functions {
        for id in &dag_func.dag_basic_blocks {
            let bb = &dag_func.dag_basic_block_arena[*id];
            println!("{}: {:?}", id.index(), bb);
        }
        for (id, dag) in &dag_func.dag_arena {
            println!("{}: {:?}", id.index(), dag);
        }
    }

    let mut machine_module =
        dag::convert_machine::ConvertToMachine::new().convert_module(dag_module);
    machine::phi_elimination::PhiElimination::new().run_on_module(&mut machine_module);
    machine::two_addr::TwoAddressConverter::new().run_on_module(&mut machine_module);
    // machine::regalloc::PhysicalRegisterAllocator::new().run_on_module(&mut machine_module);
    machine::regalloc::RegisterAllocator::new().run_on_module(&mut machine_module);

    let mut idx = 0;
    for (_, machine_func) in &machine_module.functions {
        for bb_id in &machine_func.basic_blocks {
            let bb = &machine_func.basic_block_arena[*bb_id];
            println!("Machine basic block: {:?}", bb);
            for instr in &*bb.iseq_ref() {
                println!("{}: {:?}", idx, machine_func.instr_arena[*instr]);
                idx += 1;
            }
            println!()
        }
    }

    //
    //
    // let mut jit = exec::jit::JITCompiler::new(&machine_module);
    // jit.compile_module();
    // let func = machine_module.find_function_by_name("func").unwrap();
    // println!(
    //     "ret: {:?}",
    //     jit.run(func, vec![exec::jit::GenericValue::Int32(40)])
    // );
}
