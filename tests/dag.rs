use cilk::{
    codegen::x64::{dag, machine},
    // exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, function, module, types, value},
    *,
};

#[test]
fn dag1() {
    let mut m = module::Module::new("cilk");

    let func = cilk_ir!(m; define [i32] func (i32) {
        entry:
            i = alloca i32;
            store (i32 10), (%i);
            x1 = load (%i);
            x2 = add (%x1), (%arg.0);
            br l1;
        l1:
            ret (%x2);
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
    });

    println!("{}", m.function_ref(func).to_string(&m));

    // let dag_func = dag::convert::ConvertToDAG::new(&m).construct_dag(func);
    let dag_module = dag::convert::ConvertToDAG::new(&m).convert_module();
    println!("DAG:");
    for (_, dag_func) in &dag_module.functions {
        for (id, bb) in &dag_func.dag_basic_blocks {
            println!("{}: {:?}", id.index(), bb);
        }
        for (id, dag) in &dag_func.dag_arena {
            println!("{}: {:?}", id.index(), dag);
        }
    }

    let machine_module = dag::convert_machine::ConvertToMachine::new(&dag_module).convert_module();
    machine::liveness::LivenessAnalysis::new(&machine_module).analyze_module();
    machine::regalloc::PhysicalRegisterAllocator::new(&machine_module).run_on_module();

    for (_, machine_func) in &machine_module.functions {
        for (_, bb) in &machine_func.basic_blocks {
            println!("Machine basic block: {:?}", bb);
            for instr in &bb.iseq {
                println!("{}: {:?}", instr.index(), machine_func.instr_arena[*instr]);
            }
            println!()
        }
    }

    let mut jit = machine::jit::JITCompiler::new(&machine_module);
    jit.compile_module();
    let func = machine_module.find_function_by_name("func").unwrap();
    println!(
        "ret: {:?}",
        jit.run(func, vec![machine::jit::GenericValue::Int32(2)])
    );
}
