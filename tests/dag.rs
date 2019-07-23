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
            store (%arg.0), (%i);
            c = icmp eq (%i), (i32 1);
            br (%c) l1, l2;
        l1:
            x = load (%i);
            x2 = load (%i);
            y = add (%x), (%x2);
            ret (%y);
        l2:
            ret (i32 1);
    });

    println!("{}", m.function_ref(func).to_string(&m));

    let dag_func = dag::convert::ConvertToDAG::new(&m).construct_dag(func);

    for (id, bb) in &dag_func.dag_basic_blocks {
        println!("{}: {:?}", id.index(), bb);
    }
    for (id, dag) in &dag_func.dag_arena {
        println!("{}: {:?}", id.index(), dag);
    }

    let machine_func = dag::convert_machine::ConvertToMachine::new(&m).convert_function(&dag_func);
    machine::liveness::LivenessAnalysis::new(&m).analyze_function(&machine_func);
    machine::regalloc::PhysicalRegisterAllocator::new().run_on_function(&machine_func);

    for (_, bb) in &machine_func.basic_blocks {
        println!("Machine basic block: {:?}", bb);
        for instr in &bb.iseq {
            println!("{}: {:?}", instr.index(), machine_func.instr_arena[*instr]);
        }
        println!()
    }
}
