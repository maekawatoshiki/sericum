use cilk::{
    cilk_ir,
    codegen::x64::{asm::print::MachineAsmPrinter, dag, machine},
    ir::{builder, types, value},
    module::Module,
    *, // for macro
};
use std::{
    fs,
    io::{BufWriter, Write},
    process,
};
use {rand, rand::Rng};

fn unique_file_name(extension: &str) -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            0123456789";
    const LEN: usize = 16;
    let mut rng = rand::thread_rng();
    let name: String = (0..LEN)
        .map(|_| {
            let idx = rng.gen_range(0, CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();
    format!("/tmp/{}.{}", name, extension)
}

fn assemble_and_run(c_parent: &str, s_target: &str) {
    let parent_name = unique_file_name("c");
    let target_name = unique_file_name("s");
    {
        let mut parent = BufWriter::new(fs::File::create(parent_name.as_str()).unwrap());
        let mut target = BufWriter::new(fs::File::create(target_name.as_str()).unwrap());
        parent.write_all(c_parent.as_bytes()).unwrap();
        target.write_all(s_target.as_bytes()).unwrap();
    }

    let output_name = unique_file_name("out");
    let compilation = process::Command::new("gcc")
        .args(&[
            parent_name.as_str(),
            target_name.as_str(),
            "-o",
            output_name.as_str(),
        ])
        .status()
        .unwrap();
    assert!(compilation.success());

    let execution = process::Command::new(output_name.as_str())
        .status()
        .unwrap();
    assert!(execution.success());

    fs::remove_file(output_name).unwrap();
    fs::remove_file(parent_name).unwrap();
    fs::remove_file(target_name).unwrap();
}

fn compile_and_run(c_parent: &str, module: &Module) {
    // println!("{:?}", module);

    let mut dag_module = dag::convert::ConvertToDAG::new(&module).convert_module();
    dag::combine::Combine::new().combine_module(&mut dag_module);
    dag::legalize::Legalize::new().run_on_module(&mut dag_module);
    dag::isel::MISelector::new().run_on_module(&mut dag_module);

    let mut machine_module = dag::mc_convert::convert_module(dag_module);
    machine::phi_elimination::PhiElimination::new().run_on_module(&mut machine_module); //
    machine::two_addr::TwoAddressConverter::new().run_on_module(&mut machine_module);
    machine::regalloc::RegisterAllocator::new().run_on_module(&mut machine_module); //
    machine::pro_epi_inserter::PrologueEpilogueInserter::new().run_on_module(&mut machine_module);
    machine::replace_data::ConstDataReplacer::new().run_on_module(&mut machine_module);
    machine::replace_copy::ReplaceCopyWithProperMInst::new().run_on_module(&mut machine_module);
    // debug!(println!("{:?}", machine_module));

    let mut printer = MachineAsmPrinter::new();
    printer.run_on_module(&machine_module);
    // println!("{}", printer.output);

    assemble_and_run(c_parent, printer.output.as_str());
}

#[test]
fn asm_load_store() {
    let mut m = Module::new("cilk");
    cilk_ir!(m; define [i32] test [] {
        entry:
            a = alloca i32;
            store (i32 1), (%a);
            la = load (%a);
            ret (%la);
    });
    compile_and_run(
        "
    #include <assert.h>
    extern int test();
    int main() {
        assert(test() == 1);
    }
            ",
        &m,
    );
}

#[test]
fn asm_fibo_phi() {
    let mut m = Module::new("cilk");
    cilk_ir!(m; define [i32] test [(i32)] {
        entry:
            cond = icmp le (%arg.0), (i32 2);
            br (%cond) l1, l2;
        l1:
            br merge;
        l2:
            a1 = sub (%arg.0), (i32 1);
            r1 = call test [(%a1)];
            a2 = sub (%arg.0), (i32 2);
            r2 = call test [(%a2)];
            r3 = add (%r1), (%r2);
            br merge;
        merge:
            p = phi [ [(i32 1), l1], [(%r3), l2] ];
            ret (%p);
    });
    compile_and_run(
        "#include <assert.h>
        extern int test(int);
        int main() { assert(test(10) == 55); }",
        &m,
    );
}
