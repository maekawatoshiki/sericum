#[cfg(feature = "x86_64")]
mod x86_64 {
    use cilk::{
        cilk_ir,
        codegen::x64::{
            asm::{assembler::Assembler, print::MachineAsmPrinter},
            standard_conversion_into_machine_module,
        },
        ir::{builder, global_val, types, value},
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

    // fn assemble_and_run(c_parent: &str, s_target: &str) {
    //     let parent_name = unique_file_name("c");
    //     let target_name = unique_file_name("s");
    //     {
    //         let mut parent = BufWriter::new(fs::File::create(parent_name.as_str()).unwrap());
    //         let mut target = BufWriter::new(fs::File::create(target_name.as_str()).unwrap());
    //         parent.write_all(c_parent.as_bytes()).unwrap();
    //         target.write_all(s_target.as_bytes()).unwrap();
    //     }
    //
    //     let output_name = unique_file_name("out");
    //     let compilation = process::Command::new("clang")
    //         .args(&[
    //             parent_name.as_str(),
    //             target_name.as_str(),
    //             "-o",
    //             output_name.as_str(),
    //         ])
    //         .status()
    //         .unwrap();
    //     assert!(compilation.success());
    //
    //     let execution = process::Command::new(output_name.as_str())
    //         .status()
    //         .unwrap();
    //     assert!(execution.success());
    //
    //     fs::remove_file(output_name).unwrap();
    //     fs::remove_file(parent_name).unwrap();
    //     fs::remove_file(target_name).unwrap();
    // }

    fn compile(module: &mut Module) {
        let machine_module = standard_conversion_into_machine_module(module);
        let mut asmer = Assembler::new(&machine_module);
        // let mut printer = MachineAsmPrinter::new();
        // println!("{:?}", machine_module);
        // printer.run_on_module(&machine_module);
        // println!("{}", printer.output);
        // assemble_and_run(c_parent, &printer.output);
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
        compile(&mut m);
    }
}
