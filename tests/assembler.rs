#[cfg(feature = "x86_64")]
mod x86_64 {
    use cilk::{
        cilk_ir,
        codegen::x64::{
            asm::assembler::Assembler, exec::executor::Executor,
            standard_conversion_into_machine_module,
        },
        ir::{builder, /*global_val,*/ types, value},
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

    fn compile(parent_in_c: &str, module: &mut Module) {
        let parent_name = unique_file_name("c");
        {
            let mut parent = BufWriter::new(fs::File::create(parent_name.as_str()).unwrap());
            parent.write_all(parent_in_c.as_bytes()).unwrap();
        }

        let machine_module = standard_conversion_into_machine_module(module);
        println!("{:?}", machine_module);

        let mut asmer = Assembler::new(&machine_module);
        asmer.assemble();

        let obj_name = unique_file_name(".o");
        asmer.write_to_file(&obj_name);

        let output_name = unique_file_name("out");
        let compilation = process::Command::new("clang")
            .args(&[
                obj_name.as_str(),
                parent_name.as_str(),
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
    }

    #[test]
    fn jit_asm() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] add [(i32)] {
            entry: x = add (%arg.0), (i32 2);
                   ret (%x);
        });

        cilk_ir!(m; define [i32] main [(i32)] {
            entry: x = call add [(%arg.0)];
                   ret (%x);
        });

        let machine_module = standard_conversion_into_machine_module(&mut m);
        // println!("{:?}", machine_module);

        let mut exec = Executor::new(machine_module);
        exec.compile();
        exec.execute();
    }

    #[test]
    fn asmer_minimum() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                ret (i32 42);
        });
        compile(
            "#include <assert.h>
                 extern int test(); 
                 int main() { assert(test() == 42); return 0; }",
            &mut m,
        );
    }

    #[test]
    fn asmer_local() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                i = alloca i32;
                store (i32 42), (%i);
                li = load (%i);
                ret (%li);
        });
        compile(
            "#include <assert.h>
                 extern int test(); 
                 int main() { assert(test() == 42); return 0; }",
            &mut m,
        );
    }

    #[test]
    fn asmer_arith_ri() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] add [(i32)] {
            entry: x = add (%arg.0), (i32 2);
                   ret (%x); });
        cilk_ir!(m; define [i32] sub [(i32)] {
            entry: x = sub (%arg.0), (i32 2);
                   ret (%x); });
        cilk_ir!(m; define [i32] mul [(i32)] {
            entry: x = mul (%arg.0), (i32 3);
                   ret (%x); });
        compile(
            "#include <assert.h>
                 extern int add(int); 
                 extern int sub(int); 
                 extern int mul(int); 
                 int main() { 
                     assert(add(2) == 4); 
                     assert(sub(4) == 2); 
                     assert(mul(2) == 6);
                     return 0; 
                 }",
            &mut m,
        );
    }
}
