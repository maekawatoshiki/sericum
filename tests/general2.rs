#[cfg(feature = "x86_64")]
mod x86_64 {
    use cilk::{
        cilk_ir,
        codegen::x64::{asm::print::MachineAsmPrinter, standard_conversion_into_machine_module},
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

    fn compile_and_run(c_parent: &str, module: &mut Module) {
        let machine_module = standard_conversion_into_machine_module(module);
        let mut printer = MachineAsmPrinter::new();
        println!("{:?}", machine_module);
        printer.run_on_module(&machine_module);
        println!("{}", printer.output);
        assemble_and_run(c_parent, &printer.output);
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
            &mut m,
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
            &mut m,
        );
    }
}

#[cfg(feature = "riscv64")]
mod riscv64 {
    use cilk::{
        cilk_ir,
        codegen::riscv64::{
            asm::print::MachineAsmPrinter, standard_conversion_into_machine_module,
        },
        ir::builder::FuncRef,
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
        let compilation = process::Command::new("riscv64-unknown-elf-gcc")
            .args(&[
                parent_name.as_str(),
                target_name.as_str(),
                "-o",
                output_name.as_str(),
            ])
            .status()
            .unwrap();
        assert!(compilation.success());

        let execution = process::Command::new("qemu-riscv64")
            .arg(output_name.as_str())
            .status()
            .unwrap();
        assert!(execution.success());

        fs::remove_file(output_name).unwrap();
        fs::remove_file(parent_name).unwrap();
        fs::remove_file(target_name).unwrap();
    }

    fn compile_and_run(c_parent: &str, module: &mut Module) {
        let machine_module = standard_conversion_into_machine_module(module);
        let mut printer = MachineAsmPrinter::new();
        println!("{:?}", machine_module);
        printer.run_on_module(&machine_module);
        println!("{}", printer.output);
        assemble_and_run(c_parent, &printer.output);
    }

    #[test]
    fn asm_minimum() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                ret (i32 42);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    int main() {
        assert(test() == 42);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_local_var() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                i = alloca i32;
                store (i32 42), (%i);
                li = load (%i);
                ret (%li);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    int main() {
        assert(test() == 42);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_add() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                i = alloca i32;
                store (i32 20), (%i);
                li = load (%i);
                a = add (%li), (i32 1);
                a = add (%a), (%a);
                ret (%a);
        });
        cilk_ir!(m; define [i32] big [] {
            entry:
                i = alloca i32;
                store (i32 23428042), (%i);
                li = load (%i);
                a = add (%li), (i32 1366270073);
                ret (%a);
        });
        cilk_ir!(m; define [i32] big2 [] {
            entry:
                i = alloca i32;
                store (i32 0x7fffffff), (%i);
                li = load (%i);
                a = add (i32 1), (%li);
                ret (%a);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    extern int big();
    extern int big2();
    int main() {
        assert(test() == 42);
        assert(big() == 1389698115);
        assert(big2() == -2147483648);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_sub() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                i = alloca i32;
                store (i32 20), (%i);
                li = load (%i);
                a = sub (%li), (i32 5);
                ret (%a);
        });
        cilk_ir!(m; define [i32] small [] {
            entry:
                i = alloca i32;
                store (i32 -23428042), (%i);
                li = load (%i);
                a = sub (%li), (i32 1366270073);
                ret (%a);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    extern int small();
    int main() {
        assert(test() == 15);
        assert(small() == -1389698115);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_mul_div_rem() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test_mul [] {
            entry:
                i = alloca i32;
                store (i32 3), (%i);
                li = load (%i);
                a = mul (%li), (i32 5);
                a = mul (%a), (%a);
                ret (%a);
        });
        cilk_ir!(m; define [i32] test_div [] {
            entry:
                i = alloca i32;
                store (i32 20), (%i);
                li = load (%i);
                a = div (%li), (i32 2); // 10
                a = div (%li), (%a);
                ret (%a);
        });
        cilk_ir!(m; define [i32] test_rem [] {
            entry:
                i = alloca i32;
                store (i32 10), (%i);
                li = load (%i);
                a = rem (%li), (i32 7); // 3
                a = div (%li), (%a);
                ret (%a);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test_mul();
    extern int test_div();
    extern int test_rem();
    int main() {
        assert(test_mul() == 225);
        assert(test_div() == 2);
        assert(test_rem() == 3);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_jmp() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                br l2;
            l1:
                ret (i32 1);
            l2:
                ret (i32 2);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    int main() {
        assert(test() == 2);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_jmp_eq() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [(i32)] {
            entry:
                c = icmp eq (%arg.0), (i32 2);
                br (%c) l1, l2;
            l1:
                ret (i32 1);
            l2:
                ret (i32 2);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test(int);
    int main() {
        assert(test(2) == 1);
        assert(test(3) == 2);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_pointer() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                a = alloca_ (ptr i32);
                b = alloca i32;
                store (%b), (%a);
                store (i32 3), (%b);
                c = load (%a);
                d = load (%c);
                ret (%d);
                // ret (i32 0);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test();
    int main() {
        assert(test() == 3);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_fact() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] fact [(i32)] {
            entry:
                c = icmp eq (%arg.0), (i32 1);
                br (%c) l1, l2;
            l1:
                ret (i32 1);
            l2:
                x = sub (%arg.0), (i32 1);
                y = call fact [(%x)];
                z = mul (%y), (%arg.0);
                ret (%z);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int fact(int);
    int main() {
        assert(fact(5) == 120);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_prime() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] prime [(i32)] {
            // primarity test
            entry:
                i = alloca i32;
                cond = icmp eq (%arg.0), (i32 2);
                br (%cond) l1, l2;
            l1:
                ret (i32 1);
            l2:
                r = rem (%arg.0), (i32 2);
                cond = icmp eq (%r), (i32 0);
                br (%cond) l3, l4;
            l3:
                ret (i32 0);
            l4:
                store (i32 3), (%i);
                br l5;
            l5:
                li = load (%i);
                m = mul (%li), (%li);
                cond = icmp le (%m), (%arg.0);
                br (%cond) l6, l7;
            l6:
                li = load (%i);
                r = rem (%arg.0), (%li);
                cond = icmp eq (%r), (i32 0);
                br (%cond) l8, l9;
            l8:
                ret (i32 0);
            l9:
                a = add (%li), (i32 2);
                store (%a), (%i);
                br l5;
            l7:
                ret (i32 1);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int prime(int);
    int main() {
        int p[] = {2,3,5,7,11,13,17,19,23,29,31,37,
          41,43,47,53,59,61,67,71,73,79,83,89,97}, *q=p;
        for (int i = 2; i < 100; i++)
            if (prime(i)) assert(i == *(q++));
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_fibo() {
        let mut m = module::Module::new("cilk");
        cilk_ir!(m; define [i32] fibo [(i32)] {
            entry:
                cond = icmp le (%arg.0), (i32 2);
                br (%cond) l1, l2;
            l1:
                ret (i32 1);
            l2:
                a1 = sub (%arg.0), (i32 1);
                r1 = call fibo [(%a1)];
                a2 = sub (%arg.0), (i32 2);
                r2 = call fibo [(%a2)];
                r3 = add (%r1), (%r2);
                ret (%r3);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int fibo(int);
    int main() {
        assert(fibo(10) == 55);
    }
            ",
            &mut m,
        );
    }
}
