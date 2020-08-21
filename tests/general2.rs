#[cfg(feature = "x86_64")]
mod x86_64 {
    use cilk::{
        cilk_ir,
        codegen::x64::{asm::print::MachineAsmPrinter, standard_conversion_into_machine_module},
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
        let compilation = process::Command::new("clang")
            .args(&[
                parent_name.as_str(),
                target_name.as_str(),
                "-o",
                output_name.as_str(),
            ])
            .status()
            .unwrap();
        assert!(compilation.success());

        println!("{}", output_name);
        let execution = process::Command::new(output_name.as_str())
            .status()
            .unwrap();
        assert!(execution.success());

        // fs::remove_file(output_name).unwrap();
        // fs::remove_file(parent_name).unwrap();
        // fs::remove_file(target_name).unwrap();
    }

    fn compile_and_run(c_parent: &str, module: &mut Module) {
        let machine_module = standard_conversion_into_machine_module(module);
        let mut printer = MachineAsmPrinter::new();
        // println!("{:?}", machine_module);
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
        int test_ref(int x) {
            if (x <= 2) return 1;
            return test_ref(x - 1) + test_ref(x - 2);
        }
        int main() { 
            for (int i = 0; i < 30; i++)
                assert(test(i) == test_ref(i)); 
            return 0;
        }",
            &mut m,
        );
    }

    #[test]
    fn asm_global_var() {
        let mut m = Module::new("cilk");
        let ty = m.types.new_array_ty(types::Type::i32, 8);
        let g = m
            .global_vars
            .new_global_var_with_name(ty, global_val::Linkage::Common, "g");
        let g = value::Value::Global(value::GlobalValue {
            id: g,
            ty: m.types.new_pointer_ty(ty),
        });

        cilk_ir!(m; define [i32] test [] {
            entry:
                x = alloca i32;
                store (i32 1), (%x);
                lx = load (%x);
                p = gep (%g), [(i32 0), (%lx)];
                store (i32 123), (%p);
                i = load (%p);
                ret (%i);
        });

        println!("{:?}", m);

        compile_and_run(
            "#include <assert.h>
        extern int test();
        int main() { assert(test() == 123); }",
            &mut m,
        );
    }

    #[test]
    fn asm_global_var_struct() {
        let mut m = Module::new("cilk");
        let ty = m
            .types
            .new_struct_ty(vec![types::Type::i8, types::Type::i32]);
        let g = m
            .global_vars
            .new_global_var_with_name(ty, global_val::Linkage::Common, "g");
        let g = value::Value::Global(value::GlobalValue {
            id: g,
            ty: m.types.new_pointer_ty(ty),
        });

        cilk_ir!(m; define [i32] test [] {
            entry:
                p = gep (%g), [(i32 0), (i32 1)];
                store (i32 123), (%p);
                i = load (%p);
                ret (%i);
        });

        println!("{:?}", m);

        compile_and_run(
            "#include <assert.h>
        extern int test();
        int main() { assert(test() == 123); }",
            &mut m,
        );
    }

    #[test]
    fn asm_load_store_i8() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i8] test [(i8)] {
            entry:
                x = alloca i8;
                store (%arg.0), (%x);
                y = load (%x);
                ret (%y);
        });
        compile_and_run(
            "#include <assert.h>
        extern char test(char);
        char test_ref(char x) { char y = x; return y; }
        int main() { 
            for (int i = -128; i <= 127; i++) assert(test(i) == test_ref(i));
            return 0;
        }",
            &mut m,
        );
    }

    #[test]
    fn asm_array_load_i8() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i8] test [(ptr ptr i8), (i32)] {
            entry:
                a = gep (%arg.0), [(i32 0), (%arg.1)];
                b = load (%a);
                ret (%b);
        });
        compile_and_run(
            "#include <assert.h>
        extern char test(char*, int);
        int main() { 
            assert(test(\"hello\", 1) == 'e');
            return 0;
        }",
            &mut m,
        );
    }

    #[test]
    fn asm_arith_i8() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i8] test_add [(i8), (i8)] {
            entry:
                x = add (%arg.0), (%arg.1);
                ret (%x);
        });
        cilk_ir!(m; define [i8] test_sub [(i8), (i8)] {
            entry:
                x = sub (%arg.0), (%arg.1);
                ret (%x);
        });
        cilk_ir!(m; define [i8] test_mul [(i8), (i8)] {
            entry:
                x = mul (%arg.0), (%arg.1);
                ret (%x);
        });
        cilk_ir!(m; define [i8] test_div [(i8), (i8)] {
            entry:
                x = div (%arg.0), (%arg.1);
                ret (%x);
        });
        compile_and_run(
            "#include <assert.h>
            #include <stdio.h>
             #include <stdlib.h>
        extern char test_add(char, char);
        extern char test_sub(char, char);
        extern char test_mul(char, char);
        extern char test_div(char, char);
        char test_add_ref(char x, char y) { return x + y; }
        char test_sub_ref(char x, char y) { return x - y; }
        char test_mul_ref(char x, char y) { return x * y; }
        char test_div_ref(char x, char y) { return x / y; }
        int main() { 
            for (int i = 0; i < 256; i++) {
                char x = (char) rand(), y = (char) rand();
                assert(test_add(x, y) == test_add_ref(x, y));
                assert(test_sub(x, y) == test_sub_ref(x, y));
                assert(test_mul(x, y) == test_mul_ref(x, y));
                if (y != 0) assert(test_div(x, y) == test_div_ref(x, y));
            }
            return 0;
        }",
            &mut m,
        );
    }

    #[test]
    fn asm_digit2int() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [(i8)] {
            entry:
                c = icmp le (i32 48), (%arg.0); // '0'
                br (%c) zero, exception;
            zero:
                c = icmp le (%arg.0), (i32 57); // '9'
                br (%c) nine, exception;
            nine:
                x = sext [i32] (%arg.0);
                x = sub (%x), (i32 48);
                ret (%x);
            exception:
                ret (i32 -1);
        });
        compile_and_run(
            "#include <assert.h>
        extern int test(char);
        int test_ref(char x) {
            if ('0' <= x && x <= '9') return (int)x - (int)'0';
            return -1;
        }
        int main() { 
            char x; int i;
            for (x = -128, i = 0; i < 256; i++, x++) 
                assert(test(x) == test_ref(x));
        }",
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
            .stderr(::std::process::Stdio::null())
            .stdout(::std::process::Stdio::null())
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
    fn asm_pointer2() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test2 [(ptr i32)] {
            entry:
                store (i32 3), (%arg.0);
                ret (i32 0);
        });
        cilk_ir!(m; define [i32] test [] {
            entry:
                a = alloca_ (ptr i32);
                b = alloca i32;
                store (%b), (%a);
                __  = call test2 [(%b)];
                c = load (%a);
                d = load (%c);
                ret (%d);
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
    fn asm_array() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                a = alloca_ ([8; i32]);
                x = gep (%a), [(i32 0), (i32 1)];
                store (i32 3), (%x);
                y = load (%x);
                ret (%y);
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
    fn asm_array2() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [(i32)] {
            entry:
                a = alloca_ ([8; i32]);
                x = gep (%a), [(i32 0), (%arg.0)];
                store (i32 3), (%x);
                y = load (%x);
                ret (%y);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test(int);
    int main() {
        assert(test(1) == 3);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_array3() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] matrix_mul [(ptr [8; [8; i32]]), (ptr [8; [8; i32]]), (ptr [8; [8; i32]])] {
            entry:
                x = alloca i32;
                y = alloca i32;
                z = alloca i32;
                store (i32 0), (%x);
                br label1;
            label1:
                store (i32 0), (%y);
                xx = load (%x);
                c = icmp lt (%xx), (i32 8);
                br (%c) label2, label3;
            label2:
                store (i32 0), (%z);
                yy = load (%y);
                c = icmp lt (%yy), (i32 8);
                br (%c) label4, label5;
            label4:
                zz = load (%z);
                c = icmp lt (%zz), (i32 8);
                br (%c) label6, label7;
            label6:
                cx = gep (%arg.0), [(i32 0), (%xx), (%yy)];
                ax = gep (%arg.1), [(i32 0), (%xx), (%zz)];
                bx = gep (%arg.2), [(i32 0), (%zz), (%yy)];
                ai = load (%ax);
                bi = load (%bx);
                ci = load (%cx);
                t  = mul (%ai), (%bi);
                ci = add (%ci), (%t);
                store (%ci), (%cx);

                zi = add (%zz), (i32 1);
                store (%zi), (%z);
                br label4;
            label7:
                yi = add (%yy), (i32 1);
                store (%yi), (%y);
                br label2;
            label5:
                xi = add (%xx), (i32 1);
                store (%xi), (%x);
                br label1;
            label3:
                ret (%ci);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int matrix_mul(int *, int *, int *);
    int main() {
        int c[8][8] = {0};
        int a[8][8] = {
        {3, 3, 2, 9, 0, 8, 2, 6}, 
        {6, 9, 1, 1, 3, 5, 8, 3}, 
        {0, 6, 9, 2, 7, 7, 2, 8}, 
        {0, 3, 9, 2, 4, 9, 1, 7}, 
        {0, 4, 5, 0, 4, 0, 2, 4}, 
        {3, 1, 0, 6, 6, 1, 9, 7}, 
        {5, 1, 0, 4, 4, 5, 9, 0}, 
        {7, 6, 3, 4, 4, 0, 9, 0}};

        int b[8][8] = {
        {6, 2, 2, 3, 0, 9, 9, 5}, 
        {5, 9, 7, 9, 7, 4, 6, 9}, 
        {6, 4, 3, 5, 7, 6, 9, 4}, 
        {2, 5, 3, 8, 1, 0, 0, 7}, 
        {5, 6, 3, 1, 7, 2, 6, 8}, 
        {2, 1, 0, 0, 8, 1, 1, 4}, 
        {7, 4, 7, 5, 8, 1, 8, 7}, 
        {1, 7, 3, 9, 5, 0, 0, 3}};

        int d[8][8] = {
        { 99,  144,   92,  182,  154,   61,   87,  177}, 
        {173,  178,  155,  182,  211,  115,  204,  231}, 
        {159,  213,  134,  204,  268,  101,  182,  226}, 
        {125,  159,   94,  160,  229,   84,  140,  173}, 
        { 88,  116,   81,  111,  127,   56,  109,  114}, 
        {137,  167,  133,  180,  170,   53,  142,  202}, 
        {136,  104,  104,  105,  151,   71,  152,  177}, 
        {181,  160,  152,  171,  167,  122,  222,  224}};

        matrix_mul(&c, a, b);

        for (int i = 0 ; i < 8; i++) {
            for (int k = 0; k < 8; k++)
                assert(c[i][k] == d[i][k]);
        }
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

    #[test]
    fn asm_global_var() {
        let mut m = Module::new("cilk");
        let ty = types::Type::i32;
        let g = m
            .global_vars
            .new_global_var_with_name(ty, global_val::Linkage::Common, "g");
        let g = value::Value::Global(value::GlobalValue {
            id: g,
            ty: m.types.new_pointer_ty(ty),
        });

        cilk_ir!(m; define [i32] test [] {
            entry:
                store (i32 123), (%g);
                i = load (%g);
                ret (%i);
        });

        println!("{:?}", m);

        compile_and_run(
            "#include <assert.h>
        extern int test();
        int main() { assert(test() == 123); }",
            &mut m,
        );
    }

    #[test]
    fn asm_global_var_array() {
        let mut m = Module::new("cilk");
        let ty = m.types.new_array_ty(types::Type::i32, 8);
        let g = m
            .global_vars
            .new_global_var_with_name(ty, global_val::Linkage::Common, "g");
        let g = value::Value::Global(value::GlobalValue {
            id: g,
            ty: m.types.new_pointer_ty(ty),
        });

        cilk_ir!(m; define [i32] test [] {
            entry:
                i = alloca i32;
                store (i32 2), (%i);
                li = load (%i);
                x = gep (%g), [(i32 0), (%li)];
                store (i32 123), (%x);
                i = load (%x);
                ret (%i);
        });

        println!("{:?}", m);

        compile_and_run(
            "#include <assert.h>
        extern int test();
        int main() { assert(test() == 123); }",
            &mut m,
        );
    }
}

#[cfg(feature = "aarch64")]
mod aarch64 {
    use cilk::{
        cilk_ir,
        codegen::aarch64::{
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
        let compilation = process::Command::new("aarch64-linux-gnu-gcc")
            .args(&[
                parent_name.as_str(),
                target_name.as_str(),
                "-static",
                "-o",
                output_name.as_str(),
            ])
            .stderr(::std::process::Stdio::null())
            .stdout(::std::process::Stdio::null())
            .status()
            .unwrap();
        assert!(compilation.success());

        let execution = process::Command::new("qemu-aarch64")
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
                ii = load (%i);
                ret (%ii);
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
    fn asm_arg() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [(i32), (i32)] {
            entry:
                // i = alloca i32;
                // store (i32 42), (%i);
                // ii = load (%i);
                ret (%arg.1);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test(int,int);
    int main() {
        assert(test(1, 42) == 42);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_add_sub() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test_add [(i32)] {
            entry:
                i = alloca i32;
                store (i32 40), (%i);
                ii = load (%i);
                x = add (%ii), (i32 2);
                x = add (%arg.0), (%x);
                ret (%x);
        });
        cilk_ir!(m; define [i32] test_sub [(i32)] {
            entry:
                i = alloca i32;
                store (i32 50), (%i);
                ii = load (%i);
                x = sub (%ii), (i32 8);
                x = sub (%x), (%arg.0);
                ret (%x);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test_add(int);
    extern int test_sub();
    int main() {
        assert(test_add(2) == 44);
        assert(test_sub(3) == 39);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_mul_div() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test_mul [(i32)] {
            entry:
                i = alloca i32;
                store (i32 21), (%i);
                ii = load (%i);
                x = mul (%ii), (i32 2);
                x = mul (%x), (%arg.0);
                ret (%x);
        });
        cilk_ir!(m; define [i32] test_div [(i32)] {
            entry:
                i = alloca i32;
                store (i32 85), (%i);
                ii = load (%i);
                x = div (%ii), (i32 2);
                x = div (%x), (%arg.0);
                ret (%x);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test_mul(int);
    extern int test_div();
    int main() {
        assert(test_mul(2) == 84);
        assert(test_div(3) == 14);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_brcc() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [(i32)] {
            entry:
                c = icmp eq (%arg.0), (i32 0);
                br (%c) l1, l2;
            l1:
                ret (i32 1);
            l2:
                ret (i32 0);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int test(int);
    int main() {
        assert(test(1) == 0);
        assert(test(0) == 1);
    }
            ",
            &mut m,
        );
    }

    #[test]
    fn asm_pointer1() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                x = alloca_ (ptr i32);
                y = alloca i32;
                store (%y), (%x);
                xx = load (%x);
                store (i32 42), (%xx);
                yy = load (%y);
                ret (%yy);
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
    fn asm_array1() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                x = alloca_ ([8; i32]);
                // y = alloca i32;
                // store (%y), (%x);
                // xx = load (%x);
                // store (i32 42), (%xx);
                // yy = load (%y);
                a = gep (%x), [(i32 0), (i32 1)];
                store (i32 42), (%a);
                a = load (%a);
                ret (%a);
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
    fn asm_array2() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] test [] {
            entry:
                x = alloca_ ([8; i32]);
                y = alloca i32;
                store (i32 2), (%y);
                yy = load (%y);
                a = gep (%x), [(i32 0), (%yy)];
                store (i32 42), (%a);
                a = load (%a);
                ret (%a);
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
    fn asm_array3() {
        let mut m = Module::new("cilk");
        cilk_ir!(m; define [i32] matrix_mul [(ptr [8; [8; i32]]), (ptr [8; [8; i32]]), (ptr [8; [8; i32]])] {
            entry:
                x = alloca i32;
                y = alloca i32;
                z = alloca i32;
                store (i32 0), (%x);
                br label1;
            label1:
                store (i32 0), (%y);
                xx = load (%x);
                c = icmp lt (%xx), (i32 8);
                br (%c) label2, label3;
            label2:
                store (i32 0), (%z);
                yy = load (%y);
                c = icmp lt (%yy), (i32 8);
                br (%c) label4, label5;
            label4:
                zz = load (%z);
                c = icmp lt (%zz), (i32 8);
                br (%c) label6, label7;
            label6:
                cx = gep (%arg.0), [(i32 0), (%xx), (%yy)];
                ax = gep (%arg.1), [(i32 0), (%xx), (%zz)];
                bx = gep (%arg.2), [(i32 0), (%zz), (%yy)];
                ai = load (%ax);
                bi = load (%bx);
                ci = load (%cx);
                t  = mul (%ai), (%bi);
                ci = add (%ci), (%t);
                store (%ci), (%cx);

                zi = add (%zz), (i32 1);
                store (%zi), (%z);
                br label4;
            label7:
                yi = add (%yy), (i32 1);
                store (%yi), (%y);
                br label2;
            label5:
                xi = add (%xx), (i32 1);
                store (%xi), (%x);
                br label1;
            label3:
                ret (%ci);
        });
        compile_and_run(
            "
    #include <assert.h>
    extern int matrix_mul(int *, int *, int *);
    int main() {
        int c[8][8] = {0};
        int a[8][8] = {
        {3, 3, 2, 9, 0, 8, 2, 6}, 
        {6, 9, 1, 1, 3, 5, 8, 3}, 
        {0, 6, 9, 2, 7, 7, 2, 8}, 
        {0, 3, 9, 2, 4, 9, 1, 7}, 
        {0, 4, 5, 0, 4, 0, 2, 4}, 
        {3, 1, 0, 6, 6, 1, 9, 7}, 
        {5, 1, 0, 4, 4, 5, 9, 0}, 
        {7, 6, 3, 4, 4, 0, 9, 0}};

        int b[8][8] = {
        {6, 2, 2, 3, 0, 9, 9, 5}, 
        {5, 9, 7, 9, 7, 4, 6, 9}, 
        {6, 4, 3, 5, 7, 6, 9, 4}, 
        {2, 5, 3, 8, 1, 0, 0, 7}, 
        {5, 6, 3, 1, 7, 2, 6, 8}, 
        {2, 1, 0, 0, 8, 1, 1, 4}, 
        {7, 4, 7, 5, 8, 1, 8, 7}, 
        {1, 7, 3, 9, 5, 0, 0, 3}};

        int d[8][8] = {
        { 99,  144,   92,  182,  154,   61,   87,  177}, 
        {173,  178,  155,  182,  211,  115,  204,  231}, 
        {159,  213,  134,  204,  268,  101,  182,  226}, 
        {125,  159,   94,  160,  229,   84,  140,  173}, 
        { 88,  116,   81,  111,  127,   56,  109,  114}, 
        {137,  167,  133,  180,  170,   53,  142,  202}, 
        {136,  104,  104,  105,  151,   71,  152,  177}, 
        {181,  160,  152,  171,  167,  122,  222,  224}};

        matrix_mul(&c, a, b);

        for (int i = 0 ; i < 8; i++) {
            for (int k = 0; k < 8; k++)
                assert(c[i][k] == d[i][k]);
        }
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
