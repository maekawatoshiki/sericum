#[cfg(feature = "x86_64")]
mod x86_64 {
    use cilk::{
        codegen::x64::exec,
        ir::builder::FuncRef,
        // exec::{interpreter::interp, jit::x64::compiler},
        ir::{builder, opcode, types, value},
        *,
    };

    #[test]
    fn test0_mem2reg() {
        let mut m = module::Module::new("cilk");

        let func = cilk_ir!(m; define [i32] func [] {
        entry:
            i = alloca i32;
            store (i32 3), (%i);
            li = load (%i);
            // tmp = add (%li), (i32 2);
            // store (%tmp), (%i);
            // li = load (%i);
            ret (%li);
        });

        println!("{}", m.dump(func));

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(3));
    }

    #[test]
    fn test1_mem2reg() {
        let mut m = module::Module::new("cilk");

        let func = cilk_ir!(m; define [i32] func [] {
        entry:
            i = alloca i32;
            k = alloca i32;
            br label;
        label:
            store (i32 3), (%i);
            li = load (%i);
            tmp = add (%li), (i32 2);
            store (%tmp), (%k);
            lk = load (%k);
            tmp = add (%li), (%lk);
            store (%tmp), (%i);
            li = load (%i);
            ret (%li);
        });

        println!("{}", m.dump(func));

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(8));
    }

    #[test]
    fn test2_mem2reg() {
        let mut m = module::Module::new("cilk");

        let func = cilk_ir!(m; define [i32] func [] {
        entry:
            i = alloca i32;
            br label1;
        label1:
            store (i32 3), (%i);
            br label3;
        label3:
            br label2;
        label2:
            li = load (%i);
            ret (%li);
        });

        println!("{}", m.dump(func));

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(3));
    }

    #[test]
    fn test3_mem2reg() {
        let mut m = module::Module::new("cilk");

        let func = cilk_ir!(m; define [i32] func [(i32)] {
        entry:
            i = alloca i32;
            store (i32 0), (%i);
            br label1;
        label1:
            li = load (%i);
            cond = icmp le (%li), (%arg.0);
            br (%cond) label2, label3;
        label2:
            a = add (%li), (i32 1);
            store (%a), (%i);
            br label1;
        label3:
            ret (%li);
        });

        println!("{}", m.dump(func));

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(10)]),
            exec::jit::GenericValue::Int32(11)
        );
    }

    #[test]
    fn test4_mem2reg() {
        let mut m = module::Module::new("cilk");

        let func = cilk_ir!(m; define [i32] func [(i32)] {
        entry:
            i = alloca i32;
            k = alloca i32;
            store (i32 1), (%i);
            br label1;
        label1:
            li = load (%i);
            cond = icmp le (%li), (%arg.0);
            br (%cond) label2, label3;
        label2:
            store (i32 3), (%i);
            store (i32 6), (%k);
            br label4;
        label3:
            store (i32 4), (%k);
            br label4;
        label4:
            li = load (%i);
            lk = load (%k);
            a = add (%li), (%lk);
            ret (%a);
        });

        println!("{}", m.dump(func));

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(2)]),
            exec::jit::GenericValue::Int32(9)
        );
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(0)]),
            exec::jit::GenericValue::Int32(5)
        );
    }

    #[test]
    fn pointer() {
        let mut m = module::Module::new("cilk");

        let ptr_i32_ty = m.types.new_pointer_ty(types::Type::i32);
        let cilk_memset_i32 = m.create_function(
            "cilk.memset.p0i32.i32",
            types::Type::Void,
            vec![ptr_i32_ty, types::Type::i32, types::Type::i32],
        );

        let func = cilk_ir!(m; define [i32] func [] {
        entry:
            arr = alloca_ ([16; i32]);

            __ = call (->cilk_memset_i32) [(%arr), (i32 0), (i32 16)];

            p = gep (%arr), [(i32 0), (i32 15)];
            v = load (%p);

            ret (%v);
        });

        println!("{}", m.dump(func));

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(0));
    }

    #[test]
    fn pointer2() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [i32] main [] {
        entry:
            a = alloca i32;
            pa = alloca_ (ptr i32);
            store (%a), (%pa);
            lpa = load (%pa);
            store (i32 123), (%lpa);
            la = load (%a);
            ret (%la);
        });

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(123));
    }

    #[test]
    fn pointer3() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [void] func [(ptr i32)] {
        entry:
            store (i32 123), (%arg.0);
            ret (void);
        });

        cilk_ir!(m; define [i32] main [] {
        entry:
            a = alloca i32;
            pa = alloca_ (ptr i32);
            store (%a), (%pa);
            __ = call func [(%a)];
            la = load (%a);
            ret (%la);
        });

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(123));
    }

    #[test]
    fn phi() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [i32] func [(i32)] {
            entry:
                cond = icmp le (%arg.0), (i32 2);
                br (%cond) l1, l2;
            l1:
                br merge;
            l2:
                a1 = sub (%arg.0), (i32 1);
                r1 = call func [(%a1)];
                a2 = sub (%arg.0), (i32 2);
                r2 = call func [(%a2)];
                r3 = add (%r1), (%r2);
                br merge;
            merge:
                p = phi [ [(i32 1), l1], [(%r3), l2] ];
                ret (%p);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let ret = jit.run(func, vec![exec::jit::GenericValue::Int32(7)]);
        assert_eq!(ret, exec::jit::GenericValue::Int32(13));
        let ret = jit.run(func, vec![exec::jit::GenericValue::Int32(10)]);
        assert_eq!(ret, exec::jit::GenericValue::Int32(55));
    }

    #[test]
    fn arr_2d() {
        let mut m = module::Module::new("cilk");

        // Internal function must be defined before you use it

        let _ = cilk_ir!(m; define [i32] func [] {
        // for (int i = 0; i < 2; i++)
        //   for (int k = 0; k < 2; k++)
        //     a[i][k] = i + k;

        entry:
            a = alloca_ ([2; [2; i32]]);
            v1 = alloca i32;
            v2 = alloca i32;
            store (i32 1), (%v1);
            store (i32 1), (%v2);
            v1 = load (%v1);
            v2 = load (%v2);
            x = gep (%a), [(i32 0), (%v1), (%v2)];
            store (i32 31), (%x);
            x2 = load (%x);
            ret (%x2);

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
        //     x = add (%li), (%lk);
        //     store (%x), (%g);
        //     x = add (%lk), (i32 1);
        //     store (%x), (%k);
        //     br l2;
        // l5:
        //     store (i32 0), (%k);
        //     x = add (%li), (i32 1);
        //     store (%x), (%i);
        //     br l1;
        // l3:
        //     r = gep (%a), [(i32 0), (i32 1), (i32 1)];
        //     r = load (%r);
        //     ret (%r);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let ret = jit.run(func, vec![]);
        println!("return: {:?}", ret);
        assert_eq!(ret, exec::jit::GenericValue::Int32(31));
    }

    #[test]
    fn jit_executor1() {
        let mut m = module::Module::new("cilk");

        // Internal function must be defined before you use it
        let cilk_println_i32 = m.create_function(
            "cilk.println.i32",
            ir::types::Type::Void,
            vec![ir::types::Type::i32],
        );

        let func = cilk_ir!(m; define [i32] func [(i32)] {
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

        let _main = cilk_ir!(m; define [void] main [(i32)] {
            entry:
                i = alloca i32;
                store (i32 2), (%i);
                br cond;
            cond:
                li = load (%i);
                c = icmp le (%li), (%arg.0);
                br (%c) loop_, end;
            loop_:
                x = call (->func) [(%li)];
                c = icmp eq (%x), (i32 1);
                br (%c) p, not_p;
            p:
                __ = call (->cilk_println_i32) [(%li)];
                br not_p;
            not_p:
                inc = add (%li), (i32 1);
                store (%inc), (%i);
                br cond;
            end:
                ret (void);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let main = jit.find_function_by_name("main").unwrap();
        println!(
            "main: return: {:?}",
            jit.run(main, vec![exec::jit::GenericValue::Int32(40)])
        );
    }

    #[test]
    fn jit_executor2() {
        let mut m = module::Module::new("cilk");

        // Internal function must be defined before you use it
        let cilk_println_i32 = m.create_function(
            "cilk.println.i32",
            ir::types::Type::Void,
            vec![ir::types::Type::i32],
        );

        let func = cilk_ir!(m; define [i32] func [(i32)] {
            entry:
                cond = icmp le (%arg.0), (i32 2);
                br (%cond) l1, l2;
            l1:
                br merge;
                // ret (i32 1);
            l2:
                a1 = sub (%arg.0), (i32 1);
                r1 = call func [(%a1)];
                a2 = sub (%arg.0), (i32 2);
                r2 = call func [(%a2)];
                r3 = add (%r1), (%r2);
                // ret (%r3);
                br merge;
            merge:
                p = phi [ [(i32 1), l1], [(%r3), l2] ];
                ret (%p);
        });

        let _main = cilk_ir!(m; define [void] main [(i32)] {
            entry:
                i = alloca i32;
                store (i32 1), (%i);
                br cond;
            cond:
                li = load (%i);
                c = icmp le (%li), (%arg.0);
                br (%c) loop_, end;
            loop_:
                x = call (->func) [(%li)];
                __ = call (->cilk_println_i32) [(%x)];
                inc = add (%li), (i32 1);
                store (%inc), (%i);
                br cond;
            end:
                ret (void);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let main = jit.find_function_by_name("main").unwrap();
        println!(
            "main: return: {:?}",
            jit.run(main, vec![exec::jit::GenericValue::Int32(39)])
        );
    }

    #[test]
    fn fibo() {
        use cilk::codegen::x64::standard_conversion_into_machine_module;

        let mut m = module::Module::new("cilk");

        let _fibo = cilk_ir!(m; define [i32] fibo [(i32)] {
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

        let _main = cilk_ir!(m; define [i32] main [] {
            entry:
                r = call fibo [(i32 10)];
                ret (%r);
        });

        let machine_module = standard_conversion_into_machine_module(&mut m);
        // println!("{:?}", machine_module);
        use cilk::codegen::x64::asm::print::MachineAsmPrinter;
        let mut printer = MachineAsmPrinter::new();
        printer.run_on_module(&machine_module);
        println!("ASM DUMP: \n{}", printer.output);

        assert_eq!(
            "  .text
  .intel_syntax noprefix
  .globl fibo
fibo:
.L0:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov eax, edi
  cmp eax, 2
  jg .L2
.L1:
  mov eax, 1
  jmp .L3
.L2:
  mov edi, eax
  sub edi, 1
  mov dword ptr [rbp-4], eax
  call fibo
  mov edi, dword ptr [rbp-4]
  sub edi, 2
  mov dword ptr [rbp-4], eax
  call fibo
  mov ecx, dword ptr [rbp-4]
  add ecx, eax
  mov eax, ecx
.L3:
  add rsp, 16
  pop rbp
  ret 
  .globl main
main:
.L4:
  push rbp
  mov edi, 10
  call fibo
  pop rbp
  ret 
",
            printer.output
        );
    }

    #[test]
    fn spill() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [i32] func [(i32)] {
            entry:
                x1  = add (%arg.0), (i32 1);  // 2
                x2  = add (%arg.0), (i32 2);  // 3
                x3  = add (%arg.0), (i32 3);  // 4
                x4  = add (%arg.0), (i32 4);  // 5
                x5  = add (%arg.0), (i32 5);  // 6
                x6  = add (%arg.0), (i32 6);  // 7
                x7  = add (%arg.0), (i32 7);  // 8
                x8  = add (%arg.0), (i32 8);  // 9
                x9  = add (%arg.0), (i32 9);  // 10
                x10 = add (%arg.0), (i32 10); // 11
                x11 = add (%arg.0), (i32 11); // 12
                x12 = add (%arg.0), (i32 12); // 13

                y1  = add (%x1), (%x2);
                y2  = add (%y1), (%x3);
                y3  = add (%y2), (%x4);
                y4  = add (%y3), (%x5);
                y5  = add (%y4), (%x6);
                y6  = add (%y5), (%x7);
                y7  = add (%y6), (%x8);
                y8  = add (%y7), (%x9);
                y9  = add (%y8), (%x10);
                y10 = add (%y9), (%x11);
                y11 = add (%y10), (%x12);
                y11 = add (%y11), (%y2);
                y11 = add (%y11), (%y3);
                y11 = add (%y11), (%y4);
                y11 = add (%y11), (%y5);
                y11 = add (%y11), (%y6);
                y11 = add (%y11), (%y7);
                y11 = add (%y11), (%y8);
                y11 = add (%y11), (%y9);
                y11 = add (%y11), (%y10);
                ret (%y11);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let res = jit.run(func, vec![exec::jit::GenericValue::Int32(1)]);
        println!("return: {:?}", res);
        assert_eq!(res, exec::jit::GenericValue::Int32(435));
    }

    #[test]
    fn struct1() {
        let mut m = module::Module::new("cilk");

        let f = m.create_function("f", types::Type::i32, vec![]);

        let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));

        let entry = builder.append_basic_block();
        builder.set_insert_point(entry);

        let ary_ty = builder.func.module.types.new_array_ty(types::Type::i32, 16);
        let struct_ty = builder
            .func
            .module
            .types
            .new_struct_ty(vec![ary_ty, types::Type::i32]);
        let var = builder.build_alloca(struct_ty);

        cilk_ir!((builder) {
            x = gep (%var), [(i32 0), (i32 1)];
            store (i32 3), (%x);
            load_x = load (%x);
            ret (%load_x);
        });

        println!("{}", m.dump(f));

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("f").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::Int32(3));
    }

    #[test]
    fn struct2() {
        let mut m = module::Module::new("cilk");

        let struct_ty = m
            .types
            .new_struct_ty(vec![types::Type::i32, types::Type::i32]);
        let ptr_struct_ty = m.types.new_pointer_ty(struct_ty);

        let f = m.create_function("f", types::Type::Void, vec![ptr_struct_ty]);

        let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));

        let entry = builder.append_basic_block();
        builder.set_insert_point(entry);

        cilk_ir!((builder) {
            x = gep (%arg.0), [(i32 0), (i32 1)];
            store (i32 123), (%x);
            ret (void);
        });

        let main = m.create_function("main", types::Type::i32, vec![]);

        let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, main));

        let entry = builder.append_basic_block();
        builder.set_insert_point(entry);

        let var = builder.build_alloca(struct_ty);

        cilk_ir!((builder) {
            __ = call f [(%var)];
            x = gep (%var), [(i32 0), (i32 1)];
            r = load (%x);
            ret (%r);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::Int32(123));
    }

    #[test]
    fn many_arguments() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [i32] func [
                (i32), (i32), (i32), (i32), (i32), (i32), (i32), (i32), (i32) ] {
            entry:
                x = add (%arg.0), (%arg.1);//1
                x = add (%x), (%arg.2);//3
                x = add (%x), (%arg.3);//6
                x = add (%x), (%arg.4);//10
                x = add (%x), (%arg.5);//15
                x = add (%x), (%arg.6);//21
                x = add (%x), (%arg.7);//28
                x = add (%x), (%arg.8);//36
                ret (%x);
        });

        let _ = cilk_ir!(m; define [i32] main [] {
            entry:
                x = call func [(i32 0),(i32 1),(i32 2),(i32 3),(i32 4),(i32 5),(i32 6),(i32 7),(i32 8)];
                ret (%x);
        });

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        let res = jit.run(func, vec![]);
        println!("{:?}", res);
        assert_eq!(res, exec::jit::GenericValue::Int32(36));
    }

    #[test]
    fn fact() {
        let mut m = module::Module::new("cilk");

        /*
         * int fact(int x) {
         *   if (x == 1) return 1;
         *   return x * fact(x - 1);
         * }
         */
        let _ = cilk_ir!(m; define [i32] fact [(i32)] {
            entry:
                cond = icmp eq (%arg.0), (i32 1);
                br (%cond) l1, l2;
            l1:
                ret (i32 1);
            l2:
                a = sub (%arg.0), (i32 1);
                y = call fact [(%a)];
                z = mul (%y), (%arg.0);
                ret (%z);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("fact").unwrap();
        let res = jit.run(func, vec![exec::jit::GenericValue::Int32(10)]);
        println!("{:?}", res);
        assert_eq!(res, exec::jit::GenericValue::Int32(3628800));
    }

    #[test]
    fn float2() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [f64] func [] {
            entry:
                a = alloca f64;
                store (f64 1.23), (%a);
                la = load (%a);
                ret (%la);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::F64(1.23));
    }

    #[test]
    fn float3() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [f64] func [] {
            entry:
                a = alloca f64;
                store (f64 1.23), (%a);
                la = load (%a);
                b = add (%la), (f64 2.34);
                c = sub (%b), (f64 0.34);
                d = add (%b), (%c);
                e = sub (%d), (%la);
                ret (%e);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::F64(5.57));
    }

    #[test]
    fn float4() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [f64] func [] {
            entry:
                a = alloca f64;
                store (f64 1.2), (%a);
                la = load (%a);
                b = mul (%la), (f64 5.0);
                c = mul (%b), (%la);
                d = div (%c), (f64 2.5);
                e = div (%d), (f64 1.2);
                ret (%e);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::F64(2.4));
    }

    #[test]
    fn float5() {
        let mut m = module::Module::new("cilk");

        let _ = cilk_ir!(m; define [i32] func [] {
            entry:
                a = alloca f64;
                store (f64 2.14), (%a);
                la = load (%a);
                cond = fcmp ult (f64 3.0), (%la);
                br (%cond) l1, l2;
            l1:
                ret (i32 1);
            l2:
                ret (i32 2);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let res = jit.run(func, vec![]);
        assert_eq!(res, exec::jit::GenericValue::Int32(2));
    }

    #[test]
    fn pass_arr() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [i32] func [(ptr [16; i32])] {
        entry:
            p = gep (%arg.0), [(i32 0), (i32 1)];
            store (i32 12), (%p);
            ret (i32 0);
        });

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        let arr: [u32; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
        jit.run(
            func,
            vec![exec::jit::GenericValue::Address(arr.as_ptr() as *mut u8)],
        );
        assert_eq!(arr[1], 12);
    }

    #[test]
    fn branch_folding() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [i32] func [(i32)] {
        entry:
            a = add (%arg.0), (i32 1);
            br l1;
        l1:
            a = add (%a), (i32 1);
            br l2;
        l2:
            a = add (%a), (i32 1);
            br l3;
        l3:
            a = add (%a), (i32 1);
            a = add (%a), (i32 1);
            a = add (%a), (i32 1);
            br l4;
        l4:
            a = add (%a), (i32 1);
            br l5;
        l5:
            ret (%a);
        });
        println!("{:?}", m);

        ir::cse::CommonSubexprElimination::new().run_on_module(&mut m);
        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(1)]),
            exec::jit::GenericValue::Int32(8)
        );
    }

    #[test]
    fn cse0() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [i32] func [] {
        entry:
            a = alloca i32;
            store (i32 1), (%a);
            b = load (%a);
            c = add (%b), (i32 2);
            d = mul (%c), (i32 2);
            e = add (%b), (i32 2);
            f = mul (%e), (i32 3);
            g = add (%d), (%f);
            ret (%g);
        });
        println!("{:?}", m);

        ir::cse::CommonSubexprElimination::new().run_on_module(&mut m);
        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(15));
    }

    #[test]
    fn cse1() {
        let mut m = module::Module::new("cilk");

        cilk_ir!(m; define [i32] func [(i32)] {
        entry:
            a = alloca i32;
            store (i32 1), (%a);
            v = load (%a);
            c = icmp eq (%arg.0), (i32 1);
            br (%c) label1, label2;
        label1:
            x = add (%v), (i32 2);
            store (%x), (%a);
            br label3;
        label2:
            x = add (%v), (i32 2);
            store (%x), (%a);
            br label3;
        label3:
            x = add (%v), (i32 2);
            br label4;
        label4:
            store (%x), (%a);
            x = add (%v), (i32 2);
            ret (%x);
        });
        println!("{:?}", m);

        ir::mem2reg::Mem2Reg::new().run_on_module(&mut m);
        ir::cse::CommonSubexprElimination::new().run_on_module(&mut m);
        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("func").unwrap();
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(1)]),
            exec::jit::GenericValue::Int32(3)
        );
        assert_eq!(
            jit.run(func, vec![exec::jit::GenericValue::Int32(0)]),
            exec::jit::GenericValue::Int32(3)
        );
    }

    #[test]
    fn pass_struct() {
        let mut m = module::Module::new("cilk");

        let struct_ty = m
            .types
            .new_struct_ty(vec![types::Type::i32, types::Type::i32]);
        let f = m.create_function("f", types::Type::i32, vec![struct_ty]);
        {
            let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            cilk_ir!((builder) {
                x = gep (%arg.0), [(i32 0), (i32 0)];
                load_x = load (%x);
                y = gep (%arg.0), [(i32 0), (i32 1)];
                load_y = load (%y);
                a = add (%load_x), (%load_y);
                ret (%a);
            });
        }
        let main = m.create_function("main", types::Type::i32, vec![]);
        {
            let mut builder =
                builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, main));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            let var = builder.build_alloca(struct_ty);
            cilk_ir!((builder) {
                x = gep (%var), [(i32 0), (i32 1)];
                store (i32 12), (%x);
                x = gep (%var), [(i32 0), (i32 0)];
                store (i32 10), (%x);
                r = call f [(%var)];
                ret (%r);
            });
        }

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(22));
    }

    #[test]
    fn pass_struct1() {
        let mut m = module::Module::new("cilk");

        let struct_ty = m.types.new_struct_ty(vec![
            types::Type::i32,
            types::Type::i32,
            types::Type::i32,
            types::Type::i32,
            types::Type::i32,
        ]);
        let f = m.create_function("f", types::Type::i32, vec![types::Type::i32, struct_ty]);
        {
            let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            cilk_ir!((builder) {
                x = gep (%arg.1), [(i32 0), (i32 0)];
                load_x = load (%x);
                y = gep (%arg.1), [(i32 0), (i32 1)];
                load_y = load (%y);
                z = gep (%arg.1), [(i32 0), (i32 2)];
                load_z = load (%z);
                a = add (%load_x), (%load_y);
                b = add (%a), (%load_z);
                c = add (%arg.0), (%b);
                ret (%c);
            });
        }
        let main = m.create_function("main", types::Type::i32, vec![]);
        {
            let mut builder =
                builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, main));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            let var = builder.build_alloca(struct_ty);
            cilk_ir!((builder) {
                x = gep (%var), [(i32 0), (i32 0)];
                store (i32 10), (%x);
                x = gep (%var), [(i32 0), (i32 1)];
                store (i32 12), (%x);
                x = gep (%var), [(i32 0), (i32 2)];
                store (i32 14), (%x);
                r = call f [(i32 2), (%var)];
                ret (%r);
            });
        }

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::Int32(38));
    }

    #[test]
    fn pass_struct2() {
        let mut m = module::Module::new("cilk");

        let struct_ty = m
            .types
            .new_struct_ty(vec![types::Type::i32, types::Type::f64]);
        let f = m.create_function("f", types::Type::f64, vec![struct_ty]);
        {
            let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            cilk_ir!((builder) {
                x = gep (%arg.0), [(i32 0), (i32 1)];
                load_x = load (%x);
                ret (%load_x);
            });
        }
        let main = m.create_function("main", types::Type::f64, vec![]);
        {
            let mut builder =
                builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, main));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            let var = builder.build_alloca(struct_ty);
            cilk_ir!((builder) {
                x = gep (%var), [(i32 0), (i32 1)];
                store (f64 12.3), (%x);
                r = call f [(%var)];
                ret (%r);
            });
        }

        println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::F64(12.3));
    }

    #[test]
    fn pass_struct3() {
        let mut m = module::Module::new("cilk");

        let struct_ty = m
            .types
            .new_struct_ty(vec![types::Type::i32, types::Type::f64]);
        let struct_ty2 =
            m.types
                .new_struct_ty(vec![types::Type::f64, types::Type::i32, types::Type::i32]);
        let f = m.create_function("f", types::Type::f64, vec![struct_ty, struct_ty2]);
        {
            let mut builder = builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, f));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            cilk_ir!((builder) {
                x = gep (%arg.0), [(i32 0), (i32 1)];
                load_x = load (%x);
                y = gep (%arg.1), [(i32 0), (i32 0)];
                load_y = load (%y);
                a = add (%load_x), (%load_y);
                ret (%a);
            });
        }
        let main = m.create_function("main", types::Type::f64, vec![]);
        {
            let mut builder =
                builder::Builder::new(builder::FunctionIdWithModule::new(&mut m, main));
            let entry = builder.append_basic_block();
            builder.set_insert_point(entry);
            let var = builder.build_alloca(struct_ty);
            let var2 = builder.build_alloca(struct_ty2);
            cilk_ir!((builder) {
                x = gep (%var), [(i32 0), (i32 1)];
                store (f64 12.3), (%x);
                x = gep (%var2), [(i32 0), (i32 0)];
                store (f64 12.3), (%x);
                r = call f [(%var), (%var2)];
                ret (%r);
            });
        }

        // println!("{:?}", m);

        let mut jit = exec::jit::JITExecutor::new(&mut m);
        let func = jit.find_function_by_name("main").unwrap();
        assert_eq!(jit.run(func, vec![]), exec::jit::GenericValue::F64(24.6));
    }
}
