use cilk::{
    codegen::x64::exec,
    // exec::{interpreter::interp, jit::x64::compiler},
    ir::{builder, context, types, value},
    *,
};

#[test]
fn jit_executor1() {
    let mut ctx = context::Context::new();
    let m = ctx.create_module("cilk");

    // Internal function must be defined before you use it
    let cilk_println_i32 = m.create_function(
        "cilk.println.i32",
        ir::types::Type::Void,
        vec![ir::types::Type::Int32],
    );

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

        // entry:
        //     a = add (%arg.0), (i32 123);
        //     br l;
        // l:
        //     ret (%a);

        // entry:
        //     a = add (%arg.0), (i32 2);
        //     i = rem (%arg.0), (i32 3);
        //     a = add (%a), (%i);
        //     __ = call (->cilk_println_i32) [(%a)];
        //     b = add (%a), (i32 1);
        //     br label1;
        // label1:
        //     c = add (%a), (%b);
        //     d = add (%a), (%c);
        //     ret (%d);

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
        //     i = alloca i32;
        //     store (i32 12), (%i);
        //     li = load (%i);
        //     a = add (%arg.0), (%li);
        //     ret (%a);

         // entry:
         //     a = alloca_ ([8; i32]);
         //     // a = alloca_ ([2; [2; i32]]);
         //     i = alloca i32;
         //     store (i32 1), (%i);
         //     li = load (%i);
         //
         //     idx = gep (%a), [(i32 0), (%li)];
         //     store (i32 123), (%idx);
         //
         //     idx = gep (%a), [(i32 0), (i32 1)];
         //     l = load (%idx);
         //     ret (%l);


        // entry:
        //     cond = icmp le (%arg.0), (i32 2);
        //     br (%cond) l1, l2;
        // l1:
        //     // br merge;
        //     ret (i32 1);
        // l2:
        //     a1 = sub (%arg.0), (i32 1);
        //     r1 = call func [(%a1)];
        //     a2 = sub (%arg.0), (i32 2);
        //     r2 = call func [(%a2)];
        //     r3 = add (%r1), (%r2);
        //     ret (%r3);
            // br merge;
        // merge:
        //     p = phi [ [(i32 1), l1], [(%r3), l2] ];
        //     ret (%p);
    });

    let _main = cilk_ir!(m; define [void] main (i32) {
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

    let mut jit = exec::jit::JITExecutor::new(&m);
    let main = jit.find_function_by_name("main").unwrap();
    let now = ::std::time::Instant::now();
    println!(
        "main: return: {:?}",
        jit.run(main, vec![exec::jit::GenericValue::Int32(40)])
    );
    println!(
        "duration: {:?}",
        ::std::time::Instant::now().duration_since(now)
    )
}

#[test]
fn jit_executor2() {
    let mut ctx = context::Context::new();
    let m = ctx.create_module("cilk");

    // Internal function must be defined before you use it
    let cilk_println_i32 = m.create_function(
        "cilk.println.i32",
        ir::types::Type::Void,
        vec![ir::types::Type::Int32],
    );

    let func = cilk_ir!(m; define [i32] func (i32) {
        entry:
            cond = icmp le (%arg.0), (i32 2);
            br (%cond) l1, l2;
        l1:
            // br merge;
            ret (i32 1);
        l2:
            a1 = sub (%arg.0), (i32 1);
            r1 = call func [(%a1)];
            a2 = sub (%arg.0), (i32 2);
            r2 = call func [(%a2)];
            r3 = add (%r1), (%r2);
            ret (%r3);
            // br merge;
        // merge:
        //     p = phi [ [(i32 1), l1], [(%r3), l2] ];
        //     ret (%p);
    });

    let _main = cilk_ir!(m; define [void] main (i32) {
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

    let mut jit = exec::jit::JITExecutor::new(&m);
    let main = jit.find_function_by_name("main").unwrap();
    let now = ::std::time::Instant::now();
    println!(
        "main: return: {:?}",
        jit.run(main, vec![exec::jit::GenericValue::Int32(35)])
    );
    println!(
        "duration: {:?}",
        ::std::time::Instant::now().duration_since(now)
    )
}
