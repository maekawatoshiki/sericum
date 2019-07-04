#![feature(proc_macro_hygiene)]

#[macro_use]
pub mod macros;
pub mod exec;
pub mod ir;

#[macro_use]
extern crate dynasm;
extern crate dynasmrt;
extern crate id_arena;
extern crate rustc_hash;

#[cfg(test)]
mod tests {
    use crate::{
        exec::{interpreter::interp, jit::x64::compiler, jit::x64::liveness, jit::x64::regalloc},
        ir::{builder, function, module, opcode, types, value},
    };

    #[test]
    fn interpret_phi() {
        let mut m = module::Module::new("cilk");

        let f_id = m.add_function(function::Function::new(
            "f",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let mut builder = builder::Builder::new(&mut m, f_id);

        let bb = builder.append_basic_block();
        let bb2 = builder.append_basic_block();
        let if_true = builder.append_basic_block();
        let if_false = builder.append_basic_block();
        let merge = builder.append_basic_block();

        builder.set_insert_point(bb);
        let var = builder.build_alloca(types::Type::Int32);
        let val = builder.build_load(var);
        let val2 = builder.build_add(
            val,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        builder.build_br(bb2);
        builder.set_insert_point(bb2);
        let arg0 = builder.get_param(0).unwrap();
        let val3 = builder.build_add(val2, arg0);
        let eq = builder.build_icmp(
            opcode::ICmpKind::Eq,
            val3,
            value::Value::Immediate(value::ImmediateValue::Int32(4)),
        );
        builder.build_cond_br(eq, if_true, if_false);
        builder.set_insert_point(if_true);
        builder.build_br(merge);
        builder.set_insert_point(if_false);
        builder.build_br(merge);
        builder.set_insert_point(merge);
        let ret = builder.build_phi(vec![
            (
                value::Value::Immediate(value::ImmediateValue::Int32(1)),
                if_true,
            ),
            (val3, if_false),
        ]);
        builder.build_ret(ret);

        let f = m.function_ref(f_id);
        println!("{}", f.to_string(&m));

        let mut interp = interp::Interpreter::new(&m);
        let ret = interp.run_function(f_id, vec![interp::ConcreteValue::Int32(3)]);
        assert_eq!(ret, interp::ConcreteValue::Int32(1));
        let ret = interp.run_function(f_id, vec![interp::ConcreteValue::Int32(5)]);
        assert_eq!(ret, interp::ConcreteValue::Int32(6));

        println!("exec: f(5) = {:?}", ret);
    }

    #[test]
    pub fn liveness_test() {
        let mut m = module::Module::new("cilk");

        let fibo = m.add_function(function::Function::new(
            "f",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let mut builder = builder::Builder::new(&mut m, fibo);

        let bb = builder.append_basic_block();
        let bb2 = builder.append_basic_block();
        let if_true = builder.append_basic_block();
        let if_false = builder.append_basic_block();
        let merge = builder.append_basic_block();

        builder.set_insert_point(bb);
        let var = builder.build_alloca(types::Type::Int32);
        let val = builder.build_load(var);
        let val2 = builder.build_add(
            val,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        let _ = builder.build_add(
            val,
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        );
        builder.build_br(bb2);
        builder.set_insert_point(bb2);
        let arg0 = builder.get_param(0).unwrap();
        let val3 = builder.build_add(val2, arg0);
        let eq = builder.build_icmp(
            opcode::ICmpKind::Eq,
            val3,
            value::Value::Immediate(value::ImmediateValue::Int32(4)),
        );
        builder.build_cond_br(eq, if_true, if_false);
        builder.set_insert_point(if_true);
        builder.build_br(merge);
        builder.set_insert_point(if_false);
        builder.build_br(merge);
        builder.set_insert_point(merge);
        let ret = builder.build_phi(vec![
            (
                value::Value::Immediate(value::ImmediateValue::Int32(1)),
                if_true,
            ),
            (val3, if_false),
        ]);
        builder.build_ret(ret);

        let f = m.function_ref(fibo);
        println!("{}", f.to_string(&m));

        let mut liveness = liveness::LivenessAnalyzer::new(&m);
        liveness.analyze();

        let f = m.function_ref(fibo);
        println!("liveness: {}", f.to_string(&m));
    }

    #[test]
    pub fn x64_fibo() {
        let mut m = module::Module::new("cilk");

        let fibo = m.add_function(function::Function::new(
            "f",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let mut builder = builder::Builder::new(&mut m, fibo);

        // let bb = builder.append_basic_block();
        // let bb2 = builder.append_basic_block();
        // let if_true = builder.append_basic_block();
        // let if_false = builder.append_basic_block();
        // let merge = builder.append_basic_block();
        //
        // builder.set_insert_point(bb);
        // let var = builder.build_alloca(types::Type::Int32);
        // let val = builder.build_load(var);
        // let val2 = builder.build_add(
        //     val,
        //     value::Value::Immediate(value::ImmediateValue::Int32(1)),
        // );
        // let val3 = builder.build_add(
        //     val,
        //     value::Value::Immediate(value::ImmediateValue::Int32(3)),
        // );
        // builder.build_br(bb2);
        // builder.set_insert_point(bb2);
        // let arg0 = builder.get_param(0).unwrap();
        // let val3 = builder.build_add(val2, arg0);
        // let eq = builder.build_icmp(
        //     opcode::ICmpKind::Eq,
        //     val3,
        //     value::Value::Immediate(value::ImmediateValue::Int32(4)),
        // );
        // builder.build_cond_br(eq, if_true, if_false);
        // builder.set_insert_point(if_true);
        // builder.build_br(merge);
        // builder.set_insert_point(if_false);
        // builder.build_br(merge);
        // builder.set_insert_point(merge);
        // let ret = builder.build_phi(vec![
        //     (
        //         value::Value::Immediate(value::ImmediateValue::Int32(1)),
        //         if_true,
        //     ),
        //     (val3, if_false),
        // ]);
        // builder.build_ret(ret);

        // fibonacci
        let entry = builder.append_basic_block();
        let br1 = builder.append_basic_block();
        let br2 = builder.append_basic_block();
        builder.set_insert_point(entry);
        let arg0 = builder.get_param(0).unwrap();
        let eq1 = builder.build_icmp(
            opcode::ICmpKind::Le,
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
        );
        builder.build_cond_br(eq1, br1, br2);
        builder.set_insert_point(br1);
        builder.build_ret(value::Value::Immediate(value::ImmediateValue::Int32(1)));
        builder.set_insert_point(br2);
        let fibo1arg = builder.build_sub(
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        let fibo1 = builder.build_call(value::Value::Function(fibo), vec![fibo1arg]);
        let fibo2arg = builder.build_sub(
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
        );
        let fibo2 = builder.build_call(value::Value::Function(fibo), vec![fibo2arg]);
        let add = builder.build_add(fibo1, fibo2);
        builder.build_ret(add);

        /*
         * define sum(n) {
         *   if (n == 1) return 1;
         *   return sum(n-1)+n;
         * }
         */
        // let entry = builder.append_basic_block();
        // let br1 = builder.append_basic_block();
        // let br2 = builder.append_basic_block();
        // builder.set_insert_point(entry);
        // let arg0 = builder.get_param(0).unwrap();
        // let eq1 = builder.build_icmp(
        //     opcode::ICmpKind::Eq,
        //     arg0,
        //     value::Value::Immediate(value::ImmediateValue::Int32(1)),
        // );
        // builder.build_cond_br(eq1, br1, br2);
        // builder.set_insert_point(br1);
        // builder.build_ret(value::Value::Immediate(value::ImmediateValue::Int32(1)));
        // builder.set_insert_point(br2);
        // let arg_sum = builder.build_sub(
        //     arg0,
        //     value::Value::Immediate(value::ImmediateValue::Int32(1)),
        // );
        // let call_sum = builder.build_call(value::Value::Function(fibo), vec![arg_sum]);
        // let add = builder.build_add(call_sum, arg0);
        // builder.build_ret(add);

        let f = m.function_ref(fibo);
        println!("{}", f.to_string(&m));

        let mut liveness = liveness::LivenessAnalyzer::new(&m);
        liveness.analyze();

        let f = m.function_ref(fibo);
        println!("liveness: {}", f.to_string(&m));

        let mut regalloc = regalloc::RegisterAllocator::new(&m);
        regalloc.analyze();

        let mut interp = interp::Interpreter::new(&m);
        let ret = interp.run_function(fibo, vec![interp::ConcreteValue::Int32(9)]);
        println!("exec: fibo(9) = {:?}", ret);

        let mut jit = compiler::JITCompiler::new(&m);
        jit.compile(fibo);
        println!(
            "jit: fibo(9) = {}",
            jit.run(fibo, vec![compiler::GenericValue::Int32(9)])
        );
        println!(
            "jit: fibo(40) = {}",
            jit.run(fibo, vec![compiler::GenericValue::Int32(40)])
        );
    }

    #[test]
    fn interpret_fibo() {
        // int fibo(int n) {
        //   if (n <= 2) return 1;
        //   return fibo(n - 1) + fibo(n - 2);
        // }

        let mut m = module::Module::new("cilk");

        let f_id = m.add_function(function::Function::new(
            "fibo",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let mut builder = builder::Builder::new(&mut m, f_id);

        let entry = builder.append_basic_block();
        let br1 = builder.append_basic_block();
        let br2 = builder.append_basic_block();
        builder.set_insert_point(entry);
        let arg0 = builder.get_param(0).unwrap();
        let eq1 = builder.build_icmp(
            opcode::ICmpKind::Le,
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
        );
        builder.build_cond_br(eq1, br1, br2);
        builder.set_insert_point(br1);
        builder.build_ret(value::Value::Immediate(value::ImmediateValue::Int32(1)));
        builder.set_insert_point(br2);
        let fibo1arg = builder.build_sub(
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        let fibo1 = builder.build_call(value::Value::Function(f_id), vec![fibo1arg]);
        let fibo2arg = builder.build_sub(
            arg0,
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
        );
        let fibo2 = builder.build_call(value::Value::Function(f_id), vec![fibo2arg]);
        let add = builder.build_add(fibo1, fibo2);
        builder.build_ret(add);

        let f = m.function_ref(f_id);
        println!("{}", f.to_string(&m));

        let mut interp = interp::Interpreter::new(&m);
        let ret = interp.run_function(f_id, vec![interp::ConcreteValue::Int32(10)]);
        assert_eq!(ret, interp::ConcreteValue::Int32(55));

        println!("exec: fibo(10) = {:?}", ret);
    }

    #[test]
    pub fn sum() {
        let mut m = module::Module::new("cilk");

        let sum = m.add_function(function::Function::new(
            "sum",
            types::Type::Int32,
            vec![types::Type::Int32],
        ));
        let mut builder = builder::Builder::new(&mut m, sum);

        /*
         * int sum(int max) {
         *     int i = 1;
         *     int total = 0;
         *     while (i <= max) {
         *         total += i;
         *         i += 1;
         *     }
         *     return total;
         * }
         */
        let entry = builder.append_basic_block();
        let br_cond = builder.append_basic_block();
        let br_loop = builder.append_basic_block();
        let br_merge = builder.append_basic_block();
        builder.set_insert_point(entry);
        let var_i = builder.build_alloca(types::Type::Int32);
        let var_total = builder.build_alloca(types::Type::Int32);
        builder.build_store(
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
            var_i,
        );
        builder.build_store(
            value::Value::Immediate(value::ImmediateValue::Int32(0)),
            var_total,
        );
        builder.build_br(br_cond);
        builder.set_insert_point(br_cond);
        builder.set_insert_point(br_cond);
        let arg0 = builder.get_param(0).unwrap();
        let val_i = builder.build_load(var_i);
        let cmp = builder.build_icmp(opcode::ICmpKind::Le, val_i, arg0);
        builder.build_cond_br(cmp, br_loop, br_merge);
        builder.set_insert_point(br_loop);
        let val_total = builder.build_load(var_total);
        let val_i = builder.build_load(var_i);
        let add_total = builder.build_add(val_total, val_i);
        builder.build_store(add_total, var_total);
        let add_i = builder.build_add(
            val_i,
            value::Value::Immediate(value::ImmediateValue::Int32(1)),
        );
        builder.build_store(add_i, var_i);
        builder.build_br(br_cond);
        builder.set_insert_point(br_merge);
        let val_total = builder.build_load(var_total);
        builder.build_ret(val_total);

        // let f = m.function_ref(sum);
        // println!("{}", f.to_string(&m));

        let mut liveness = liveness::LivenessAnalyzer::new(&m);
        liveness.analyze();

        let f = m.function_ref(sum);
        println!("liveness: {}", f.to_string(&m));

        let mut regalloc = regalloc::RegisterAllocator::new(&m);
        regalloc.analyze();

        let mut interp = interp::Interpreter::new(&m);
        let ret = interp.run_function(sum, vec![interp::ConcreteValue::Int32(10)]);
        println!("exec: sum(10) = {:?}", ret);

        let mut jit = compiler::JITCompiler::new(&m);
        jit.compile(sum);
        println!(
            "jit: sum(10) = {}",
            jit.run(sum, vec![compiler::GenericValue::Int32(10)])
        );
    }
}
