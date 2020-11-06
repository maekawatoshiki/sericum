macro_rules! some_then {
    ($x:pat, $e:expr, $t:expr) => {{
        if let Some($x) = $e {
            $t
        }
    }};
}

#[allow(unused_macros)]
macro_rules! match_then {
    ($x:pat, $e:expr, $t:expr) => {{
        if let $x = $e {
            $t
        }
    }};
}

macro_rules! debug {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!("Debug at {}", file!());
            $($arg)*;
        }
    };
}

#[macro_export]
macro_rules! sericum_parse_ty {
    ($_:expr, i8) => {
        types::Type::i8
    };
    ($_:expr, i32) => {
        types::Type::i32
    };
    ($_:expr, i64) => {
        types::Type::i64
    };
    ($_:expr, f64) => {
        types::Type::f64
    };
    ($_:expr, void) => {
        types::Type::Void
    };
    ($tys:expr, ptr $($elem:tt)*) => {{
        let e = $crate::sericum_parse_ty!($tys, $($elem)*);
        $tys.new_pointer_ty(e)
    }};
    ($tys:expr, [$n:expr; $ty:ident]) => {{
        let e = { $crate::sericum_parse_ty!($tys, $ty)};
        $tys.new_array_ty(e, $n)
    }};
    ($tys:expr, [$n:expr; $($elem:tt)*]) => {{
        let e = { $crate::sericum_parse_ty!($tys, $($elem)*) };
        $tys.new_array_ty(e, $n)
    }};
}

#[macro_export]
macro_rules! sericum_value {
    ($builder:expr; %arg . $n:expr) => {{
        $builder.get_param($n).unwrap()
    }};
    ($builder:expr; void) => {{
        value::Value::None
    }};
    ($builder:expr; i8 $n:expr) => {{
        value::Value::Immediate(value::ImmediateValue::Int8($n))
    }};
    ($builder:expr; i32 $n:expr) => {{
        value::Value::Immediate(value::ImmediateValue::Int32($n))
    }};
    ($builder:expr; i64 $n:expr) => {{
        value::Value::Immediate(value::ImmediateValue::Int64($n))
    }};
    ($builder:expr; f64 $n:expr) => {{
        value::Value::Immediate(value::ImmediateValue::F64($n))
    }};
    ($builder:expr; % $n:expr) => {{
        $n
    }};
}

#[macro_export]
macro_rules! icmp_kind {
    (le) => {
        opcode::ICmpKind::Le
    };
    (eq) => {
        opcode::ICmpKind::Eq
    };
    (lt) => {
        opcode::ICmpKind::Lt
    };
}

#[macro_export]
macro_rules! fcmp_kind {
    (ule) => {
        opcode::FCmpKind::ULe
    };
    (ueq) => {
        opcode::FCmpKind::UEq
    };
    (ult) => {
        opcode::FCmpKind::ULt
    };
}

#[macro_export]
macro_rules! sericum_expr {
    ($builder:expr; $bb_map:expr; $label:ident : $($remain:tt)*) => {
        let bb = *$bb_map.entry(stringify!($label)).or_insert_with(|| $builder.append_basic_block());
        $builder.set_insert_point(bb);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = alloca $ty:ident; $($remain:tt)*) => {
        let $x = $builder.build_alloca($crate::sericum_parse_ty!($builder.func_ref_mut().types, $ty));
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = alloca_ ($($ty:tt)*); $($remain:tt)*) => {
        let $x = {
            let types = &mut $builder.func_ref_mut().types;
            let ty = $crate::sericum_parse_ty!(types, $( $ty )*);
            $builder.build_alloca(ty)
        };
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = load ($($val:tt)*); $($remain:tt)*) => {
        let val = $crate::sericum_value!($builder; $( $val )*);
        let $x = $builder.build_load(val);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; store ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let src = $crate::sericum_value!($builder; $( $val1 )*);
        let dst = $crate::sericum_value!($builder; $( $val2 )*);
        $builder.build_store(src, dst);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = add ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_add(val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = sub ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_sub(val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = mul ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_mul(val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = div ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_div(val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = rem ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_rem(val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = sext [$($ty:tt)*] ($($val:tt)*); $($remain:tt)*) => {
        let val = $crate::sericum_value!($builder; $( $val )*);
        let ty = $crate::sericum_parse_ty!($builder.func.module.types, $($ty)*);
        let $x = $builder.build_sext(val, ty);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = gep ($($val:tt)*), [$( ( $($idx:tt)* ) ),*] ; $($remain:tt)*) => {
        let val = $crate::sericum_value!($builder; $( $val )*);
        let indices = vec![$( $crate::sericum_value!($builder; $( $idx )*) ),*];
        let $x = $builder.build_gep(val, indices);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = phi [$( [ ($($arg:tt)*), $bb:ident ] ),*] ; $($remain:tt)*) => {
        let args = vec![$(
            ($crate::sericum_value!($builder; $( $arg )*),
            *$bb_map.entry(stringify!($bb)).or_insert_with(|| $builder.append_basic_block()))
        ),*];
        let $x = $builder.build_phi(args);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = call $name:ident [$( ( $($arg:tt)* ) ),*] ; $($remain:tt)*) => {
        let args = vec![ $( $crate::sericum_value!($builder; $( $arg )*) ),* ];
        let $x = $builder.build_call(value::Value::Function({
            let id = $builder.module().unwrap().find_function(stringify!($name)).unwrap();
            value::FunctionValue {
                func_id: id,
                ty: $builder.module().unwrap().function_ref(id).ty,
            }}), args);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = call (->$id:expr) [$( ( $($arg:tt)* ) ),*] ; $($remain:tt)*) => {
        let args = vec![ $( $crate::sericum_value!($builder; $( $arg )*) ),* ];
        let $x = $builder.build_call(value::Value::Function({
            let ty = $builder.module().unwrap().function_ref($id).ty;
            value::FunctionValue { func_id: $id, ty}
        }), args);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = icmp $kind:ident ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_icmp($crate::icmp_kind!($kind), val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; $x:ident = fcmp $kind:ident ($($val1:tt)*), ($($val2:tt)*); $($remain:tt)*) => {
        let val1 = $crate::sericum_value!($builder; $( $val1 )*);
        let val2 = $crate::sericum_value!($builder; $( $val2 )*);
        let $x = $builder.build_fcmp($crate::fcmp_kind!($kind), val1, val2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; br ($($cond:tt)*) $l1:ident, $l2:ident; $($remain:tt)*) => {
        let bb1 = *$bb_map.entry(stringify!($l1)).or_insert_with(|| $builder.append_basic_block());
        let bb2 = *$bb_map.entry(stringify!($l2)).or_insert_with(|| $builder.append_basic_block());
        let cond = $crate::sericum_value!($builder; $( $cond )*);
        $builder.build_cond_br(cond, bb1, bb2);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; br $label:ident; $($remain:tt)*) => {
        let bb = *$bb_map.entry(stringify!($label)).or_insert_with(|| $builder.append_basic_block());
        $builder.build_br(bb);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };
    ($builder:expr; $bb_map:expr; ret ($($val:tt)*) ; $($remain:tt)*) => {
        let val = $crate::sericum_value!($builder; $( $val )*);
        $builder.build_ret(val);
        $crate::sericum_expr!($builder; $bb_map; $( $remain )*);
    };

    ($builder:expr; $bb_map:expr; ) => {{}};
}

// TODO: Use proc-macro
#[macro_export]
macro_rules! sericum_ir {
    ($m:expr; define [$($ret_ty:tt)*] $name:ident [$(($($arg:tt)*)),*] { $($exp:tt)* }) => {{
        use rustc_hash::FxHashMap;
        use $crate::ir::{basic_block, builder::IRBuilder};
        let ret_ty = $crate::sericum_parse_ty!($m.types, $($ret_ty)*);
        let args_ty = vec![$( $crate::sericum_parse_ty!($m.types, $($arg)*) ),*];
        let f_id = $m.create_function(
            stringify!($name), ret_ty, args_ty
        );
        let mut builder = $m.ir_builder(f_id);
        let mut bb_map: FxHashMap<&str, basic_block::BasicBlockId> = FxHashMap::default();
        $crate::sericum_expr!(builder; bb_map; $( $exp )*);
        f_id
    }};
    (($builder:expr) { $($exp:tt)* }) => {{
        use $crate::ir::basic_block;
        use rustc_hash::FxHashMap;
        let mut bb_map: FxHashMap<&str, basic_block::BasicBlockId> = FxHashMap::default();
        $crate::sericum_expr!($builder; bb_map; $( $exp )*);
    }}
}
