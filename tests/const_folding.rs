use cilk::ir::{builder, function, module, types, value};

#[test]
fn const_folding() {
    let mut m = module::Module::new("cilk");

    let f = m.add_function(function::Function::new("f", types::Type::Void, vec![]));
    let mut builder = builder::Builder::new(&mut m, f);

    let entry = builder.append_basic_block();
    builder.set_insert_point(entry);

    assert_eq!(
        builder.build_add(
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        ),
        value::Value::Immediate(value::ImmediateValue::Int32(5))
    );
    assert_eq!(
        builder.build_sub(
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        ),
        value::Value::Immediate(value::ImmediateValue::Int32(-1))
    );
    assert_eq!(
        builder.build_mul(
            value::Value::Immediate(value::ImmediateValue::Int32(2)),
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        ),
        value::Value::Immediate(value::ImmediateValue::Int32(6))
    );
    assert_eq!(
        builder.build_rem(
            value::Value::Immediate(value::ImmediateValue::Int32(5)),
            value::Value::Immediate(value::ImmediateValue::Int32(3)),
        ),
        value::Value::Immediate(value::ImmediateValue::Int32(2))
    );
}
