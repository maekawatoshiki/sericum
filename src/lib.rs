pub mod ir;

#[cfg(test)]
mod tests {
    use crate::ir;

    #[test]
    fn it_works() {
        // nonsense for now
        let a = ir::types::Type::Int32;
        let b = ir::module::Module::new("cilk");
    }
}
