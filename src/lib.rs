pub mod ir;

#[cfg(test)]
mod tests {
    use crate::ir;

    #[test]
    fn it_works() {
        let a = ir::types::Type::Int32;
    }
}
