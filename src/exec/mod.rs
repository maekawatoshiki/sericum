pub mod interpreter;
pub mod jit;

trait TypeSize {
    fn size_in_byte(&self) -> usize;
}

