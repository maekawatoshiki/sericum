use super::module::*;

#[derive(Debug, Clone)]
pub struct Context {}

impl Context {
    pub fn new() -> Self {
        Context {}
    }

    pub fn create_module(&mut self) {}
}
