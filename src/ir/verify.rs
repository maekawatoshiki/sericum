use crate::{function::Function, module::Module};
use std::{error::Error, fmt};

#[derive(Debug)]
pub enum VerifyError {
    Message(&'static str),
}

type Result<T> = ::std::result::Result<T, VerifyError>;

pub fn verify_module(module: &Module) -> Result<()> {
    for (_, func) in &module.functions {
        verify_function(func)?
    }

    Ok(())
}

pub fn verify_function(_func: &Function) -> Result<()> {
    Ok(())
}

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VerifyError::Message(msg) => write!(f, "VerifyError: Message: {}", msg),
        }
    }
}

impl Error for VerifyError {}
