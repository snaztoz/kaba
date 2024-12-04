use std::fmt::Display;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    // TODO: division by zero error, etc.
}

impl Display for RuntimeError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
