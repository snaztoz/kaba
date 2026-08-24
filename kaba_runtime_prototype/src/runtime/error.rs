use std::fmt::Display;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    DivisionByZero,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::DivisionByZero => write!(f, "division by zero"),
        }
    }
}
