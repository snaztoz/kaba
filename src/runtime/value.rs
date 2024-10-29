use compiler::ast::Literal;
use std::{convert::From, fmt::Display};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Void,
    Integer(i32),
    Float(f64),
    Boolean(bool),
    Function(usize),
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Void => write!(f, "void"),
            RuntimeValue::Integer(n) => write!(f, "{n}"),
            RuntimeValue::Float(n) => write!(f, "{n}"),
            RuntimeValue::Boolean(b) => write!(f, "{b}"),
            RuntimeValue::Function(p) => write!(f, "<function #{p}>"),
        }
    }
}

impl From<Literal> for RuntimeValue {
    fn from(val: Literal) -> Self {
        match val {
            Literal::Void => Self::Void,
            Literal::Integer(n) => Self::Integer(n),
            Literal::Float(n) => Self::Float(n),
            Literal::Boolean(b) => Self::Boolean(b),
        }
    }
}
