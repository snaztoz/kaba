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
            Self::Void => write!(f, "void"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Function(p) => write!(f, "<function #{p}>"),
        }
    }
}

impl From<Literal> for RuntimeValue {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Void => Self::Void,
            Literal::Integer(n) => Self::Integer(n.try_into().unwrap()),
            Literal::Float(n) => Self::Float(n),
            Literal::Boolean(b) => Self::Boolean(b),
        }
    }
}
