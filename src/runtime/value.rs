use compiler::ast::Value;
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

impl From<Value> for RuntimeValue {
    fn from(val: Value) -> Self {
        match val {
            Value::Void => Self::Void,
            Value::Integer(n) => Self::Integer(n),
            Value::Float(n) => Self::Float(n),
            Value::Boolean(b) => Self::Boolean(b),
        }
    }
}
