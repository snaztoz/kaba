use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Void,
    Integer(i32),
    Float(f64),
    Boolean(bool),
    Function(usize),
    Array(usize),
}

impl RuntimeValue {
    pub fn unwrap_integer(&self) -> usize {
        if let RuntimeValue::Integer(i) = self {
            usize::try_from(*i).unwrap()
        } else {
            panic!()
        }
    }

    pub fn unwrap_array_ptr(&self) -> usize {
        if let Self::Array(ptr) = self {
            *ptr
        } else {
            panic!()
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),

            Self::Function(p) => write!(f, "<function #{p}>"),
            Self::Array(p) => write!(f, "<array #{p}>"),
        }
    }
}
