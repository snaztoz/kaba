use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Void,
    Int(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(String),
    Function(usize),
    Array(usize),
    Record(usize),
}

impl RuntimeValue {
    pub fn unwrap_integer(&self) -> usize {
        if let RuntimeValue::Int(i) = self {
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
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::String(s) => write!(f, "{s}"),

            Self::Function(p) => write!(f, "<function #{p}>"),
            Self::Array(p) => write!(f, "<array #{p}>"),
            Self::Record(p) => write!(f, "<record #{p}>"),
        }
    }
}
