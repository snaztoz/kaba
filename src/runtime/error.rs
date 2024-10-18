use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    VariableAlreadyExist(String),
    VariableNotExist(String),
    IdentifierNotExist(String),
    WrongNumberOfArguments { expected: usize, provided: usize },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableAlreadyExist(name) => writeln!(f, "variable `{name}` already exist"),
            Self::VariableNotExist(name) => writeln!(f, "variable `{name}` is not exist"),
            Self::IdentifierNotExist(name) => writeln!(f, "identifier `{name}` is not exist"),
            Self::WrongNumberOfArguments { expected, provided } => writeln!(
                f,
                "expecting {} number of argument(s), get {} instead",
                expected, provided
            ),
        }
    }
}
