use std::fmt::Display;

use logos::Span;

pub type Result<T> = std::result::Result<T, LexingError>;

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LexingError {
    InvalidSymbol {
        token: String,
        span: Span,
    },

    UnexpectedEof {
        span: Span,
    },

    UnexpectedToken {
        span: Span,
    },

    UnknownToken {
        token: String,
        span: Span,
    },

    UnsupportedEscapeCharacter {
        c: char,
        span: Span,
    },

    InvalidHexNumber {
        n: String,
        span: Span,
    },

    #[default]
    Default,
}

impl LexingError {
    pub fn span(&self) -> Span {
        match self {
            Self::InvalidSymbol { span, .. }
            | Self::UnexpectedEof { span, .. }
            | Self::UnexpectedToken { span, .. }
            | Self::UnknownToken { span, .. }
            | Self::UnsupportedEscapeCharacter { span, .. }
            | Self::InvalidHexNumber { span, .. } => span.clone(),

            _ => unreachable!(),
        }
    }
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidSymbol { token, .. } => {
                write!(f, "not a valid symbol: `{token}`")
            }
            Self::UnexpectedEof { .. } => {
                write!(f, "not expecting an end-of-file (EOF)")
            }
            Self::UnexpectedToken { .. } => {
                write!(f, "unexpected token")
            }
            Self::UnknownToken { token, .. } => {
                write!(f, "unknown token: `{token}`")
            }
            Self::UnsupportedEscapeCharacter { c, .. } => {
                write!(f, "unsupported escape character: `\\{c}`")
            }
            Self::InvalidHexNumber { n, .. } => {
                write!(f, "not a valid hex number: `{n}`")
            }
            _ => unreachable!(),
        }
    }
}
