use crate::lexer::TokenKind;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, ParsingError>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedToken {
        expect: TokenKind,
        found: TokenKind,
        span: Span,
    },
}

impl ParsingError {
    pub fn span(&self) -> Span {
        match self {
            Self::UnexpectedToken { span, .. } => span.clone(),
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expect, found, .. } => {
                write!(f, "expecting to find {expect} but get {found} instead",)
            }
        }
    }
}
