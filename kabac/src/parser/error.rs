use crate::lexer::token::TokenKind;
use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, ParsingError>;

#[derive(Debug, PartialEq)]
pub struct ParsingError {
    pub variant: ParsingErrorVariant,
    pub span: Span,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingErrorVariant {
    UnexpectedToken { expect: TokenKind, found: TokenKind },
    NumberLiteralLimitExceeded,
}

impl Display for ParsingErrorVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expect, found, .. } => {
                write!(f, "expecting {expect}, found {found} instead")
            }
            Self::NumberLiteralLimitExceeded => {
                write!(f, "number literal limit exceeded")
            }
        }
    }
}
