use crate::lexer::token::TokenKind;
use logos::Span;
use std::fmt::Display;

pub type Result<'src, T> = std::result::Result<T, ParsingError<'src>>;

#[derive(Debug, PartialEq)]
pub struct ParsingError<'src> {
    pub variant: ParsingErrorVariant<'src>,
    pub span: Span,
}

impl Display for ParsingError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingErrorVariant<'src> {
    UnexpectedToken {
        expect: TokenKind<'src>,
        found: TokenKind<'src>,
    },
    NumberLiteralLimitExceeded,
}

impl Display for ParsingErrorVariant<'_> {
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
