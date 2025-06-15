use logos::Span;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, LexingError>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct LexingError {
    pub variant: LexingErrorVariant,
    pub span: Span,
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LexingErrorVariant {
    InvalidHexNumber,
    InvalidSymbol,
    UnexpectedEof,
    UnexpectedToken,
    UnsupportedEscapeCharacter,

    #[default]
    UnknownToken,
}

impl Display for LexingErrorVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidHexNumber => {
                write!(f, "not a valid hex number")
            }
            Self::InvalidSymbol => {
                write!(f, "not a valid symbol")
            }
            Self::UnexpectedEof => {
                write!(f, "not expecting an end-of-file (EOF)")
            }
            Self::UnexpectedToken => {
                write!(f, "unexpected token")
            }
            Self::UnsupportedEscapeCharacter => {
                write!(f, "unsupported escape character")
            }
            Self::UnknownToken => {
                write!(f, "unknown token")
            }
        }
    }
}
