//! This module contains errors that may be thrown during compiling process.

use logos::Span;

/// Wraps error variants that may occur during the compilation process,
/// alongside with the information of its source code file path and the source
/// code itself.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Error {
    pub message: String,
    pub span: Option<Span>,
}
