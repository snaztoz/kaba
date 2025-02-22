#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

pub use ast::{AstNode, AstNodeVariant, FunctionParam, Literal};
pub use error::{Error, Result};
pub use logos::Span;
pub use semantic::state::SymbolTable;

mod ast;
mod error;
mod lexer;
mod parser;
mod semantic;

/// Compile Kaba source code.
///
/// It is assumed that all newlines are already normalized to line feed
/// character (LF).
///
/// If it is not yet normalized, use the [`normalize_newlines`] function to
/// achieve this.
pub fn compile(src: &str) -> Result<(AstNode<'_>, SymbolTable)> {
    let tokens = lexer::lex(src).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span),
    })?;

    let ast = parser::parse(tokens).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span),
    })?;

    let sym_table = semantic::analyze(&ast).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span),
    })?;

    Ok((ast, sym_table))
}

// Normalize all newline characters to line feed (LF).
pub fn normalize_newlines(src: &str) -> String {
    src.replace("\r\n", "\n").replace('\r', "\n")
}
