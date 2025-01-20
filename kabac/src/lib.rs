#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

pub use ast::{AstNode, FunctionParam, Literal};
pub use error::{Error, Result};
pub use logos::Span;
pub use semantic::state::SymbolTableData;

mod ast;
mod error;
mod lexer;
mod parser;
mod semantic;

pub fn compile(src: &str) -> Result<(AstNode, SymbolTableData)> {
    let src = normalize_newlines(src);

    let tokens = lexer::lex(&src).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span()),
    })?;

    let ast = parser::parse(tokens).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span()),
    })?;

    let sym_table = semantic::analyze(&ast).map_err(|e| Error {
        message: e.to_string(),
        span: Some(e.span().clone()),
    })?;

    Ok((ast, sym_table))
}

// Normalize all newline characters to LF
fn normalize_newlines(src: &str) -> String {
    src.replace("\r\n", "\n").replace('\r', "\n")
}
