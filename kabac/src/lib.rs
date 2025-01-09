#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use ast::AstNode;
pub use error::Error;
pub use logos::Span;
pub use semantic::state::symtable::SymTable;

pub mod ast;
mod error;
mod lexer;
mod parser;
mod semantic;

type Result<'a, T> = std::result::Result<T, Error>;

pub fn compile(src: &str) -> Result<(AstNode, SymTable)> {
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
