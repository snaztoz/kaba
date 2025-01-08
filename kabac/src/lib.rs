#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

pub use compiler::Compiler;
pub use error::Error;
pub use semantic::state::symtable::SymTable;

pub mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod semantic;

type Result<'a, T> = std::result::Result<T, Error<'a>>;
