// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use std::path::Path;

use ast::Program;
pub use compiler::Compiler;
pub use error::Error;

pub mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod util;

type Result<T> = std::result::Result<T, Error>;

/// Provide a quick way to compile a Kaba source code file, without the
/// needs to manually lex the source code, parsing the tokens, etc.
///
/// # Examples
///
/// ```no_run
/// use compiler as kabac;
/// use std::path::PathBuf;
///
/// let source_code_file_path = PathBuf::from("my-program.kaba");
///
/// let result = kabac::compile(&source_code_file_path);
///
/// assert!(result.is_ok());
/// ```
///
pub fn compile(file_path: &Path) -> Result<Program> {
    let compiler = Compiler::from_source_code_file(file_path)?;
    compiler.compile()
}
