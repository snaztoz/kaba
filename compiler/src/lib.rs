// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use ast::Program as ProgramAst;
pub use error::Error;

pub mod ast;
mod error;
mod lexer;
mod parser;
mod util;

/// Provide a quick way to compile a Kaba program, without the needs to
/// manually lex the source code, parsing the tokens, etc.
///
/// # Examples
///
/// ```
///
/// use compiler as kabac;
///
/// let program = "var x = 5; print(x);";
///
/// let result = kabac::compile(program);
///
/// assert!(result.is_ok());
/// ```
///
pub fn compile(program: &str) -> Result<ProgramAst, Error> {
    let tokens = lexer::lex(program)?;
    let ast = parser::parse(tokens)?;

    // TODO: return bytecode

    Ok(ast)
}
