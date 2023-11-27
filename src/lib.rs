// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use runtime::{Runtime, WriteStream};

mod ast;
mod lexer;
mod parser;
pub mod runtime;

pub fn run<'a>(
    program: &str,
    output_stream: WriteStream<'a>,
    error_stream: WriteStream<'a>,
) -> Result<(), String> {
    let tokens = lexer::lex(program).map_err(|e| e.to_string())?;
    let ast = parser::parse(tokens).map_err(|e| e.to_string())?;

    let mut runtime = Runtime::new(program, ast, output_stream, error_stream);
    runtime.run().map_err(|e| e.to_string())?;

    Ok(())
}
