// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use runtime::{Runtime, WriteStream};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod runtime;

/// Provide a quick way to run a Kaba program, without the needs to
/// manually lex the source code, parsing the tokens, etc.
///
/// You can also provide custom IO stream using other type that
/// implements [`std::io::Write`], such as [`Vec<u8>`].
///
/// # Examples
///
/// ```
/// // Using standard IO streams.
///
/// use kaba;
/// use std::io;
///
/// let program = "var x = 5; print(x);";
///
/// let result = kaba::run(program, &mut io::stdout(), &mut io::stderr());
///
/// assert!(result.is_ok());
/// ````
///
/// ```
/// // Using Vec<u8> as IO streams.
///
/// use kaba;
///
/// let program = "var x = 5; print(x);";
///
/// let mut output_stream = vec![];
/// let mut error_stream = vec![];
///
/// let result = kaba::run(program, &mut output_stream, &mut error_stream);
///
/// assert!(result.is_ok());
/// ```
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
