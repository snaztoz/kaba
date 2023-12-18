// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains errors that may be thrown during
//! compiling process.

use crate::{lexer::Token, util};
use indoc::formatdoc;
use logos::Span;
use std::fmt;

/// Errors that may be thrown during the compiling stage, such
/// as invalid syntax or semantic errors.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Error {
    // Lexer errors
    LexingIdentifierStartsWithNumber {
        token: String,
        span: Span,
    },
    LexingUnknownToken {
        token: String,
        span: Span,
    },

    // Parser errors
    ParsingUnexpectedToken {
        expected: Token,
        found: Token,
        span: Span,
    },
    ParsingUnexpectedEof,

    #[default]
    Error,
}

impl Error {
    pub fn get_verbose_message(&self, filename: &str, program: &str) -> String {
        let message = self.to_string();

        let span = match self {
            Self::LexingIdentifierStartsWithNumber { span, .. } => span.clone(),
            Self::LexingUnknownToken { span, .. } => span.clone(),
            Self::ParsingUnexpectedToken { span, .. } => span.clone(),
            Self::ParsingUnexpectedEof => program.len()..program.len(),
            _ => unreachable!(),
        };

        let (line, row, col) = util::find_line_row_and_col_from_span(program, &span);

        let row_number_pad = (0..row.to_string().len()).map(|_| " ").collect::<String>();

        formatdoc! {"
            at {filename} ({row}:{col})
             {row_number_pad} |
             {row} | {line}
             {row_number_pad} |
             {row_number_pad}--> {message}
        "}
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexingIdentifierStartsWithNumber { token, .. } => {
                write!(f, "identifier can't start with number: `{token}`")
            }
            Self::LexingUnknownToken { token, .. } => {
                write!(f, "unknown token `{token}`")
            }

            Self::ParsingUnexpectedToken {
                expected, found, ..
            } => write!(f, "expecting to find {expected}, but get {found} instead",),
            Self::ParsingUnexpectedEof => write!(f, "unexpected end-of-file (EOF)",),

            _ => unreachable!(),
        }
    }
}
