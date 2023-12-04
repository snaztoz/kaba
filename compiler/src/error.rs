// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains errors that may be thrown during
//! compiling process.

use crate::lexer::Token;
use std::fmt;

/// Errors that may be thrown during the compiling stage, such
/// as invalid syntax or semantic errors.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Error {
    // Lexer errors
    LexingIdentifierStartsWithNumber,

    // Parser errors
    ParsingUnexpectedToken {
        expected: Token,
        found: Token,
        pos: usize,
    },
    ParsingUnexpectedEof,

    // Default
    #[default]
    Error,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexingIdentifierStartsWithNumber => {
                write!(f, "Identifier can't start with number")
            }

            Self::ParsingUnexpectedToken {
                expected, found, ..
            } => write!(
                f,
                "expecting to find {}, but get {} instead",
                expected, found
            ),
            Self::ParsingUnexpectedEof => write!(f, "unexpected end-of-file (EOF)",),

            _ => unreachable!(),
        }
    }
}
