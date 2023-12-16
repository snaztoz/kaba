// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains errors that may be thrown during
//! compiling process.

use crate::lexer::Token;
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexingIdentifierStartsWithNumber { token, .. } => {
                write!(f, "identifier can't start with number: `{token}`")
            }
            Self::LexingUnknownToken { token, .. } => {
                write!(f, "unknown token: `{token}`")
            }

            Self::ParsingUnexpectedToken {
                expected, found, ..
            } => write!(f, "expecting to find {expected}, but get {found} instead",),
            Self::ParsingUnexpectedEof => write!(f, "unexpected end-of-file (EOF)",),

            _ => unreachable!(),
        }
    }
}
