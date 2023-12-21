// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains errors that may be thrown during
//! compiling process.

use crate::{lexer::Token, util};
use indoc::writedoc;
use logos::Span;
use std::{borrow::Borrow, fmt, path::PathBuf};

/// Wraps error variants that may occur during the compilation
/// process, alongside with the information of its source code
/// file path and the source code itself.
#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub file_path: PathBuf,
    pub source_code: String,
    pub variant: Box<ErrorVariant>, // box in order to avoid the size from getting too large
}

impl Error {
    fn get_error_span(&self) -> Span {
        match self.variant.borrow() {
            ErrorVariant::LexingIdentifierStartsWithNumber { span, .. } => span.clone(),
            ErrorVariant::LexingUnknownToken { span, .. } => span.clone(),
            ErrorVariant::ParsingUnexpectedToken { span, .. } => span.clone(),
            ErrorVariant::ParsingUnexpectedEof => self.source_code.len()..self.source_code.len(),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file_path = self.file_path.display();
        let message = self.variant.to_string();
        let span = self.get_error_span();

        let (line, row, col) = util::get_line_row_and_col_from_span(&self.source_code, &span);
        let row_number_pad = util::pad_white_spaces(row.to_string().len());

        writedoc!(
            f,
            "
            at {file_path} ({row}:{col})
             {row_number_pad} |
             {row} | {line}
             {row_number_pad} |
             {row_number_pad}--> {message}
            ",
        )
    }
}

/// Error variants that may be thrown during the compiling stage,
/// such as invalid syntax or semantic errors.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum ErrorVariant {
    // Source code file errors
    SourceCodeWrongExtension,
    SourceCodeFileNotExist {
        path: PathBuf,
    },

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

impl fmt::Display for ErrorVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SourceCodeWrongExtension => {
                write!(f, "Kaba source code file must have '.kaba' extension")
            }
            Self::SourceCodeFileNotExist { path } => {
                write!(f, "file '{}' is not exist", path.display())
            }

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
