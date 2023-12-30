// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains errors that may be thrown during
//! compiling process.

use crate::util;
use indoc::writedoc;
use logos::Span;
use std::{fmt, path::PathBuf};

/// Wraps error variants that may occur during the compilation
/// process, alongside with the information of its source code
/// file path and the source code itself.
#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub file_path: PathBuf,
    pub source_code: String,
    pub message: String,
    pub span: Option<Span>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = &self.message;
        let file_path = self.file_path.display();

        let position = self
            .span
            .as_ref()
            .map(|span| util::get_position_from_span(&self.source_code, span));

        if let Some((line, row, col)) = position {
            let position_information = format!(", at `{file_path}` ({row}:{col})");
            let row_number_pad = util::pad_white_spaces(row.to_string().len());

            writedoc!(
                f,
                "
                {message}{position_information}
                 {row_number_pad} |
                 {row} | {line}
                 {row_number_pad} |"
            )
        } else {
            write!(f, "{message}")
        }
    }
}
