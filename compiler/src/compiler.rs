// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the high-level utility for the source
//! code compilation procedure.

use crate::{ast::Program as ProgramAst, error::ErrorVariant, lexer, parser, Error, Result};
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Compiler instance that will handle the overall compilation
/// process of a Kaba source code.
pub struct Compiler {
    file_path: Option<PathBuf>,
    source_code: String,
}

impl Compiler {
    /// Construct a compiler instance straight from a source code
    /// and without file path.
    pub fn from_source_code(source_code: &str) -> Self {
        Self {
            file_path: None,
            source_code: String::from(source_code),
        }
    }

    /// Construct a compiler instance from a source code file path.
    pub fn from_source_code_file(file_path: &Path) -> Result<Self> {
        if !matches!(file_path.extension().and_then(|e| e.to_str()), Some("kaba")) {
            return Err(Error {
                file_path: PathBuf::from(file_path),
                source_code: String::new(),
                variant: Box::new(ErrorVariant::SourceCodeWrongExtension),
            });
        } else if !file_path.exists() {
            return Err(Error {
                file_path: PathBuf::from(file_path),
                source_code: String::new(),
                variant: Box::new(ErrorVariant::SourceCodeFileNotExist {
                    path: PathBuf::from(file_path),
                }),
            });
        }

        let source_code = fs::read_to_string(file_path).unwrap();

        Ok(Self {
            file_path: Some(PathBuf::from(file_path)),
            source_code,
        })
    }

    /// Run the compilation process.
    pub fn compile(&self) -> Result<ProgramAst> {
        lexer::lex(&self.source_code)
            .and_then(parser::parse)
            .map_err(|e| Error {
                file_path: self.file_path.clone().unwrap_or(PathBuf::new()),
                source_code: self.source_code.clone(),
                variant: Box::new(e),
            })
    }
}
