// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the high-level utility for the source
//! code compilation procedure.

use crate::{ast::Program as ProgramAst, lexer, parser, semantic, Error, Result};
use std::{
    fmt::Display,
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
        let extension = file_path.extension().and_then(|e| e.to_str());
        if !matches!(extension, Some("kaba")) {
            return Err(Error {
                file_path: Some(PathBuf::from(file_path)),
                source_code: String::new(),
                message: SourceCodeError::WrongExtension.to_string(),
                span: None,
            });
        } else if !file_path.exists() {
            let error = SourceCodeError::FileNotExist {
                path: PathBuf::from(file_path),
            };
            return Err(Error {
                file_path: Some(PathBuf::from(file_path)),
                source_code: String::new(),
                message: error.to_string(),
                span: None,
            });
        }

        let source_code = fs::read_to_string(file_path).unwrap();

        Ok(Self {
            file_path: Some(PathBuf::from(file_path)),
            source_code,
        })
    }

    /// Run the compilation process.
    pub fn compile(mut self) -> Result<ProgramAst> {
        self.normalize_newlines();

        let tokens = lexer::lex(&self.source_code);
        if let Err(e) = &tokens {
            return Err(Error {
                file_path: self.file_path,
                source_code: String::from(&self.source_code),
                message: e.to_string(),
                span: e.get_span(),
            });
        }

        let ast = parser::parse(tokens.unwrap());
        if let Err(e) = &ast {
            return Err(Error {
                file_path: self.file_path,
                source_code: self.source_code,
                message: e.to_string(),
                span: e.get_span(),
            });
        }

        if let Err(e) = semantic::check(ast.as_ref().unwrap()) {
            return Err(Error {
                file_path: self.file_path,
                source_code: self.source_code,
                message: e.to_string(),
                span: e.get_span(),
            });
        }

        Ok(ast.unwrap())
    }

    // Normalize all newline characters to LF
    fn normalize_newlines(&mut self) {
        self.source_code = self.source_code.replace("\r\n", "\n").replace('\r', "\n");
    }
}

enum SourceCodeError {
    WrongExtension,
    FileNotExist { path: PathBuf },
}

impl Display for SourceCodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::WrongExtension => {
                    "Kaba source code file must have '.kaba' extension".to_string()
                }
                Self::FileNotExist { path } => {
                    format!("file '{}' is not exist", path.display())
                }
            }
        )
    }
}
