//! This module contains the high-level utility for the source
//! code compilation procedure.

use crate::{ast::AstNode, lexer, parser, semantic, Error, Result};
use std::{
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

/// Compiler instance that will handle the overall compilation
/// process of a Kaba source code.
pub struct Compiler {
    path: Option<PathBuf>,
    src: String,
}

impl Compiler {
    /// Construct a compiler instance straight from a source code
    /// and without file path.
    pub fn from_src(src: &str) -> Self {
        Self {
            path: None,
            src: String::from(src),
        }
    }

    /// Construct a compiler instance from a source code file path.
    pub fn from_file(path: &Path) -> Result<Self> {
        let ext = path.extension().and_then(|e| e.to_str());
        if !matches!(ext, Some("kaba")) {
            return Err(Error {
                path: Some(PathBuf::from(path)),
                src: String::new(),
                message: SourceCodeError::WrongExtension.to_string(),
                span: None,
            });
        }

        if !path.exists() {
            let error = SourceCodeError::FileNotExist {
                path: PathBuf::from(path),
            };
            return Err(Error {
                path: Some(PathBuf::from(path)),
                src: String::new(),
                message: error.to_string(),
                span: None,
            });
        }

        let src = fs::read_to_string(path).unwrap();

        Ok(Self {
            path: Some(PathBuf::from(path)),
            src,
        })
    }

    /// Run the compilation process.
    pub fn compile(mut self) -> Result<AstNode> {
        self.normalize_newlines();

        let tokens = lexer::lex(&self.src);
        if let Err(e) = &tokens {
            return Err(Error {
                path: self.path,
                src: String::from(&self.src),
                message: e.to_string(),
                span: e.span(),
            });
        }

        let ast = parser::parse(tokens.unwrap());
        if let Err(e) = &ast {
            return Err(Error {
                path: self.path,
                src: self.src,
                message: e.to_string(),
                span: e.span(),
            });
        }

        if let Err(e) = semantic::check(ast.as_ref().unwrap()) {
            return Err(Error {
                path: self.path,
                src: self.src,
                message: e.to_string(),
                span: Some(e.span().clone()),
            });
        }

        Ok(ast.unwrap())
    }

    // Normalize all newline characters to LF
    fn normalize_newlines(&mut self) {
        self.src = self.src.replace("\r\n", "\n").replace('\r', "\n");
    }
}

enum SourceCodeError {
    WrongExtension,
    FileNotExist { path: PathBuf },
}

impl Display for SourceCodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::WrongExtension => {
                write!(f, "Kaba source code file must have '.kaba' extension")
            }
            Self::FileNotExist { path } => {
                write!(f, "file '{}' does not exist", path.display())
            }
        }
    }
}
