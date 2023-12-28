// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the required logic operations during the
//! lexing/tokenizing stage of a Kaba source code.

use crate::error::ErrorVariant;
use logos::{Lexer, Logos, Span};
use std::fmt::Display;

/// Provide a quick way to lex a Kaba program's source code, without the
/// needs to setting up and running the lexer manually.
///
/// Produces a vector of [`RichToken`] that contains additional
/// information of a token.
pub fn lex(program: &str) -> Result<Vec<RichToken>, ErrorVariant> {
    let mut l = Token::lexer(program);
    let mut tokens = vec![];

    while let Some(token) = l.next() {
        let token = token.map_err(|e| match e {
            ErrorVariant::Error => ErrorVariant::LexingUnknownToken {
                token: String::from(l.slice()),
                span: l.span(),
            },
            _ => e,
        });

        tokens.push(RichToken {
            kind: token?,
            range: l.span(),
            value: String::from(l.slice()),
        })
    }

    Ok(tokens)
}

/// A wrapper around raw [`Token`] that also store the metadata
/// information of a token, such as its actual position inside
/// the source code.
#[derive(Clone, Debug, PartialEq)]
pub struct RichToken {
    pub kind: Token,
    pub range: Span,
    pub value: String,
}

/// The list of all tokens that may exists in a valid Kaba
/// source code.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = ErrorVariant)]
#[rustfmt::skip]
pub enum Token {
    #[regex("[a-zA-Z0-9_]+", identifier)]
    Identifier(String),

    //
    // Literals
    //

    #[regex("[0-9]+", priority = 2, callback = integer)]
    Integer(i32),

    #[regex(r"[0-9]+\.[0-9]+", callback = float)]
    Float(f64),

    //
    // Keywords
    //

    #[token("var")] Var,

    //
    // Symbols
    //

    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("*")] Mul,
    #[token("/")] Div,

    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token(",")] Comma,
    #[token("=")] Assign,
    #[token("(")] LParen,
    #[token(")")] RParen,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(_) => write!(f, "identifier"),
            Self::Integer(_) => write!(f, "integer"),
            Self::Float(_) => write!(f, "float"),
            Self::Var => write!(f, "`var` keyword"),
            Self::Add => write!(f, "addition operator (`+`)"),
            Self::Sub => write!(f, "subtraction operator (`-`)"),
            Self::Mul => write!(f, "multiplication operator (`*`)"),
            Self::Div => write!(f, "division operator (`/`)"),
            Self::Colon => write!(f, "colon (`:`)"),
            Self::Semicolon => write!(f, "semicolon (`;`)"),
            Self::Comma => write!(f, "comma (`,`)"),
            Self::Assign => write!(f, "assignment (`=`)"),
            Self::LParen => write!(f, "left parentheses (`(`)"),
            Self::RParen => write!(f, "right parentheses (`)`)"),
        }
    }
}

fn identifier(lex: &mut Lexer<Token>) -> Result<String, ErrorVariant> {
    let value = lex.slice();
    if value.chars().next().unwrap().is_numeric() {
        return Err(ErrorVariant::LexingIdentifierStartsWithNumber {
            token: String::from(value),
            span: lex.span(),
        });
    }
    Ok(String::from(value))
}

fn integer(lex: &mut Lexer<Token>) -> i32 {
    lex.slice().parse().unwrap()
}

fn float(lex: &mut Lexer<Token>) -> f64 {
    lex.slice().parse().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing_identifier() {
        let cases = ["abc", "_d768a7ABC_adsf", "_", "_123"];

        for c in cases {
            let lex_result = lex(c);

            assert!(lex_result.is_ok());

            let tokens = lex_result.unwrap();

            assert!(tokens.len() == 1);
            assert_eq!(tokens[0].kind, Token::Identifier(String::from(c)));
        }
    }

    #[test]
    fn test_lexing_invalid_identifier() {
        let cases = ["123abc"];

        for c in cases {
            let lex_result = lex(c);

            assert!(lex_result.is_err());
        }
    }

    #[test]
    fn test_lexing_integer() {
        let cases = ["123", "0"];

        for c in cases {
            let lex_result = lex(c);

            assert!(lex_result.is_ok());

            let tokens = lex_result.unwrap();

            assert!(tokens.len() == 1);
            assert_eq!(tokens[0].kind, Token::Integer(c.parse().unwrap()));
        }
    }

    #[test]
    fn test_lexing_float() {
        let cases = ["123.5", "0.0723"];

        for c in cases {
            let lex_result = lex(c);

            assert!(lex_result.is_ok());

            let tokens = lex_result.unwrap();

            assert!(tokens.len() == 1);
            assert_eq!(tokens[0].kind, Token::Float(c.parse().unwrap()));
        }
    }
}
