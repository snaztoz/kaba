// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

use std::fmt::Display;

use logos::{Lexer, Logos, Span};

pub fn lex(program: &str) -> Result<Vec<RichToken>, LexerError> {
    let mut l = Token::lexer(program);
    let mut tokens = vec![];

    while let Some(t) = l.next() {
        tokens.push(RichToken {
            kind: t?,
            range: l.span(),
            value: String::from(l.slice()),
        })
    }

    Ok(tokens)
}

#[derive(Clone, Debug, PartialEq)]
pub struct RichToken {
    pub kind: Token,
    pub range: Span,
    pub value: String,
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = LexerError)]
#[rustfmt::skip]
pub enum Token {
    #[regex("[a-zA-Z0-9_]+", identifier)]
    Identifier(String),

    //
    // Literals
    //

    #[regex("[0-9]+", priority = 2, callback = integer)]
    Integer(i64),

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
            Self::Identifier(name) => write!(f, "identifier `{name}`"),
            Self::Integer(_) => write!(f, "integer"),
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

fn identifier(lex: &mut Lexer<Token>) -> Result<String, LexerError> {
    let value = lex.slice();
    if value.chars().next().unwrap().is_numeric() {
        return Err(LexerError::IdentifierStartsWithNumber);
    }
    Ok(String::from(value))
}

fn integer(lex: &mut Lexer<Token>) -> i64 {
    lex.slice().parse().unwrap()
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LexerError {
    IdentifierStartsWithNumber,

    #[default]
    Default,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentifierStartsWithNumber => {
                writeln!(f, "Identifier can't start with number")
            }
            _ => unreachable!(),
        }
    }
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
}