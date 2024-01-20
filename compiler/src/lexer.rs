// Copyright 2023 Hafidh Muqsithanova Sukarno
// SPDX-License-Identifier: Apache-2.0

//! This module contains the required logic operations during the
//! lexing/tokenizing stage of a Kaba source code.

use logos::{Lexer, Logos, Span};
use std::fmt::Display;

/// Provide a quick way to lex a Kaba program's source code, without the
/// needs to setting up and running the lexer manually.
///
/// Produces a vector of [`RichToken`] that contains additional
/// information of a token.
pub fn lex(source_code: &str) -> Result<Vec<RichToken>, LexingError> {
    let mut l = Token::lexer(source_code);
    let mut tokens = vec![];

    while let Some(token) = l.next() {
        let kind = token.map_err(|e| match e {
            LexingError::Default => LexingError::UnknownToken {
                token: String::from(l.slice()),
                span: l.span(),
            },
            _ => e,
        })?;

        if kind.is_comment() {
            continue;
        }

        tokens.push(RichToken {
            kind,
            span: l.span(),
        })
    }

    tokens.push(RichToken {
        kind: Token::Eof,
        span: source_code.len()..source_code.len(),
    });

    Ok(tokens)
}

/// A wrapper around raw [`Token`] that also store the metadata
/// information of a token, such as its actual position inside
/// the source code.
#[derive(Clone, Debug, PartialEq)]
pub struct RichToken {
    pub kind: Token,
    pub span: Span,
}

/// The list of all tokens that may exists in a valid Kaba
/// source code.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = LexingError)]
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

    #[token("true")]
    BooleanTrue,

    #[token("false")]
    BooleanFalse,

    //
    // Keywords
    //

    #[token("var")]      Var,
    #[token("if")]       If,
    #[token("else")]     Else,
    #[token("while")]    While,
    #[token("break")]    Break,
    #[token("continue")] Continue,
    #[token("fn")]       Fn,
    #[token("return")]   Return,

    //
    // Symbols
    //

    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("*")] Mul,
    #[token("/")] Div,
    #[token("%")] Mod,

    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token(",")] Comma,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LBrace,
    #[token("}")] RBrace,

    #[token("=")]  Assign,
    #[token("+=")] AddAssign,
    #[token("-=")] SubAssign,
    #[token("*=")] MulAssign,
    #[token("/=")] DivAssign,
    #[token("%=")] ModAssign,

    #[token("==")] Eq,
    #[token("!=")] Neq,
    #[token(">")]  Gt,
    #[token(">=")] Gte,
    #[token("<")]  Lt,
    #[token("<=")] Lte,

    #[token("||")] Or,
    #[token("&&")] And,
    #[token("!")]  Not,

    // Comments

    #[token("//", callback = single_line_comment)]
    SingleLineComment(String),

    #[token("/*", callback = multi_line_comment)]
    MultiLineComment(String),

    // This will always be appended as the last token
    // inside token list
    Eof,
}

impl Token {
    fn is_comment(&self) -> bool {
        matches!(self, Token::SingleLineComment(_)) || matches!(self, Token::MultiLineComment(_))
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(_) => write!(f, "identifier"),
            Self::Integer(_) => write!(f, "integer"),
            Self::Float(_) => write!(f, "float"),
            Self::BooleanTrue => write!(f, "true"),
            Self::BooleanFalse => write!(f, "false"),

            Self::Var => write!(f, "`var` keyword"),
            Self::If => write!(f, "`if` keyword"),
            Self::Else => write!(f, "`else` keyword"),
            Self::While => write!(f, "`while` keyword"),
            Self::Break => write!(f, "`break` keyword"),
            Self::Continue => write!(f, "`continue` keyword"),
            Self::Fn => write!(f, "`fn` keyword"),
            Self::Return => write!(f, "`return` keyword"),

            Self::Add => write!(f, "addition operator (`+`)"),
            Self::Sub => write!(f, "subtraction operator (`-`)"),
            Self::Mul => write!(f, "multiplication operator (`*`)"),
            Self::Div => write!(f, "division operator (`/`)"),
            Self::Mod => write!(f, "modulo operator (`%`)"),
            Self::Colon => write!(f, "colon (`:`)"),
            Self::Semicolon => write!(f, "semicolon (`;`)"),
            Self::Comma => write!(f, "comma (`,`)"),
            Self::LParen => write!(f, "left parentheses (`(`)"),
            Self::RParen => write!(f, "right parentheses (`)`)"),
            Self::LBrace => write!(f, "left brace (`{{`)"),
            Self::RBrace => write!(f, "right brace (`}}`)"),
            Self::Assign => write!(f, "assign operator (`=`)"),
            Self::AddAssign => write!(f, "add assign operator (`+=`)"),
            Self::SubAssign => write!(f, "sub assign operator (`-=`)"),
            Self::MulAssign => write!(f, "mul assign operator (`*=`)"),
            Self::DivAssign => write!(f, "div assign operator (`/=`)"),
            Self::ModAssign => write!(f, "mod assign operator (`%=`)"),
            Self::Eq => write!(f, "equal operator (`==`)"),
            Self::Neq => write!(f, "not equal operator (`!=`)"),
            Self::Gt => write!(f, "greater than operator (`>`)"),
            Self::Gte => write!(f, "greater than or equal operator (`>=`)"),
            Self::Lt => write!(f, "less than operator (`<`)"),
            Self::Lte => write!(f, "less than or equal operator (`<=`)"),
            Self::Or => write!(f, "logical or operator (`||`)"),
            Self::And => write!(f, "logical and operator (`&&`)"),
            Self::Not => write!(f, "logical not operator (`!`)"),

            Self::SingleLineComment(_) => write!(f, "single line comment"),
            Self::MultiLineComment(_) => write!(f, "multi line comment"),

            Self::Eof => write!(f, "end-of-file (EOF)"),
        }
    }
}

fn identifier(lex: &mut Lexer<Token>) -> Result<String, LexingError> {
    let value = lex.slice();
    if value.chars().next().unwrap().is_numeric() {
        return Err(LexingError::IdentifierStartsWithNumber {
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

fn single_line_comment(lex: &mut Lexer<Token>) -> String {
    let remainder = lex.remainder();
    if let Some(newline_index) = remainder.find('\n') {
        lex.bump(newline_index + 1);
        String::from(&remainder[..newline_index])
    } else {
        lex.bump(remainder.bytes().len());
        String::from(remainder)
    }
}

fn multi_line_comment(lex: &mut Lexer<Token>) -> Result<String, LexingError> {
    let remainder = lex.remainder();
    if let Some(comment_closing_index) = remainder.find("*/") {
        lex.bump(comment_closing_index + 2);
        Ok(String::from(&remainder[..comment_closing_index]))
    } else {
        let source_len = lex.source().len();
        Err(LexingError::MissingCommentClosingTag {
            span: source_len..source_len,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LexingError {
    IdentifierStartsWithNumber {
        token: String,
        span: Span,
    },
    MissingCommentClosingTag {
        span: Span,
    },
    UnknownToken {
        token: String,
        span: Span,
    },

    #[default]
    Default,
}

impl LexingError {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Self::IdentifierStartsWithNumber { span, .. } => Some(span.clone()),
            Self::MissingCommentClosingTag { span } => Some(span.clone()),
            Self::UnknownToken { span, .. } => Some(span.clone()),
            _ => None,
        }
    }
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentifierStartsWithNumber { token, .. } => {
                write!(f, "identifier can't start with number: `{token}`")
            }
            Self::MissingCommentClosingTag { .. } => {
                write!(f, "missing comment closing tag (`*/`)")
            }
            Self::UnknownToken { token, .. } => {
                write!(f, "unknown token `{token}`")
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn lex_and_assert_result(input: &str, expected: Token) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(tokens.len() == 2);
        assert_eq!(tokens[0].kind, expected);
    }

    fn lex_and_assert_err(input: &str) {
        let result = lex(input);
        assert!(result.is_err());
    }

    //
    // Test identifiers
    //

    #[test]
    fn test_lexing_normal_identifier() {
        let input = "abc";
        lex_and_assert_result(input, Token::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_with_mixed_characters() {
        let input = "_d768a7ABC_adsf";
        lex_and_assert_result(input, Token::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_that_only_a_single_underline() {
        let input = "_";
        lex_and_assert_result(input, Token::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_without_alphabets() {
        let input = "_123";
        lex_and_assert_result(input, Token::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_that_starts_with_number() {
        let input = "123abc";
        lex_and_assert_err(input);
    }

    //
    // Test integer literals
    //

    #[test]
    fn test_lexing_an_integer_literal() {
        let input = "123";
        lex_and_assert_result(input, Token::Integer(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_zero_literal() {
        let input = "0";
        lex_and_assert_result(input, Token::Integer(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_big_integer_literal() {
        let input = "2147483647";
        lex_and_assert_result(input, Token::Integer(input.parse().unwrap()));
    }

    //
    // Test float literals
    //

    #[test]
    fn test_lexing_a_float_literal() {
        let input = "123.5";
        lex_and_assert_result(input, Token::Float(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_small_float_literal() {
        let input = "0.0723";
        lex_and_assert_result(input, Token::Float(input.parse().unwrap()));
    }

    //
    // Test comment literals
    //

    fn lex_and_assert_contains_single_line_comment(input: &str) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(!tokens
            .iter()
            .any(|t| matches!(t.kind, Token::SingleLineComment(_))))
    }

    fn lex_and_assert_contains_multi_line_comment(input: &str) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(!tokens
            .iter()
            .any(|t| matches!(t.kind, Token::MultiLineComment(_))))
    }

    #[test]
    fn test_lexing_a_single_line_comment_above_code() {
        let input = indoc! {"
            // This is a single line comment
            var x = 5;
        "};
        lex_and_assert_contains_single_line_comment(input);
    }

    #[test]
    fn test_lexing_a_single_line_comment_after_code() {
        let input = indoc! {"
            var x = 10;
            print(x); // this should works too!
        "};
        lex_and_assert_contains_single_line_comment(input);
    }

    #[test]
    fn test_lexing_a_single_line_comment_that_commenting_out_a_code() {
        let input = indoc! {"
            // print(y);
        "};
        lex_and_assert_contains_single_line_comment(input);
    }

    #[test]
    fn test_lexing_a_single_line_comment_spans_until_eof() {
        let input = indoc! {"
        // A single line comment that spans to EOF"};
        lex_and_assert_contains_single_line_comment(input);
    }

    #[test]
    fn test_lexing_a_multi_line_comment_above_code() {
        let input = indoc! {"
            /**
             * This is a multi line comment
             */
            var x = 5;
        "};
        lex_and_assert_contains_multi_line_comment(input);
    }

    #[test]
    fn test_lexing_a_multi_line_comment_before_code() {
        let input = indoc! {"
            /* no problem */ var x = 5;
        "};
        lex_and_assert_contains_multi_line_comment(input);
    }

    #[test]
    fn test_lexing_a_multi_line_comment_after_code() {
        let input = indoc! {"
            var x = 5; /* also no problem */
        "};
        lex_and_assert_contains_multi_line_comment(input);
    }

    #[test]
    fn test_lexing_a_multi_line_comment_that_commenting_out_a_code() {
        let input = indoc! {"
            /**
             * All good
             */

            /**
             * print(y);
             *       ^
             *      A non existing variable
             */
        "};
        lex_and_assert_contains_single_line_comment(input);
    }

    #[test]
    fn test_lexing_a_multi_line_comment_that_has_no_closing_tag() {
        let input = indoc! {"
            /* No closing tag
            print(0);
        "};
        assert!(lex(input).is_err());
    }
}
