//! This module contains the required logic operations during the tokenizing
//! stage of a Kaba source code.

use error::{LexingError, Result};
use logos::Logos;
use token::{Token, TokenKind};

mod error;
mod rule;
pub mod token;

/// Provide a quick way to lex a Kaba program's source code, without the needs
/// to setting up and running the lexer manually.
///
/// Produces a vector of [`Token`] that contains additional information of a
/// token.
pub fn lex(src: &str) -> Result<Vec<Token>> {
    let mut l = TokenKind::lexer(src);
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

        tokens.push(Token {
            kind,
            span: l.span(),
        })
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        span: src.len()..src.len(),
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn assert_token_kind(input: &str, expected: TokenKind) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(tokens.len() == 2);
        assert_eq!(tokens[0].kind, expected);
    }

    fn assert_is_err(input: &str) {
        let result = lex(input);
        assert!(result.is_err());
    }

    //
    // Symbols
    //

    #[test]
    fn normal_symbol() {
        let input = "abc";
        assert_token_kind(input, TokenKind::Symbol(String::from(input)));
    }

    #[test]
    fn symbol_with_mixed_characters() {
        let input = "_d768a7ABC_adsf";
        assert_token_kind(input, TokenKind::Symbol(String::from(input)));
    }

    #[test]
    fn symbol_that_consists_of_single_underline() {
        let input = "_";
        assert_token_kind(input, TokenKind::Symbol(String::from(input)));
    }

    #[test]
    fn symbol_without_alphabets() {
        let input = "_123";
        assert_token_kind(input, TokenKind::Symbol(String::from(input)));
    }

    #[test]
    fn symbol_that_starts_with_number() {
        let input = "123abc";
        assert_is_err(input);
    }

    //
    // Integer literals
    //

    #[test]
    fn integer_literal() {
        let input = "123";
        assert_token_kind(input, TokenKind::Int(input.parse().unwrap()));
    }

    #[test]
    fn zero_literal() {
        let input = "0";
        assert_token_kind(input, TokenKind::Int(input.parse().unwrap()));
    }

    #[test]
    fn big_integer_literal() {
        let input = "2147483647";
        assert_token_kind(input, TokenKind::Int(input.parse().unwrap()));
    }

    //
    // Float literals
    //

    #[test]
    fn float_literal() {
        let input = "123.5";
        assert_token_kind(input, TokenKind::Float(input.parse().unwrap()));
    }

    #[test]
    fn small_float_literal() {
        let input = "0.0723";
        assert_token_kind(input, TokenKind::Float(input.parse().unwrap()));
    }

    //
    // Char literal
    //

    #[test]
    fn single_character() {
        let input = "'a'";
        assert_token_kind(input, TokenKind::Char('a'));
    }

    #[test]
    fn single_digit_character() {
        let input = "'1'";
        assert_token_kind(input, TokenKind::Char('1'));
    }

    #[test]
    fn newline_character() {
        let input = "'\\n'";
        assert_token_kind(input, TokenKind::Char('\n'));
    }

    #[test]
    fn hex_ascii_character_escape() {
        let input = "'\\x41'";
        assert_token_kind(input, TokenKind::Char('A'));
    }

    //
    // String literals
    //

    #[test]
    fn empty_string() {
        let input = r#""""#;
        assert_token_kind(input, TokenKind::String(String::from("")));
    }

    #[test]
    fn single_character_string() {
        let input = r#""a""#;
        assert_token_kind(input, TokenKind::String(String::from("a")));
    }

    #[test]
    fn multiple_characters_string() {
        let input = r#""abc def ghi 012 @93875252435    ""#;
        assert_token_kind(
            input,
            TokenKind::String(String::from("abc def ghi 012 @93875252435    ")),
        );
    }

    #[test]
    fn escape_characters_string() {
        let input = r#""\\\n\t\r\0\'\"""#;
        assert_token_kind(input, TokenKind::String(String::from("\\\n\t\r\0\'\"")));
    }

    #[test]
    fn string_with_hex_character_escape() {
        let input = r#""\x41""#;
        assert_token_kind(input, TokenKind::String(String::from("A")));
    }

    //
    // Comment literals
    //

    fn assert_comments_are_skipped(input: &str) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(!tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Comment(_))))
    }

    #[test]
    fn comment_above_code() {
        let input = indoc! {"
            // This is a single line comment
            var x = 5;
        "};
        assert_comments_are_skipped(input);
    }

    #[test]
    fn comment_after_code() {
        let input = indoc! {"
            var x = 10;
            print(x); // this should works too!
        "};
        assert_comments_are_skipped(input);
    }

    #[test]
    fn commenting_out_a_code() {
        let input = indoc! {"
            // print(y);
        "};
        assert_comments_are_skipped(input);
    }

    #[test]
    fn comment_that_spans_until_eof() {
        let input = indoc! {"
        // A single line comment that spans to EOF"};
        assert_comments_are_skipped(input);
    }
}
