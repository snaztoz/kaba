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

    fn lex_and_assert_result(input: &str, expected: TokenKind) {
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
        lex_and_assert_result(input, TokenKind::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_with_mixed_characters() {
        let input = "_d768a7ABC_adsf";
        lex_and_assert_result(input, TokenKind::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_that_only_a_single_underline() {
        let input = "_";
        lex_and_assert_result(input, TokenKind::Identifier(String::from(input)));
    }

    #[test]
    fn test_lexing_identifier_without_alphabets() {
        let input = "_123";
        lex_and_assert_result(input, TokenKind::Identifier(String::from(input)));
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
        lex_and_assert_result(input, TokenKind::Int(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_zero_literal() {
        let input = "0";
        lex_and_assert_result(input, TokenKind::Int(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_big_integer_literal() {
        let input = "2147483647";
        lex_and_assert_result(input, TokenKind::Int(input.parse().unwrap()));
    }

    //
    // Test float literals
    //

    #[test]
    fn test_lexing_a_float_literal() {
        let input = "123.5";
        lex_and_assert_result(input, TokenKind::Float(input.parse().unwrap()));
    }

    #[test]
    fn test_lexing_a_small_float_literal() {
        let input = "0.0723";
        lex_and_assert_result(input, TokenKind::Float(input.parse().unwrap()));
    }

    //
    // Test char literal
    //

    #[test]
    fn lex_single_character() {
        let input = "'a'";
        lex_and_assert_result(input, TokenKind::Char('a'));
    }

    #[test]
    fn lex_single_digit_character() {
        let input = "'1'";
        lex_and_assert_result(input, TokenKind::Char('1'));
    }

    #[test]
    fn lex_newline_character() {
        let input = "'\\n'";
        lex_and_assert_result(input, TokenKind::Char('\n'));
    }

    #[test]
    fn lex_hex_ascii_character_escape() {
        let input = "'\\x41'";
        lex_and_assert_result(input, TokenKind::Char('A'));
    }

    //
    // String literals
    //

    #[test]
    fn lex_empty_string() {
        let input = r#""""#;
        lex_and_assert_result(input, TokenKind::String(String::from("")));
    }

    #[test]
    fn lex_single_character_string() {
        let input = r#""a""#;
        lex_and_assert_result(input, TokenKind::String(String::from("a")));
    }

    #[test]
    fn lex_multiple_characters_string() {
        let input = r#""abc def ghi 012 @93875252435    ""#;
        lex_and_assert_result(
            input,
            TokenKind::String(String::from("abc def ghi 012 @93875252435    ")),
        );
    }

    #[test]
    fn lex_escape_characters_string() {
        let input = r#""\\\n\t\r\0\'\"""#;
        lex_and_assert_result(input, TokenKind::String(String::from("\\\n\t\r\0\'\"")));
    }

    #[test]
    fn string_hex_character_escape() {
        let input = r#""\x41""#;
        lex_and_assert_result(input, TokenKind::String(String::from("A")));
    }

    //
    // Test comment literals
    //

    fn lex_and_assert_comments_are_skipped(input: &str) {
        let result = lex(input);
        assert!(result.is_ok());

        let tokens = result.unwrap();
        assert!(!tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Comment(_))))
    }

    #[test]
    fn test_lexing_comment_above_code() {
        let input = indoc! {"
            // This is a single line comment
            var x = 5;
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_comment_after_code() {
        let input = indoc! {"
            var x = 10;
            print(x); // this should works too!
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_a_comment_that_commenting_out_a_code() {
        let input = indoc! {"
            // print(y);
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_comment_that_spans_until_eof() {
        let input = indoc! {"
        // A single line comment that spans to EOF"};
        lex_and_assert_comments_are_skipped(input);
    }
}
