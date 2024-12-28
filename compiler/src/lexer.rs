//! This module contains the required logic operations during the tokenizing
//! stage of a Kaba source code.

use logos::{Lexer, Logos, Span};
use std::fmt::Display;

type Result<T> = std::result::Result<T, LexingError>;

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

/// A wrapper around raw [`TokenKind`] that also store the metadata information
/// of a token, such as its actual position inside the source code.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// The list of all token kinds that may exists in a valid Kaba source code.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = LexingError)]
#[rustfmt::skip]
pub enum TokenKind {
    #[regex("[a-zA-Z0-9_]+", lex_identifier)]
    Identifier(String),

    //
    // Literals
    //

    #[regex("[0-9]+", priority = 2, callback = lex_integer)]
    Int(i32),

    #[regex(r"[0-9]+\.[0-9]+", callback = lex_float)]
    Float(f32),

    #[regex(r"true|false", callback = lex_bool)]
    Bool(bool),

    #[regex(r#"'(?:\\['"\\nrt0]|\\x[0-9a-fA-F]{2}|[^'\\])'"#, callback = lex_char)]
    Char(char),

    #[token("\"", callback = lex_string)]
    String(String),

    //
    // Keywords
    //

    #[token("var")]      Var,
    #[token("if")]       If,
    #[token("else")]     Else,
    #[token("while")]    While,
    #[token("each")]     Each,
    #[token("break")]    Break,
    #[token("continue")] Continue,
    #[token("in")]       In,
    #[token("fn")]       Fn,
    #[token("return")]   Return,
    #[token("do")]       Do,
    #[token("end")]      End,
    #[token("as")]       As,
    #[token("debug")]    Debug,

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
    #[token("[")] LBrack,
    #[token("]")] RBrack,
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

    #[token("->")] RightPoint,

    // Comments

    #[token("#", callback = lex_comment)]
    Comment(String),

    // This will always be appended as the last token
    // inside token list
    Eof,
}

impl TokenKind {
    const fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(_) => write!(f, "identifier"),
            Self::Int(_) => write!(f, "integer literal"),
            Self::Float(_) => write!(f, "float literal"),
            Self::Bool(b) => write!(f, "boolean `{b}` literal"),
            Self::Char(_) => write!(f, "char literal"),
            Self::String(_) => write!(f, "string literal"),

            Self::Var => write!(f, "`var` keyword"),
            Self::If => write!(f, "`if` keyword"),
            Self::Else => write!(f, "`else` keyword"),
            Self::While => write!(f, "`while` keyword"),
            Self::Each => write!(f, "`each` keyword"),
            Self::Break => write!(f, "`break` keyword"),
            Self::Continue => write!(f, "`continue` keyword"),
            Self::In => write!(f, "`in` keyword"),
            Self::Fn => write!(f, "`fn` keyword"),
            Self::Return => write!(f, "`return` keyword"),
            Self::Do => write!(f, "`do` keyword"),
            Self::End => write!(f, "`end` keyword"),
            Self::As => write!(f, "`as` keyword"),
            Self::Debug => write!(f, "`debug` keyword"),

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
            Self::LBrack => write!(f, "left bracket (`[`)"),
            Self::RBrack => write!(f, "right bracket (`]`)"),
            Self::LBrace => write!(f, "left brace (`{{`)"),
            Self::RBrace => write!(f, "right bracket (`}}`)"),
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

            Self::RightPoint => write!(f, "right pointing operator (`->`)"),

            Self::Comment(_) => write!(f, "comment"),

            Self::Eof => write!(f, "end-of-file (EOF)"),
        }
    }
}

fn lex_identifier(lex: &mut Lexer<TokenKind>) -> Result<String> {
    let value = lex.slice();

    if value.chars().next().unwrap().is_numeric() {
        return Err(LexingError::InvalidIdentifier {
            token: String::from(value),
            span: lex.span(),
        });
    }

    Ok(String::from(value))
}

fn lex_integer(lex: &mut Lexer<TokenKind>) -> i32 {
    lex.slice().parse().unwrap()
}

fn lex_float(lex: &mut Lexer<TokenKind>) -> f32 {
    lex.slice().parse().unwrap()
}

fn lex_bool(lex: &mut Lexer<TokenKind>) -> bool {
    match lex.slice() {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    }
}

fn lex_char(lex: &mut Lexer<TokenKind>) -> char {
    let slice = lex.slice();
    let char_slice = &slice[1..slice.len() - 1];

    if char_slice.len() == 1 {
        return char_slice.parse().unwrap();
    }

    if char_slice.len() == 2 {
        return match char_slice {
            "\\'" => '\'',
            "\\\"" => '\"',
            "\\n" => '\n',
            "\\r" => '\r',
            "\\t" => '\t',
            "\\0" => '\0',
            _ => unimplemented!("other character escape"),
        };
    }

    if let Some(hex) = char_slice.strip_prefix("\\x") {
        if let Ok(value) = u8::from_str_radix(hex, 16) {
            return value as char;
        } else {
            panic!("Invalid hexadecimal value.");
        }
    }

    unreachable!()
}

fn lex_string(lex: &mut Lexer<TokenKind>) -> Result<String> {
    let mut buff = String::new();

    while let Some(c) = lex.remainder().chars().next() {
        lex.bump(c.len_utf8());
        match c {
            '\\' => {
                let c = lex_escape_character(lex)?;
                buff.push(c);
            }
            '\"' => {
                return Ok(buff);
            }
            _ => buff.push(c),
        }
    }

    Err(LexingError::UnexpectedEof {
        span: lex.source().len()..lex.source().len(),
    })
}

fn lex_escape_character(lex: &mut Lexer<TokenKind>) -> Result<char> {
    let c = match lex.remainder().chars().next() {
        Some(c) => c,
        None => {
            return Err(LexingError::UnexpectedEof {
                span: lex.span().end..lex.span().end,
            })
        }
    };

    lex.bump(c.len_utf8());

    let c = match c {
        '\\' | '\'' | '\"' => c,
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        'x' => lex_hex(lex)?,

        _ => {
            return Err(LexingError::UnsupportedEscapeCharacter {
                c,
                span: lex.span().end - 2..lex.span().end,
            })
        }
    };

    Ok(c)
}

fn lex_hex(lex: &mut Lexer<TokenKind>) -> Result<char> {
    let mut buff = String::new();

    for _ in 0..2 {
        let c = match lex.remainder().chars().next() {
            Some(c) => c,
            None => {
                return Err(LexingError::UnexpectedEof {
                    span: lex.span().end..lex.span().end,
                })
            }
        };

        lex.bump(c.len_utf8());
        buff.push(c)
    }

    u8::from_str_radix(&buff, 16)
        .map(|b| b as char)
        .map_err(|_| LexingError::InvalidHexNumber {
            n: buff,
            span: lex.span().end - 2..lex.span().end,
        })
}

fn lex_comment(lex: &mut Lexer<TokenKind>) -> String {
    let remainder = lex.remainder();
    if let Some(newline_index) = remainder.find('\n') {
        lex.bump(newline_index + 1);
        String::from(&remainder[..newline_index])
    } else {
        lex.bump(remainder.bytes().len());
        String::from(remainder)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LexingError {
    InvalidIdentifier {
        token: String,
        span: Span,
    },

    UnexpectedEof {
        span: Span,
    },

    UnknownToken {
        token: String,
        span: Span,
    },

    UnsupportedEscapeCharacter {
        c: char,
        span: Span,
    },

    InvalidHexNumber {
        n: String,
        span: Span,
    },

    #[default]
    Default,
}

impl LexingError {
    pub fn span(&self) -> Span {
        match self {
            Self::InvalidIdentifier { span, .. }
            | Self::UnexpectedEof { span, .. }
            | Self::UnknownToken { span, .. }
            | Self::UnsupportedEscapeCharacter { span, .. }
            | Self::InvalidHexNumber { span, .. } => span.clone(),

            _ => unreachable!(),
        }
    }
}

impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidIdentifier { token, .. } => {
                write!(f, "not a valid identifier: `{token}`")
            }
            Self::UnexpectedEof { .. } => {
                write!(f, "not expecting an end-of-file (EOF)")
            }
            Self::UnknownToken { token, .. } => {
                write!(f, "unknown token: `{token}`")
            }
            Self::UnsupportedEscapeCharacter { c, .. } => {
                write!(f, "unsupported escape character: `\\{c}`")
            }
            Self::InvalidHexNumber { n, .. } => {
                write!(f, "not a valid hex number: `{n}`")
            }
            _ => unreachable!(),
        }
    }
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
            # This is a single line comment
            var x = 5;
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_comment_after_code() {
        let input = indoc! {"
            var x = 10;
            print(x); # this should works too!
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_a_comment_that_commenting_out_a_code() {
        let input = indoc! {"
            # print(y);
        "};
        lex_and_assert_comments_are_skipped(input);
    }

    #[test]
    fn test_lexing_comment_that_spans_until_eof() {
        let input = indoc! {"
        # A single line comment that spans to EOF"};
        lex_and_assert_comments_are_skipped(input);
    }
}
