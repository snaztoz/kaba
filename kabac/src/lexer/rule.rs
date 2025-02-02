use super::{
    error::{LexingError, LexingErrorVariant, Result},
    token::TokenKind,
};
use logos::Lexer;

pub fn lex_symbol(lex: &mut Lexer<TokenKind>) -> Result<String> {
    let value = lex.slice();

    if value.chars().next().unwrap().is_numeric() {
        return Err(LexingError {
            variant: LexingErrorVariant::InvalidSymbol,
            span: lex.span(),
        });
    }

    Ok(String::from(value))
}

pub fn lex_integer(lex: &mut Lexer<TokenKind>) -> u32 {
    lex.slice().parse().unwrap()
}

pub fn lex_float(lex: &mut Lexer<TokenKind>) -> f32 {
    lex.slice().parse().unwrap()
}

pub fn lex_bool(lex: &mut Lexer<TokenKind>) -> bool {
    match lex.slice() {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    }
}

pub fn lex_char(lex: &mut Lexer<TokenKind>) -> Result<char> {
    let c = match lex.remainder().chars().next() {
        Some(c) => c,
        None => {
            return Err(LexingError {
                variant: LexingErrorVariant::UnexpectedEof,
                span: lex.span().end..lex.span().end,
            })
        }
    };

    lex.bump(c.len_utf8());

    let c = match c {
        '\\' => lex_escape_character(lex)?,
        '\'' => {
            return Err(LexingError {
                variant: LexingErrorVariant::UnexpectedToken,
                span: lex.span().end - 1..lex.span().end,
            })
        }
        _ => c,
    };

    match lex.remainder().chars().next() {
        Some('\'') => (),
        Some(_) => {
            return Err(LexingError {
                variant: LexingErrorVariant::UnexpectedToken,
                span: lex.span().end..lex.span().end,
            })
        }
        None => {
            return Err(LexingError {
                variant: LexingErrorVariant::UnexpectedEof,
                span: lex.span().end..lex.span().end,
            })
        }
    };

    lex.bump(c.len_utf8());

    Ok(c)
}

pub fn lex_string(lex: &mut Lexer<TokenKind>) -> Result<String> {
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

    Err(LexingError {
        variant: LexingErrorVariant::UnexpectedEof,
        span: lex.source().len()..lex.source().len(),
    })
}

pub fn lex_escape_character(lex: &mut Lexer<TokenKind>) -> Result<char> {
    let c = match lex.remainder().chars().next() {
        Some(c) => c,
        None => {
            return Err(LexingError {
                variant: LexingErrorVariant::UnexpectedEof,
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
            return Err(LexingError {
                variant: LexingErrorVariant::UnsupportedEscapeCharacter,
                span: lex.span().end - 2..lex.span().end,
            })
        }
    };

    Ok(c)
}

pub fn lex_hex(lex: &mut Lexer<TokenKind>) -> Result<char> {
    let mut buff = String::new();

    for _ in 0..2 {
        let c = match lex.remainder().chars().next() {
            Some(c) => c,
            None => {
                return Err(LexingError {
                    variant: LexingErrorVariant::UnexpectedEof,
                    span: lex.span().end..lex.span().end,
                })
            }
        };

        lex.bump(c.len_utf8());
        buff.push(c)
    }

    u8::from_str_radix(&buff, 16)
        .map(|b| b as char)
        .map_err(|_| LexingError {
            variant: LexingErrorVariant::InvalidHexNumber,
            span: lex.span().end - 2..lex.span().end,
        })
}

pub fn lex_comment(lex: &mut Lexer<TokenKind>) -> String {
    let remainder = lex.remainder();
    if let Some(newline_index) = remainder.find('\n') {
        lex.bump(newline_index + 1);
        String::from(&remainder[..newline_index])
    } else {
        lex.bump(remainder.bytes().len());
        String::from(remainder)
    }
}
