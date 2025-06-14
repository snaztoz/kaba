use super::{
    error::{LexingError, LexingErrorVariant, Result},
    token::TokenKind,
};
use logos::Lexer;

pub fn lex_symbol<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> Result<&'src str> {
    let value = lex.slice();

    if value.chars().next().unwrap().is_numeric() {
        return Err(LexingError {
            variant: LexingErrorVariant::InvalidSymbol,
            span: lex.span(),
        });
    }

    Ok(value)
}

pub fn lex_integer<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> u32 {
    lex.slice().parse().unwrap()
}

pub fn lex_float<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> f32 {
    lex.slice().parse().unwrap()
}

pub fn lex_bool<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> bool {
    match lex.slice() {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    }
}

pub fn lex_char<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> Result<char> {
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

pub fn lex_string<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> Result<String> {
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

pub fn lex_escape_character<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> Result<char> {
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

pub fn lex_hex<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> Result<char> {
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

pub fn lex_comment<'src>(lex: &mut Lexer<'src, TokenKind<'src>>) -> &'src str {
    let remainder = lex.remainder();
    if let Some(newline_index) = remainder.find('\n') {
        lex.bump(newline_index + 1);
        &remainder[..newline_index]
    } else {
        lex.bump(remainder.bytes().len());
        remainder
    }
}
