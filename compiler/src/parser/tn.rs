use super::{
    error::{ParsingError, ParsingErrorVariant, Result},
    state::ParserState,
};
use crate::{
    ast::{AstNode, AstNodeVariant, TypeNotation},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    match state.tokens.current_kind() {
        TokenKind::Symbol(name) => {
            let tn = AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::TypeNotation {
                    tn: TypeNotation::Symbol(name),
                },
                span: state.tokens.current().span.clone(),
            };

            state.tokens.advance();

            Ok(tn)
        }

        TokenKind::LBrack => parse_array_tn(state),
        TokenKind::LParen => parse_function_tn(state),

        _ => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Symbol("type name"),
                found: state.tokens.current().kind.clone(),
            },
            span: state.tokens.current().span,
        }),
    }
}

fn parse_array_tn<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    state.tokens.expect(&TokenKind::LBrack)?;

    state.tokens.expect(&TokenKind::RBrack)?;

    let elem_tn = parse(state)?;

    let end = elem_tn.span.end;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::TypeNotation {
            tn: TypeNotation::Array {
                elem_tn: Box::new(elem_tn),
            },
        },
        span: start..end,
    })
}

fn parse_function_tn<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    state.tokens.expect(&TokenKind::LParen)?;

    let mut params_tn = vec![];
    while !state.tokens.current_is(&TokenKind::RParen) {
        let tn = parse(state)?;

        params_tn.push(tn);

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.expect(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RParen => continue,

            kind => {
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }

    state.tokens.expect(&TokenKind::RParen)?;

    state.tokens.expect(&TokenKind::RightPoint)?;

    let return_tn = Box::new(parse(state)?);

    let end = return_tn.span.end;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::TypeNotation {
            tn: TypeNotation::Callable {
                params_tn,
                return_tn,
            },
        },
        span: start..end,
    })
}
