use super::{
    error::{ParsingError, ParsingErrorVariant, Result},
    state::ParserState,
};
use crate::{
    ast::{AstNode, TypeNotation},
    lexer::token::TokenKind,
};

pub fn parse(state: &ParserState) -> Result<AstNode> {
    match state.tokens.current_kind() {
        TokenKind::Symbol(name) => {
            let tn = AstNode::TypeNotation {
                tn: TypeNotation::Symbol(name),
                span: state.tokens.current().span.clone(),
            };

            state.tokens.advance();

            Ok(tn)
        }

        TokenKind::LBrack => parse_array_tn(state),
        TokenKind::LParen => parse_function_tn(state),

        _ => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("foo")),
                found: state.tokens.current().kind.clone(),
            },
            span: state.tokens.current().span,
        }),
    }
}

fn parse_array_tn(state: &ParserState) -> Result<AstNode> {
    let start = state.tokens.current().span.start;

    // Expecting "["
    state.tokens.skip(&TokenKind::LBrack)?;

    // Expecting "]"
    state.tokens.skip(&TokenKind::RBrack)?;

    // Parse array element type
    let elem_tn = parse(state)?;

    let end = elem_tn.span().end;

    Ok(AstNode::TypeNotation {
        tn: TypeNotation::Array {
            elem_tn: Box::new(elem_tn),
        },
        span: start..end,
    })
}

fn parse_function_tn(state: &ParserState) -> Result<AstNode> {
    let start = state.tokens.current().span.start;

    // Expecting "("
    state.tokens.skip(&TokenKind::LParen)?;

    // Expecting parameter type notation(s)
    let mut params_tn = vec![];
    while !state.tokens.current_is(&TokenKind::RParen) {
        // Expecting type notation
        let tn = parse(state)?;

        params_tn.push(tn);

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
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

    // Expecting ")"
    state.tokens.skip(&TokenKind::RParen)?;

    // Expecting "->"
    state.tokens.skip(&TokenKind::RightPoint)?;

    // Expecting return type notation
    let return_tn = Box::new(parse(state)?);

    let end = return_tn.span().end;

    Ok(AstNode::TypeNotation {
        tn: TypeNotation::Callable {
            params_tn,
            return_tn,
        },
        span: start..end,
    })
}
