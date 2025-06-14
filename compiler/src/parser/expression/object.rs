use crate::parser::{
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    tn, Result,
};
use crate::{
    ast::{AstNode, AstNodeVariant, ObjectInitializer},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    state.tokens.expect(&TokenKind::New)?;

    let object_tn = tn::parse(state)?;

    state.tokens.expect(&TokenKind::LBrace)?;

    if state.tokens.current_is(&TokenKind::RBrace) {
        let end = state.tokens.current().span.end;

        state.tokens.expect(&TokenKind::RBrace)?;

        return Ok(AstNode {
            id: state.next_id(),
            variant: AstNodeVariant::ObjectCreation {
                tn: Box::new(object_tn),
                initializer: ObjectInitializer::Empty,
            },
            span: start..end,
        });
    }

    let initializer = parse_initializer(state)?;

    let end = state.tokens.current().span.end;

    state.tokens.expect(&TokenKind::RBrace)?;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::ObjectCreation {
            tn: Box::new(object_tn),
            initializer,
        },
        span: start..end,
    })
}

fn parse_initializer<'src>(state: &ParserState<'src, '_>) -> Result<'src, ObjectInitializer<'src>> {
    let expr = super::parse(state)?;

    let initializer = if state.tokens.current_is(&TokenKind::Colon) {
        state.tokens.advance();

        let value = super::parse(state)?;

        if state.tokens.current_is(&TokenKind::Comma) {
            state.tokens.advance();
        }

        let fields = parse_initializer_keyval_fields(state, vec![(expr, value)])?;

        ObjectInitializer::KeyVal(fields)
    } else {
        state.tokens.advance();

        let elems = parse_initializer_array_elems(state, vec![expr])?;

        ObjectInitializer::Array(elems)
    };

    Ok(initializer)
}

fn parse_initializer_keyval_fields<'src>(
    state: &ParserState<'src, '_>,
    mut fields: Vec<(AstNode<'src>, AstNode<'src>)>,
) -> Result<'src, Vec<(AstNode<'src>, AstNode<'src>)>> {
    loop {
        if state.tokens.current_is(&TokenKind::RBrace) {
            return Ok(fields);
        }

        let key = super::parse(state)?;

        state.tokens.expect(&TokenKind::Colon)?;

        let value = super::parse(state)?;

        fields.push((key, value));

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.expect(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RBrace => continue,

            kind => {
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RBrace,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }
}

fn parse_initializer_array_elems<'src>(
    state: &ParserState<'src, '_>,
    mut elems: Vec<AstNode<'src>>,
) -> Result<'src, Vec<AstNode<'src>>> {
    loop {
        if state.tokens.current_is(&TokenKind::RBrace) {
            return Ok(elems);
        }

        elems.push(super::parse(state)?);

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.expect(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RBrace => continue,

            kind => {
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RBrace,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }
}
