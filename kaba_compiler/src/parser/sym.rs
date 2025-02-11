use super::{
    error::{ParsingError, ParsingErrorVariant, Result},
    state::ParserState,
};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>, label: &'src str) -> Result<'src, AstNode<'src>> {
    let token = state.tokens.current();

    if let TokenKind::Symbol(name) = token.kind {
        state.tokens.advance();
        return Ok(AstNode {
            variant: AstNodeVariant::Symbol { name },
            span: token.span,
        });
    }

    Err(ParsingError {
        variant: ParsingErrorVariant::UnexpectedToken {
            expect: TokenKind::Symbol(label),
            found: token.kind,
        },
        span: token.span,
    })
}
