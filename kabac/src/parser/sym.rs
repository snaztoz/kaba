use super::{
    error::{ParsingError, ParsingErrorVariant, Result},
    state::ParserState,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub fn parse(state: &ParserState, label: &str) -> Result<AstNode> {
    let token = state.tokens.current();

    if let TokenKind::Symbol(name) = token.kind {
        state.tokens.advance();
        return Ok(AstNode::Symbol {
            name,
            span: token.span,
        });
    }

    Err(ParsingError {
        variant: ParsingErrorVariant::UnexpectedToken {
            expect: TokenKind::Symbol(String::from(label)),
            found: token.kind,
        },
        span: token.span,
    })
}
