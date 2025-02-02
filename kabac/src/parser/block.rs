use super::{
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    statement, Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};
use logos::Span;

pub struct Block {
    pub body: Vec<AstNode>,
    pub span: Span,
}

pub fn parse(state: &ParserState) -> Result<Block> {
    let start = state.tokens.current().span.start;

    // Expecting "{"
    state.tokens.skip(&TokenKind::LBrace)?;

    // Expecting statements
    let stmts = parse_stmts(state)?;

    let end = state.tokens.current().span.end;

    // Expecting "}"
    state.tokens.skip(&TokenKind::RBrace)?;

    Ok(Block {
        body: stmts,
        span: start..end,
    })
}

fn parse_stmts(state: &ParserState) -> Result<Vec<AstNode>> {
    let mut stmts = vec![];
    loop {
        if state.tokens.current_is(&TokenKind::RBrace) {
            break;
        }

        if state.tokens.current_is(&TokenKind::Eof) {
            return Err(ParsingError {
                variant: ParsingErrorVariant::UnexpectedToken {
                    expect: TokenKind::RBrace,
                    found: TokenKind::Eof,
                },
                span: state.tokens.current().span,
            });
        }

        let stmt = statement::parse(state)?;
        stmts.push(stmt);
    }

    Ok(stmts)
}
