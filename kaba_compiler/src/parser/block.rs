use super::{
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    statement, Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};
use logos::Span;

pub struct Block<'src> {
    pub body: Vec<AstNode<'src>>,
    pub span: Span,
}

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, Block<'src>> {
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

fn parse_stmts<'src>(state: &ParserState<'src, '_>) -> Result<'src, Vec<AstNode<'src>>> {
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
