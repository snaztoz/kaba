//! This module contains the required logic operations during the parsing stage
//! of a Kaba tokens.

use crate::{
    ast::AstNode,
    lexer::token::{Token, TokenKind},
};
use error::Result;
use state::ParserState;
use statement::StatementParser;
use stream::TokenStream;

mod block;
mod conditional;
mod each_loop;
mod error;
mod expression;
mod function;
mod state;
mod statement;
mod stream;
#[cfg(test)]
mod test_util;
mod tn;
mod variable;
mod while_loop;

/// Parse a Kaba program from the provided tokens.
///
/// Produces an AST that represents the entire source code of the given tokens.
pub fn parse(tokens: Vec<Token>) -> Result<AstNode> {
    let tokens = TokenStream::new(tokens);
    let parser_state = ParserState::new(&tokens);

    let mut body = vec![];
    let global_scope_id = parser_state.next_scope_id();

    while !tokens.current_is(&TokenKind::Eof) {
        let stmt = StatementParser::new(&parser_state).parse()?;
        body.push(stmt)
    }

    Ok(AstNode::Program {
        body,
        scope_id: global_scope_id,
        span: 0..tokens.current().span.end,
    })
}
