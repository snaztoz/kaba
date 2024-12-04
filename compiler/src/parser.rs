//! This module contains the required logic operations during the parsing stage
//! of a Kaba tokens.

use crate::{
    ast::AstNode,
    lexer::{Token, TokenKind},
};
use error::Result;
use statement::StatementParser;
use stream::TokenStream;

mod block;
mod conditional;
mod error;
mod expression;
mod function;
mod statement;
mod stream;
#[cfg(test)]
mod test_util;
mod tn;
mod variable;
mod while_loop;

/// Provide a quick way to parse Kaba tokens, without the needs to setting up
/// and running the parser manually.
///
/// Produces an AST that represents the entire source code of the given tokens
/// (see [`crate::ast::Program`]).
pub fn parse(tokens: Vec<Token>) -> Result<AstNode> {
    Parser::new(tokens).parse()
}

struct Parser {
    tokens: TokenStream,
}

impl Parser {
    const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenStream::new(tokens),
        }
    }

    fn parse(&self) -> Result<AstNode> {
        let mut body = vec![];
        while !self.tokens.current_is(&TokenKind::Eof) {
            let stmt = StatementParser::new(&self.tokens).parse()?;
            body.push(stmt)
        }

        Ok(AstNode::Program { body })
    }
}
