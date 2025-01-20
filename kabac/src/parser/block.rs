use super::{error::ParsingError, state::ParserState, statement::StatementParser, Result};
use crate::{ast::AstNode, lexer::token::TokenKind};
use logos::Span;

pub struct Block {
    pub body: Vec<AstNode>,
    pub span: Span,
}

pub struct BlockParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> BlockParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl BlockParser<'_> {
    pub fn parse(&self) -> Result<Block> {
        let start = self.state.tokens.current().span.start;

        // Expecting "{"
        self.state.tokens.skip(&TokenKind::LBrace)?;

        // Expecting statements
        let stmts = self.parse_stmts()?;

        let end = self.state.tokens.current().span.end;

        // Expecting "}"
        self.state.tokens.skip(&TokenKind::RBrace)?;

        Ok(Block {
            body: stmts,
            span: start..end,
        })
    }

    fn parse_stmts(&self) -> Result<Vec<AstNode>> {
        let mut stmts = vec![];
        loop {
            if self.state.tokens.current_is(&TokenKind::RBrace) {
                break;
            }

            if self.state.tokens.current_is(&TokenKind::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::RBrace,
                    found: TokenKind::Eof,
                    span: self.state.tokens.current().span,
                });
            }

            let stmt = StatementParser::new(self.state).parse()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }
}
