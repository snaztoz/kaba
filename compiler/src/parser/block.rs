use super::{error::ParsingError, statement::StatementParser, stream::TokenStream, Result};
use crate::{ast::AstNode, lexer::TokenKind};
use logos::Span;

pub struct Block {
    pub body: Vec<AstNode>,
    pub span: Span,
}

pub struct BlockParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> BlockParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl BlockParser<'_> {
    pub fn parse(&self) -> Result<Block> {
        let start = self.tokens.current().span.start;

        // Expecting "{"
        self.tokens.skip(&TokenKind::LBrace)?;

        // Expecting statements
        let stmts = self.parse_stmts()?;

        let end = self.tokens.current().span.end;

        // Expecting "}"
        self.tokens.skip(&TokenKind::RBrace)?;

        Ok(Block {
            body: stmts,
            span: start..end,
        })
    }

    fn parse_stmts(&self) -> Result<Vec<AstNode>> {
        let mut stmts = vec![];
        loop {
            if self.tokens.current_is(&TokenKind::RBrace) {
                break;
            }

            if self.tokens.current_is(&TokenKind::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::RBrace,
                    found: TokenKind::Eof,
                    span: self.tokens.current().span,
                });
            }

            let stmt = StatementParser::new(self.tokens).parse()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }
}
