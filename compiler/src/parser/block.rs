use super::{error::ParsingError, statement::StatementParser, stream::TokenStream, Result};
use crate::{ast::AstNode, lexer::TokenKind};
use logos::Span;

pub struct Block {
    pub body: Vec<AstNode>,
    pub span: Span,
}

pub struct BlockParser<'a> {
    tokens: &'a TokenStream,
    extra_delimiter: Option<TokenKind>,
}

impl<'a> BlockParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self {
            tokens,
            extra_delimiter: None,
        }
    }
}

impl BlockParser<'_> {
    pub fn allow_delimiter(mut self, delimiter: TokenKind) -> Self {
        self.extra_delimiter = Some(delimiter);
        self
    }

    pub fn parse(&self) -> Result<Block> {
        let start = self.tokens.current().span.start;

        // Expecting "do"
        self.tokens.skip(&TokenKind::Do)?;

        // Expecting statements
        let stmts = self.parse_stmts()?;

        let end = self.tokens.current().span.end;

        // Expecting "end" keyword or `extra_delimiter`
        //
        // Skip only if delimiter *is not* "end"
        if self.tokens.current_is(&TokenKind::End) {
            self.tokens.skip(&TokenKind::End)?;
        }

        Ok(Block {
            body: stmts,
            span: start..end,
        })
    }

    fn parse_stmts(&self) -> Result<Vec<AstNode>> {
        let mut stmts = vec![];
        loop {
            if let Some(tok) = &self.extra_delimiter {
                if self.tokens.current_is(tok) {
                    break;
                }
            }

            if self.tokens.current_is(&TokenKind::End) {
                break;
            } else if self.tokens.current_is(&TokenKind::Eof) {
                return Err(ParsingError::UnexpectedToken {
                    expect: TokenKind::End,
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
