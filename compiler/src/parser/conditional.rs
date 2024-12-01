use super::{
    block::BlockParser, error::ParsingError, expression::ExpressionParser, stream::TokenStream,
    Result,
};
use crate::{ast::AstNode, lexer::TokenKind};

pub struct ConditionalParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> ConditionalParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl ConditionalParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        let mut end;

        // Expecting "if" keyword
        self.tokens.skip(&TokenKind::If)?;

        // Expecting expression
        let cond = ExpressionParser::new(self.tokens).parse()?;

        // Expecting block
        let block = self.block_parser().parse()?;

        end = block.span.end;

        // Expecting >= 0 "else if" or 1 "else"
        let or_else = self.parse_alt_branches(&mut end)?;

        Ok(AstNode::If {
            cond: Box::new(cond),
            body: block.body,
            or_else: or_else.map(Box::new),
            span: start..end,
        })
    }

    fn parse_alt_branches(&self, end_pos: &mut usize) -> Result<Option<AstNode>> {
        if !self.tokens.current_is(&TokenKind::Else) {
            return Ok(None);
        }

        let start = self.tokens.current().span.start;

        // Expecting "else" keyword
        self.tokens.skip(&TokenKind::Else)?;

        match self.tokens.current_kind() {
            TokenKind::If => {
                // Expecting "else if ..." statement
                let alt = self.parse()?;

                *end_pos = alt.span().end;

                Ok(Some(alt))
            }

            TokenKind::Do => {
                // Expecting block
                let block = self.block_parser().parse()?;

                *end_pos = block.span.end;

                Ok(Some(AstNode::Else {
                    body: block.body,
                    span: start..block.span.end,
                }))
            }

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Else,
                found: kind.clone(),
                span: self.tokens.current().span,
            }),
        }
    }

    fn block_parser(&self) -> BlockParser<'_> {
        BlockParser::new(self.tokens).allow_delimiter(TokenKind::Else)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::parse_and_assert_result,
    };

    #[test]
    fn if_statement() {
        parse_and_assert_result(
            "if 15 > 10 do print(1); end",
            AstNode::If {
                cond: Box::new(AstNode::Gt {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(15),
                        span: 3..5,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(10),
                        span: 8..10,
                    }),
                    span: 3..10,
                }),
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("print"),
                        span: 14..19,
                    }),
                    args: vec![AstNode::Literal {
                        lit: Literal::Integer(1),
                        span: 20..21,
                    }],
                    span: 14..22,
                }],
                or_else: None,
                span: 0..27,
            },
        );
    }

    #[test]
    fn if_else_branches() {
        parse_and_assert_result(
            "if false do else if false do else do end",
            AstNode::If {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Boolean(false),
                    span: 3..8,
                }),
                body: vec![],
                or_else: Some(Box::new(AstNode::If {
                    cond: Box::new(AstNode::Literal {
                        lit: Literal::Boolean(false),
                        span: 20..25,
                    }),
                    body: vec![],
                    or_else: Some(Box::new(AstNode::Else {
                        body: vec![],
                        span: 29..40,
                    })),
                    span: 17..40,
                })),
                span: 0..40,
            },
        );
    }
}
