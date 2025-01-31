use super::{
    block::BlockParser,
    error::{ParsingError, ParsingErrorVariant},
    expression::ExpressionParser,
    state::ParserState,
    Result,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub struct ConditionalParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> ConditionalParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl ConditionalParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;
        let mut end;

        // Expecting "if" keyword
        self.state.tokens.skip(&TokenKind::If)?;

        // Expecting expression
        let cond = ExpressionParser::new(self.state).parse()?;

        // Expecting block
        let scope_id = self.state.next_scope_id();
        let block = BlockParser::new(self.state).parse()?;

        end = block.span.end;

        // Expecting >= 0 "else if" or 1 "else"
        let or_else = self.parse_alt_branches(&mut end)?;

        Ok(AstNode::If {
            cond: Box::new(cond),
            body: block.body,
            scope_id,
            or_else: or_else.map(Box::new),
            span: start..end,
        })
    }

    fn parse_alt_branches(&self, end_pos: &mut usize) -> Result<Option<AstNode>> {
        if !self.state.tokens.current_is(&TokenKind::Else) {
            return Ok(None);
        }

        let start = self.state.tokens.current().span.start;

        // Expecting "else" keyword
        self.state.tokens.skip(&TokenKind::Else)?;

        match self.state.tokens.current_kind() {
            TokenKind::If => {
                // Expecting "else if ..." statement
                let alt = self.parse()?;

                *end_pos = alt.span().end;

                Ok(Some(alt))
            }

            TokenKind::LBrace => {
                // Expecting block
                let scope_id = self.state.next_scope_id();
                let block = BlockParser::new(self.state).parse()?;

                *end_pos = block.span.end;

                Ok(Some(AstNode::Else {
                    body: block.body,
                    scope_id,
                    span: start..block.span.end,
                }))
            }

            kind => Err(ParsingError {
                variant: ParsingErrorVariant::UnexpectedToken {
                    expect: TokenKind::Else,
                    found: kind.clone(),
                },
                span: self.state.tokens.current().span,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::assert_ast,
    };

    #[test]
    fn if_statement() {
        assert_ast(
            "if 15 > 10 { print(1); }",
            AstNode::If {
                cond: Box::new(AstNode::Gt {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(15),
                        span: 3..5,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(10),
                        span: 8..10,
                    }),
                    span: 3..10,
                }),
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: String::from("print"),
                        span: 13..18,
                    }),
                    args: vec![AstNode::Literal {
                        lit: Literal::Int(1),
                        span: 19..20,
                    }],
                    span: 13..21,
                }],
                scope_id: 2,
                or_else: None,
                span: 0..24,
            },
        );
    }

    #[test]
    fn if_else_branches() {
        assert_ast(
            "if false {} else if false {} else {}",
            AstNode::If {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(false),
                    span: 3..8,
                }),
                body: vec![],
                scope_id: 2,
                or_else: Some(Box::new(AstNode::If {
                    cond: Box::new(AstNode::Literal {
                        lit: Literal::Bool(false),
                        span: 20..25,
                    }),
                    body: vec![],
                    scope_id: 3,
                    or_else: Some(Box::new(AstNode::Else {
                        body: vec![],
                        scope_id: 4,
                        span: 29..36,
                    })),
                    span: 17..36,
                })),
                span: 0..36,
            },
        );
    }
}
