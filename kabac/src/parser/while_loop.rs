use super::{block::BlockParser, expression::ExpressionParser, state::ParserState, Result};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub struct WhileLoopParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> WhileLoopParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl WhileLoopParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "while" keyword
        self.state.tokens.skip(&TokenKind::While)?;

        // Expecting expression
        let cond = ExpressionParser::new(self.state).parse()?;

        // Expecting block
        let block = BlockParser::new(self.state).parse()?;

        let end = block.span.end;

        Ok(AstNode::While {
            cond: Box::new(cond),
            body: block.body,
            span: start..end,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::assert_ast,
    };

    #[test]
    fn while_statement() {
        assert_ast(
            "while true {}",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![],
                span: 0..13,
            },
        );
    }

    #[test]
    fn while_statement_with_loop_control_statements() {
        assert_ast(
            "while true { continue; break; }",
            AstNode::While {
                cond: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 6..10,
                }),
                body: vec![
                    AstNode::Continue { span: 13..21 },
                    AstNode::Break { span: 23..28 },
                ],
                span: 0..31,
            },
        );
    }
}
