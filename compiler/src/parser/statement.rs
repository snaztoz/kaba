use super::{
    conditional::ConditionalParser, expression::ExpressionParser,
    function::FunctionDefinitionParser, stream::TokenStream, variable::VariableDeclarationParser,
    while_loop::WhileLoopParser, Result,
};
use crate::{
    ast::AstNode,
    lexer::{Token, TokenKind},
};

pub struct StatementParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> StatementParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl StatementParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        // Check if statement starts with a keyword
        match self.tokens.current_kind() {
            TokenKind::Var => return VariableDeclarationParser::new(self.tokens).parse(),
            TokenKind::If => return ConditionalParser::new(self.tokens).parse(),
            TokenKind::While => return WhileLoopParser::new(self.tokens).parse(),
            TokenKind::Break | TokenKind::Continue => return self.parse_loop_control(),
            TokenKind::Fn => return FunctionDefinitionParser::new(self.tokens).parse(),
            TokenKind::Return => return self.parse_return_statement(),
            TokenKind::Debug => return self.parse_debug_statement(),
            _ => (),
        }

        // Expecting expression
        let expr = ExpressionParser::new(self.tokens).parse()?;

        // Expecting ";"
        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(expr.unwrap_group())
    }

    fn parse_loop_control(&self) -> Result<AstNode> {
        let Token { kind, span, .. } = self.tokens.current();

        // Expecting either "break" or "continue" keyword
        let control = match kind {
            TokenKind::Break => AstNode::Break { span },
            TokenKind::Continue => AstNode::Continue { span },
            _ => unreachable!(),
        };

        self.tokens.advance();

        // Expecting ";"
        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(control)
    }

    fn parse_return_statement(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        let mut end = self.tokens.current().span.end;

        // Expecting "return" keyword
        self.tokens.skip(&TokenKind::Return)?;

        // Expecting expression (optional)
        let expr = if self.tokens.current_is(&TokenKind::Semicolon) {
            None
        } else {
            let expr = ExpressionParser::new(self.tokens).parse()?;
            end = expr.span().end;
            Some(expr)
        };

        // Expecting ";"
        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Return {
            expr: expr.map(Box::new),
            span: start..end,
        })
    }

    fn parse_debug_statement(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "debug" keyword
        self.tokens.skip(&TokenKind::Debug)?;

        // Expecting expression
        let expr = Box::new(ExpressionParser::new(self.tokens).parse()?);

        let end = expr.span().end;

        // Expecting ";"
        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Debug {
            expr,
            span: start..end,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        parser::test_util::parse_and_assert_result,
    };

    #[test]
    fn debug_statement() {
        parse_and_assert_result(
            "debug 5 + 5 * 7;",
            AstNode::Debug {
                expr: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
                        span: 6..7,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(5),
                            span: 10..11,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(7),
                            span: 14..15,
                        }),
                        span: 10..15,
                    }),
                    span: 6..15,
                }),
                span: 0..15,
            },
        )
    }
}
