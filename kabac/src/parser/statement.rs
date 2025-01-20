use super::{
    conditional::ConditionalParser, each_loop::EachLoopParser, expression::ExpressionParser,
    function::FunctionDefinitionParser, state::ParserState, variable::VariableDeclarationParser,
    while_loop::WhileLoopParser, Result,
};
use crate::{
    ast::AstNode,
    lexer::token::{Token, TokenKind},
};

pub struct StatementParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> StatementParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl StatementParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        // Check if statement starts with a keyword
        match self.state.tokens.current_kind() {
            TokenKind::Var => return VariableDeclarationParser::new(self.state).parse(),
            TokenKind::If => return ConditionalParser::new(self.state).parse(),
            TokenKind::While => return WhileLoopParser::new(self.state).parse(),
            TokenKind::Each => return EachLoopParser::new(self.state).parse(),
            TokenKind::Break | TokenKind::Continue => return self.parse_loop_control(),
            TokenKind::Fn => return FunctionDefinitionParser::new(self.state).parse(),
            TokenKind::Return => return self.parse_return_statement(),
            TokenKind::Debug => return self.parse_debug_statement(),
            _ => (),
        }

        // Expecting expression
        let expr = ExpressionParser::new(self.state).parse()?;

        // Expecting ";"
        self.state.tokens.skip(&TokenKind::Semicolon)?;

        Ok(expr.unwrap_group())
    }

    fn parse_loop_control(&self) -> Result<AstNode> {
        let Token { kind, span, .. } = self.state.tokens.current();

        // Expecting either "break" or "continue" keyword
        let control = match kind {
            TokenKind::Break => AstNode::Break { span },
            TokenKind::Continue => AstNode::Continue { span },
            _ => unreachable!(),
        };

        self.state.tokens.advance();

        // Expecting ";"
        self.state.tokens.skip(&TokenKind::Semicolon)?;

        Ok(control)
    }

    fn parse_return_statement(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;
        let mut end = self.state.tokens.current().span.end;

        // Expecting "return" keyword
        self.state.tokens.skip(&TokenKind::Return)?;

        // Expecting expression (optional)
        let expr = if self.state.tokens.current_is(&TokenKind::Semicolon) {
            None
        } else {
            let expr = ExpressionParser::new(self.state).parse()?;
            end = expr.span().end;
            Some(expr)
        };

        // Expecting ";"
        self.state.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::Return {
            expr: expr.map(Box::new),
            span: start..end,
        })
    }

    fn parse_debug_statement(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "debug" keyword
        self.state.tokens.skip(&TokenKind::Debug)?;

        // Expecting expression
        let expr = Box::new(ExpressionParser::new(self.state).parse()?);

        let end = expr.span().end;

        // Expecting ";"
        self.state.tokens.skip(&TokenKind::Semicolon)?;

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
        parser::test_util::assert_ast,
    };

    #[test]
    fn debug_statement() {
        assert_ast(
            "debug 5 + 5 * 7;",
            AstNode::Debug {
                expr: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 6..7,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(5),
                            span: 10..11,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(7),
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
