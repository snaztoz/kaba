use super::{
    block::BlockParser, error::ParsingError, stream::TokenStream, tn::TypeNotationParser, Result,
};
use crate::{
    ast::{AstNode, FunctionParam},
    lexer::token::TokenKind,
};

pub struct FunctionDefinitionParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> FunctionDefinitionParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl FunctionDefinitionParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "fn" keyword
        self.tokens.skip(&TokenKind::Fn)?;

        // Expecting identifier
        let id = self.parse_id()?;

        // Expecting "("
        self.tokens.skip(&TokenKind::LParen)?;

        // Expecting >= 0 function parameter declaration(s)
        let params = self.parse_params()?;

        // Expecting ")"
        self.tokens.skip(&TokenKind::RParen)?;

        // Expecting return type notation (optional)
        let return_tn = self.parse_return_tn()?;

        // Expecting function body
        let block = BlockParser::new(self.tokens).parse()?;

        Ok(AstNode::FunctionDefinition {
            params,
            id: Box::new(id),
            return_tn: return_tn.map(Box::new),
            body: block.body,
            span: start..block.span.end,
        })
    }

    fn parse_id(&self) -> Result<AstNode> {
        let id = match self.tokens.current_kind() {
            TokenKind::Identifier(name) => Ok(AstNode::Identifier {
                name,
                span: self.tokens.current().span,
            }),

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("function_name")),
                found: kind.clone(),
                span: self.tokens.current().span,
            }),
        };

        self.tokens.advance();

        id
    }

    fn parse_params(&self) -> Result<Vec<FunctionParam>> {
        let mut params = vec![];
        while !self.tokens.current_is(&TokenKind::RParen) {
            // Expecting identifier
            let id = self.parse_id()?;

            // Expecting ":"
            self.tokens.skip(&TokenKind::Colon)?;

            // Expecting type notation
            let tn = TypeNotationParser::new(self.tokens).parse()?;

            params.push(FunctionParam { id, tn });

            // Expecting either "," or ")"

            match self.tokens.current_kind() {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: self.tokens.current().span,
                    });
                }
            }
        }

        Ok(params)
    }

    fn parse_return_tn(&self) -> Result<Option<AstNode>> {
        if !self.tokens.current_is(&TokenKind::Colon) {
            return Ok(None);
        }

        self.tokens.skip(&TokenKind::Colon)?;

        Ok(Some(TypeNotationParser::new(self.tokens).parse()?))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, FunctionParam, Literal, TypeNotation},
        parser::test_util::assert_ast,
    };

    #[test]
    fn empty_function_definition() {
        assert_ast(
            "fn foo() {}",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![],
                return_tn: None,
                body: vec![],
                span: 0..11,
            },
        );
    }

    #[test]
    fn function_definition_with_parameters_and_trailing_comma() {
        assert_ast(
            "fn foo(x: int, y: bool,) {}",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![
                    FunctionParam {
                        id: AstNode::Identifier {
                            name: String::from("x"),
                            span: 7..8,
                        },
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("int")),
                            span: 10..13,
                        },
                    },
                    FunctionParam {
                        id: AstNode::Identifier {
                            name: String::from("y"),
                            span: 15..16,
                        },
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("bool")),
                            span: 18..22,
                        },
                    },
                ],
                return_tn: None,
                body: vec![],
                span: 0..27,
            },
        );
    }

    #[test]
    fn function_definition_with_parameter_and_body() {
        assert_ast(
            "fn write(x: int) { print(x); }",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("write"),
                    span: 3..8,
                }),
                params: vec![FunctionParam {
                    id: AstNode::Identifier {
                        name: String::from("x"),
                        span: 9..10,
                    },
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Identifier(String::from("int")),
                        span: 12..15,
                    },
                }],
                return_tn: None,
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("print"),
                        span: 19..24,
                    }),
                    args: vec![AstNode::Identifier {
                        name: String::from("x"),
                        span: 25..26,
                    }],
                    span: 19..27,
                }],
                span: 0..30,
            },
        );
    }

    #[test]
    fn function_definition_with_return_statement() {
        assert_ast(
            "fn foo(): int { return 5; }",
            AstNode::FunctionDefinition {
                id: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                params: vec![],
                return_tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(String::from("int")),
                    span: 10..13,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 23..24,
                    })),
                    span: 16..24,
                }],
                span: 0..27,
            },
        );
    }
}
