use super::{
    error::{ParsingError, Result},
    expression::ExpressionParser,
    stream::TokenStream,
    tn::TypeNotationParser,
};
use crate::{ast::AstNode, lexer::TokenKind};

pub struct VariableDeclarationParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> VariableDeclarationParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
    }
}

impl VariableDeclarationParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "var" keyword
        self.tokens.skip(&TokenKind::Var)?;

        // Parse identifier
        let id = self.parse_id()?;

        // Parse type notation (optional)
        let tn = self.parse_tn()?;

        // Expecting "="
        self.tokens.skip(&TokenKind::Assign)?;

        // Parse value
        let expr = ExpressionParser::new(self.tokens).parse()?;

        let end = expr.span().end;

        // Expecting ";"
        self.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::VariableDeclaration {
            id: Box::new(id),
            tn: tn.map(Box::new),
            val: Box::new(expr.unwrap_group()),
            span: start..end,
        })
    }

    fn parse_id(&self) -> Result<AstNode> {
        match self.tokens.current_kind() {
            TokenKind::Identifier(name) => {
                let id = AstNode::Identifier {
                    name,
                    span: self.tokens.current().span.clone(),
                };

                self.tokens.advance();

                Ok(id)
            }
            _ => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("foo")),
                found: self.tokens.current_kind().clone(),
                span: self.tokens.current().span,
            }),
        }
    }

    fn parse_tn(&self) -> Result<Option<AstNode>> {
        let tn = if self.tokens.current_is(&TokenKind::Colon) {
            self.tokens.skip(&TokenKind::Colon)?;
            Some(TypeNotationParser::new(self.tokens).parse()?)
        } else {
            None
        };

        Ok(tn)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal, TypeNotation},
        parser::test_util::{parse_and_assert_error, parse_and_assert_result},
    };

    #[test]
    fn variable_declaration_without_type_notation() {
        parse_and_assert_result(
            "var abc = 123 * x;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 4..7,
                }),
                tn: None,
                val: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 10..13,
                    }),
                    rhs: Box::new(AstNode::Identifier {
                        name: String::from("x"),
                        span: 16..17,
                    }),
                    span: 10..17,
                }),
                span: 0..17,
            },
        );
    }

    #[test]
    fn variable_declaration_with_grouped_expression_as_initial_value() {
        parse_and_assert_result(
            "var x = (123 + 50);",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: None,
                val: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 9..12,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(50),
                        span: 15..17,
                    }),
                    span: 9..17,
                }),
                span: 0..18,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_grouped_expression_as_initial_value() {
        parse_and_assert_result(
            "var x = ((((foo))));",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: None,
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 12..15,
                }),
                span: 0..19,
            },
        );
    }

    #[test]
    fn variable_declaration_without_initial_value() {
        parse_and_assert_error("var x: Int;");
    }

    #[test]
    fn variable_declaration_with_both_type_notation_and_initial_value() {
        parse_and_assert_result(
            "var x: Int = 5;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Identifier(String::from("Int")),
                    span: 7..10,
                })),
                val: Box::new(AstNode::Literal {
                    lit: Literal::Integer(5),
                    span: 13..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn variable_declaration_with_function_type_notation() {
        parse_and_assert_result(
            "var x: (Int) -> Void = foo;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Int")),
                            span: 8..11,
                        }],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Identifier(String::from("Void")),
                            span: 16..20,
                        }),
                    },
                    span: 7..20,
                })),
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 23..26,
                }),
                span: 0..26,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_function_type_notation() {
        parse_and_assert_result(
            "var x: (Int, Bool) -> (Int,) -> Void = foo;",
            AstNode::VariableDeclaration {
                id: Box::from(AstNode::Identifier {
                    name: String::from("x"),
                    span: 4..5,
                }),
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![
                            AstNode::TypeNotation {
                                tn: TypeNotation::Identifier(String::from("Int")),
                                span: 8..11,
                            },
                            AstNode::TypeNotation {
                                tn: TypeNotation::Identifier(String::from("Bool")),
                                span: 13..17,
                            },
                        ],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Callable {
                                params_tn: vec![AstNode::TypeNotation {
                                    tn: TypeNotation::Identifier(String::from("Int")),
                                    span: 23..26,
                                }],
                                return_tn: Box::new(AstNode::TypeNotation {
                                    tn: TypeNotation::Identifier(String::from("Void")),
                                    span: 32..36,
                                }),
                            },
                            span: 22..36,
                        }),
                    },
                    span: 7..36,
                })),
                val: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 39..42,
                }),
                span: 0..42,
            },
        );
    }
}
