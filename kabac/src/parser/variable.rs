use super::{
    error::{ParsingError, Result},
    expression::ExpressionParser,
    state::ParserState,
    tn::TypeNotationParser,
};
use crate::{ast::AstNode, lexer::token::TokenKind};

pub struct VariableDeclarationParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> VariableDeclarationParser<'a> {
    pub const fn new(state: &'a ParserState<'a>) -> Self {
        Self { state }
    }
}

impl VariableDeclarationParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "var" keyword
        self.state.tokens.skip(&TokenKind::Var)?;

        // Parse symbol
        let sym_id = self.state.next_symbol_id();
        let sym = self.parse_sym()?;

        // Parse type notation (optional)
        let tn = self.parse_tn()?;

        // Expecting "="
        self.state.tokens.skip(&TokenKind::Assign)?;

        // Parse value
        let expr = ExpressionParser::new(self.state).parse()?;

        let end = expr.span().end;

        // Expecting ";"
        self.state.tokens.skip(&TokenKind::Semicolon)?;

        Ok(AstNode::VariableDeclaration {
            sym: Box::new(sym),
            sym_id,
            tn: tn.map(Box::new),
            val: Box::new(expr.unwrap_group()),
            span: start..end,
        })
    }

    fn parse_sym(&self) -> Result<AstNode> {
        match self.state.tokens.current_kind() {
            TokenKind::Symbol(name) => {
                let sym = AstNode::Symbol {
                    name,
                    span: self.state.tokens.current().span.clone(),
                };

                self.state.tokens.advance();

                Ok(sym)
            }
            _ => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("foo")),
                found: self.state.tokens.current_kind().clone(),
                span: self.state.tokens.current().span,
            }),
        }
    }

    fn parse_tn(&self) -> Result<Option<AstNode>> {
        let tn = if self.state.tokens.current_is(&TokenKind::Colon) {
            self.state.tokens.skip(&TokenKind::Colon)?;
            Some(TypeNotationParser::new(self.state).parse()?)
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
        parser::test_util::{assert_ast, assert_is_err},
    };

    #[test]
    fn variable_declaration_without_type_notation() {
        assert_ast(
            "var abc = 123 * x;",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("abc"),
                    span: 4..7,
                }),
                sym_id: 1,
                tn: None,
                val: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(123),
                        span: 10..13,
                    }),
                    rhs: Box::new(AstNode::Symbol {
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
        assert_ast(
            "var x = (123 + 50);",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: None,
                val: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(123),
                        span: 9..12,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(50),
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
        assert_ast(
            "var x = ((((foo))));",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: None,
                val: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 12..15,
                }),
                span: 0..19,
            },
        );
    }

    #[test]
    fn variable_declaration_without_initial_value() {
        assert_is_err("var x: Int;");
    }

    #[test]
    fn variable_declaration_with_both_type_notation_and_initial_value() {
        assert_ast(
            "var x: int = 5;",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Symbol(String::from("int")),
                    span: 7..10,
                })),
                val: Box::new(AstNode::Literal {
                    lit: Literal::Int(5),
                    span: 13..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn variable_declaration_with_function_type_notation() {
        assert_ast(
            "var x: (int) -> void = foo;",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("int")),
                            span: 8..11,
                        }],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("void")),
                            span: 16..20,
                        }),
                    },
                    span: 7..20,
                })),
                val: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 23..26,
                }),
                span: 0..26,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_function_type_notation() {
        assert_ast(
            "var x: (int, bool) -> (int,) -> void = foo;",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: Some(Box::from(AstNode::TypeNotation {
                    tn: TypeNotation::Callable {
                        params_tn: vec![
                            AstNode::TypeNotation {
                                tn: TypeNotation::Symbol(String::from("int")),
                                span: 8..11,
                            },
                            AstNode::TypeNotation {
                                tn: TypeNotation::Symbol(String::from("bool")),
                                span: 13..17,
                            },
                        ],
                        return_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Callable {
                                params_tn: vec![AstNode::TypeNotation {
                                    tn: TypeNotation::Symbol(String::from("int")),
                                    span: 23..26,
                                }],
                                return_tn: Box::new(AstNode::TypeNotation {
                                    tn: TypeNotation::Symbol(String::from("void")),
                                    span: 32..36,
                                }),
                            },
                            span: 22..36,
                        }),
                    },
                    span: 7..36,
                })),
                val: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 39..42,
                }),
                span: 0..42,
            },
        );
    }

    #[test]
    fn variable_declaration_with_array_type() {
        assert_ast(
            "var x: [][]int = foo;",
            AstNode::VariableDeclaration {
                sym: Box::from(AstNode::Symbol {
                    name: String::from("x"),
                    span: 4..5,
                }),
                sym_id: 1,
                tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Array {
                        elem_tn: Box::new(AstNode::TypeNotation {
                            tn: TypeNotation::Array {
                                elem_tn: Box::new(AstNode::TypeNotation {
                                    tn: TypeNotation::Symbol(String::from("int")),
                                    span: 11..14,
                                }),
                            },
                            span: 9..14,
                        }),
                    },
                    span: 7..14,
                })),
                val: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 17..20,
                }),
                span: 0..20,
            },
        );
    }
}
