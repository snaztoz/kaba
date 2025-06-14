use super::{error::Result, expression, state::ParserState, sym, tn};
use crate::{
    ast::{AstNode, AstNodeVariant},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "var" keyword
    state.tokens.skip(&TokenKind::Var)?;

    // Parse symbol
    let sym = sym::parse(state, "variable name")?;

    // Parse type notation (optional)
    let tn = parse_tn(state)?;

    // Expecting "="
    state.tokens.skip(&TokenKind::Assign)?;

    // Parse value
    let expr = expression::parse(state)?;

    let end = expr.span.end;

    // Expecting ";"
    state.tokens.skip(&TokenKind::Semicolon)?;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::VariableDeclaration {
            sym: Box::new(sym),
            tn: tn.map(Box::new),
            val: Box::new(expr.into_group_inner()),
        },
        span: start..end,
    })
}

fn parse_tn<'src>(state: &ParserState<'src, '_>) -> Result<'src, Option<AstNode<'src>>> {
    let tn = if state.tokens.current_is(&TokenKind::Assign) {
        None
    } else {
        Some(tn::parse(state)?)
    };

    Ok(tn)
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, AstNodeVariant, Literal, TypeNotation},
        parser::test_util::{assert_ast, assert_is_err},
    };

    #[test]
    fn variable_declaration_without_type_notation() {
        assert_ast(
            "var abc = 123 * x;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "abc" },
                        span: 4..7,
                    }),

                    tn: None,
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Mul {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(123),
                                },
                                span: 10..13,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "x" },
                                span: 16..17,
                            }),
                        },
                        span: 10..17,
                    }),
                },
                span: 0..17,
            },
        );
    }

    #[test]
    fn variable_declaration_with_grouped_expression_as_initial_value() {
        assert_ast(
            "var x = (123 + 50);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: None,
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Add {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(123),
                                },
                                span: 9..12,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(50),
                                },
                                span: 15..17,
                            }),
                        },
                        span: 9..17,
                    }),
                },
                span: 0..18,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_grouped_expression_as_initial_value() {
        assert_ast(
            "var x = ((((foo))));",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: None,
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 12..15,
                    }),
                },
                span: 0..19,
            },
        );
    }

    #[test]
    fn variable_declaration_without_initial_value() {
        assert_is_err("var x Int;");
    }

    #[test]
    fn variable_declaration_with_both_type_notation_and_initial_value() {
        assert_ast(
            "var x int = 5;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: Some(Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::TypeNotation {
                            tn: TypeNotation::Symbol("int"),
                        },
                        span: 6..9,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(5),
                        },
                        span: 12..13,
                    }),
                },
                span: 0..13,
            },
        );
    }

    #[test]
    fn variable_declaration_with_function_type_notation() {
        assert_ast(
            "var x (int) -> void = foo;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: Some(Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::TypeNotation {
                            tn: TypeNotation::Callable {
                                params_tn: vec![AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::TypeNotation {
                                        tn: TypeNotation::Symbol("int"),
                                    },
                                    span: 7..10,
                                }],
                                return_tn: Box::new(AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::TypeNotation {
                                        tn: TypeNotation::Symbol("void"),
                                    },
                                    span: 15..19,
                                }),
                            },
                        },
                        span: 6..19,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 22..25,
                    }),
                },
                span: 0..25,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_function_type_notation() {
        assert_ast(
            "var x (int, bool) -> (int,) -> void = foo;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: Some(Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::TypeNotation {
                            tn: TypeNotation::Callable {
                                params_tn: vec![
                                    AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("int"),
                                        },
                                        span: 7..10,
                                    },
                                    AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("bool"),
                                        },
                                        span: 12..16,
                                    },
                                ],
                                return_tn: Box::new(AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::TypeNotation {
                                        tn: TypeNotation::Callable {
                                            params_tn: vec![AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::TypeNotation {
                                                    tn: TypeNotation::Symbol("int"),
                                                },
                                                span: 22..25,
                                            }],
                                            return_tn: Box::new(AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::TypeNotation {
                                                    tn: TypeNotation::Symbol("void"),
                                                },
                                                span: 31..35,
                                            }),
                                        },
                                    },
                                    span: 21..35,
                                }),
                            },
                        },
                        span: 6..35,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 38..41,
                    }),
                },
                span: 0..41,
            },
        );
    }

    #[test]
    fn variable_declaration_with_array_type() {
        assert_ast(
            "var x [][]int = foo;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::VariableDeclaration {
                    sym: Box::from(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 4..5,
                    }),

                    tn: Some(Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::TypeNotation {
                            tn: TypeNotation::Array {
                                elem_tn: Box::new(AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::TypeNotation {
                                        tn: TypeNotation::Array {
                                            elem_tn: Box::new(AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::TypeNotation {
                                                    tn: TypeNotation::Symbol("int"),
                                                },
                                                span: 10..13,
                                            }),
                                        },
                                    },
                                    span: 8..13,
                                }),
                            },
                        },
                        span: 6..13,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 16..19,
                    }),
                },
                span: 0..19,
            },
        );
    }
}
