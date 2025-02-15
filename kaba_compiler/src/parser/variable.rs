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
    let tn = if state.tokens.current_is(&TokenKind::Colon) {
        state.tokens.skip(&TokenKind::Colon)?;
        Some(tn::parse(state)?)
    } else {
        None
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
        assert_is_err("var x: Int;");
    }

    #[test]
    fn variable_declaration_with_both_type_notation_and_initial_value() {
        assert_ast(
            "var x: int = 5;",
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
                        span: 7..10,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(5),
                        },
                        span: 13..14,
                    }),
                },
                span: 0..14,
            },
        );
    }

    #[test]
    fn variable_declaration_with_function_type_notation() {
        assert_ast(
            "var x: (int) -> void = foo;",
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
                                    span: 8..11,
                                }],
                                return_tn: Box::new(AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::TypeNotation {
                                        tn: TypeNotation::Symbol("void"),
                                    },
                                    span: 16..20,
                                }),
                            },
                        },
                        span: 7..20,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 23..26,
                    }),
                },
                span: 0..26,
            },
        );
    }

    #[test]
    fn variable_declaration_with_nested_function_type_notation() {
        assert_ast(
            "var x: (int, bool) -> (int,) -> void = foo;",
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
                                        span: 8..11,
                                    },
                                    AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("bool"),
                                        },
                                        span: 13..17,
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
                                                span: 23..26,
                                            }],
                                            return_tn: Box::new(AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::TypeNotation {
                                                    tn: TypeNotation::Symbol("void"),
                                                },
                                                span: 32..36,
                                            }),
                                        },
                                    },
                                    span: 22..36,
                                }),
                            },
                        },
                        span: 7..36,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 39..42,
                    }),
                },
                span: 0..42,
            },
        );
    }

    #[test]
    fn variable_declaration_with_array_type() {
        assert_ast(
            "var x: [][]int = foo;",
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
                                                span: 11..14,
                                            }),
                                        },
                                    },
                                    span: 9..14,
                                }),
                            },
                        },
                        span: 7..14,
                    })),
                    val: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 17..20,
                    }),
                },
                span: 0..20,
            },
        );
    }
}
