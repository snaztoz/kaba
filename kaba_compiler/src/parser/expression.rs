use super::{
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    sym, tn, Result,
};
use crate::{
    ast::{AstNode, AstNodeVariant, Literal},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    parse_assignment(state)
}

fn parse_assignment<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let lhs = parse_logical_and_or_expression(state)?;

    // Expecting "=", "+=", "-=", "*=", "/=", or "%=" (optional)
    match state.tokens.current_kind() {
        TokenKind::Assign => {
            state.tokens.skip(&TokenKind::Assign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Assign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }

        TokenKind::AddAssign => {
            state.tokens.skip(&TokenKind::AddAssign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::AddAssign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::SubAssign => {
            state.tokens.skip(&TokenKind::SubAssign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::SubAssign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::MulAssign => {
            state.tokens.skip(&TokenKind::MulAssign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::MulAssign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::DivAssign => {
            state.tokens.skip(&TokenKind::DivAssign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::DivAssign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::ModAssign => {
            state.tokens.skip(&TokenKind::ModAssign)?;

            let rhs = parse_logical_and_or_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::ModAssign {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        _ => Ok(lhs),
    }
}

fn parse_logical_and_or_expression<'src>(
    state: &ParserState<'src, '_>,
) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let mut lhs = parse_equality_expression(state)?;

    loop {
        // Expecting "||" or "&&" (both are optional)
        match state.tokens.current_kind() {
            TokenKind::Or => {
                state.tokens.skip(&TokenKind::Or)?;

                let rhs = parse_equality_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Or {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            TokenKind::And => {
                state.tokens.skip(&TokenKind::And)?;

                let rhs = parse_equality_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::And {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            _ => return Ok(lhs),
        }
    }
}

fn parse_equality_expression<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let mut lhs = parse_comparison_expression(state)?;

    loop {
        // Expecting "==" or "!=" (both are optional)
        match state.tokens.current_kind() {
            TokenKind::Eq => {
                state.tokens.skip(&TokenKind::Eq)?;

                let rhs = parse_comparison_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Eq {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            TokenKind::Neq => {
                state.tokens.skip(&TokenKind::Neq)?;

                let rhs = parse_comparison_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Neq {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            _ => return Ok(lhs),
        }
    }
}

fn parse_comparison_expression<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let lhs = parse_additive_expression(state)?;

    // Expecting ">", ">=", "<" or "<=" (all are optional)
    match state.tokens.current_kind() {
        TokenKind::Gt => {
            state.tokens.skip(&TokenKind::Gt)?;

            let rhs = parse_additive_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Gt {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::Gte => {
            state.tokens.skip(&TokenKind::Gte)?;

            let rhs = parse_additive_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Gte {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::Lt => {
            state.tokens.skip(&TokenKind::Lt)?;

            let rhs = parse_additive_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Lt {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::Lte => {
            state.tokens.skip(&TokenKind::Lte)?;

            let rhs = parse_additive_expression(state)?;
            let span = lhs.span.start..rhs.span.end;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Lte {
                    lhs: Box::new(lhs.into_group_inner()),
                    rhs: Box::new(rhs.into_group_inner()),
                },
                span,
            })
        }
        _ => Ok(lhs),
    }
}

fn parse_additive_expression<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let mut lhs = parse_multiplicative_expression(state)?;

    loop {
        // Expecting "+" or "-" (both are optional)
        match state.tokens.current_kind() {
            TokenKind::Add => {
                state.tokens.skip(&TokenKind::Add)?;

                let rhs = parse_multiplicative_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Add {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            TokenKind::Sub => {
                state.tokens.skip(&TokenKind::Sub)?;

                let rhs = parse_multiplicative_expression(state)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Sub {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            _ => return Ok(lhs),
        }
    }
}

fn parse_multiplicative_expression<'src>(
    state: &ParserState<'src, '_>,
) -> Result<'src, AstNode<'src>> {
    // Parse first term
    let mut lhs = parse_unary_expression(state, false)?;

    loop {
        // Expecting "*", "/" or "%" (all are optional)
        match state.tokens.current_kind() {
            TokenKind::Mul => {
                state.tokens.skip(&TokenKind::Mul)?;

                let rhs = parse_unary_expression(state, false)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Mul {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            TokenKind::Div => {
                state.tokens.skip(&TokenKind::Div)?;

                let rhs = parse_unary_expression(state, false)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Div {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            TokenKind::Mod => {
                state.tokens.skip(&TokenKind::Mod)?;

                let rhs = parse_unary_expression(state, false)?;
                let span = lhs.span.start..rhs.span.end;

                lhs = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Mod {
                        lhs: Box::new(lhs.into_group_inner()),
                        rhs: Box::new(rhs.into_group_inner()),
                    },
                    span,
                };
            }
            _ => return Ok(lhs),
        }
    }
}

fn parse_unary_expression<'src>(
    state: &ParserState<'src, '_>,
    is_negated: bool,
) -> Result<'src, AstNode<'src>> {
    //  Prefixed by >= 0 "negation" or "not" expression
    if state.tokens.current_is(&TokenKind::Sub) {
        return parse_prefix_expression(state, &TokenKind::Sub);
    } else if state.tokens.current_is(&TokenKind::Not) {
        return parse_prefix_expression(state, &TokenKind::Not);
    }

    // Parse primary expression
    let mut expr = parse_primary_expression(state, is_negated)?;

    // Followed by >= 0 function call, field access, or indexed access
    //
    // TODO: field access
    loop {
        match state.tokens.current_kind() {
            TokenKind::LParen => {
                let callee_start = expr.span.start;

                state.tokens.skip(&TokenKind::LParen)?;

                let args = parse_function_call(state)?;
                let span = callee_start..state.tokens.current().span.end;

                state.tokens.skip(&TokenKind::RParen)?;

                expr = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::FunctionCall {
                        callee: Box::new(expr.into_group_inner()),
                        args,
                    },
                    span,
                };
            }

            TokenKind::Dot => {
                let callee_start = expr.span.start;

                state.tokens.skip(&TokenKind::Dot)?;

                // Expecting symbol
                let field = sym::parse(state, "field name")?;
                let span = callee_start..field.span.end;

                expr = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::FieldAccess {
                        object: Box::new(expr.into_group_inner()),
                        field: Box::new(field),
                    },
                    span,
                };
            }

            TokenKind::LBrack => {
                let callee_start = expr.span.start;

                state.tokens.skip(&TokenKind::LBrack)?;

                // Expecting expression
                let index = parse(state)?;
                let span = callee_start..state.tokens.current().span.end;

                state.tokens.skip(&TokenKind::RBrack)?;

                expr = AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::IndexAccess {
                        object: Box::new(expr.into_group_inner()),
                        index: Box::new(index),
                    },
                    span,
                };
            }

            _ => break,
        }
    }

    Ok(expr)
}

fn parse_prefix_expression<'src>(
    state: &ParserState<'src, '_>,
    token: &TokenKind<'src>,
) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;
    state.tokens.skip(token)?;

    let expr = parse_unary_expression(state, matches!(token, TokenKind::Sub))?;
    let span = start..expr.span.end;

    match token {
        TokenKind::Sub => {
            // Don't wrap the expression in a negation operation if it's only
            // a literal. Instead, just update the span.
            if let AstNodeVariant::Literal {
                lit: Literal::Int(n),
                ..
            } = expr.variant
            {
                return Ok(AstNode {
                    id: state.next_id(),
                    variant: AstNodeVariant::Literal {
                        lit: Literal::Int(n),
                    },
                    span: start..expr.span.end,
                });
            }

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Neg {
                    expr: Box::new(expr.into_group_inner()),
                },
                span,
            })
        }
        TokenKind::Not => Ok(AstNode {
            id: state.next_id(),
            variant: AstNodeVariant::Not {
                expr: Box::new(expr.into_group_inner()),
            },
            span,
        }),

        _ => unreachable!(),
    }
}

fn parse_primary_expression<'src>(
    state: &ParserState<'src, '_>,
    is_negated: bool,
) -> Result<'src, AstNode<'src>> {
    let token = state.tokens.current();

    match token.kind {
        TokenKind::LParen => {
            // Parse group expression

            let lparen_start = token.span.start;
            state.tokens.skip(&TokenKind::LParen)?;

            let expr = parse(state)?;

            let span = lparen_start..state.tokens.current().span.end;
            state.tokens.skip(&TokenKind::RParen)?;

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Group {
                    expr: Box::new(expr),
                },
                span,
            })
        }

        // Expecting either symbols or literals
        TokenKind::Symbol(name) => {
            state.tokens.advance();
            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Symbol { name },
                span: token.span,
            })
        }
        TokenKind::Int(n) => {
            state.tokens.advance();

            //
            // The limit of integer literals are i32::MAX or i32::MAX+1 if
            // and only if `is_negated` is true.
            //
            let i32_max = i32::MAX as u32;
            if n > i32_max + 1 || (n == i32_max + 1 && !is_negated) {
                return Err(ParsingError {
                    variant: ParsingErrorVariant::NumberLiteralLimitExceeded,
                    span: token.span,
                });
            }

            let lit: i32 = match n {
                n if n == i32_max + 1 => i32::MIN,
                n if is_negated => -(n as i32),
                n => n as i32,
            };

            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Literal {
                    lit: Literal::Int(lit),
                },
                span: token.span,
            })
        }
        TokenKind::Float(n) => {
            state.tokens.advance();
            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Literal {
                    lit: Literal::Float(n),
                },
                span: token.span,
            })
        }
        TokenKind::Bool(b) => {
            state.tokens.advance();
            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Literal {
                    lit: Literal::Bool(b),
                },
                span: token.span,
            })
        }
        TokenKind::Char(c) => {
            state.tokens.advance();
            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Literal {
                    lit: Literal::Char(c),
                },
                span: token.span,
            })
        }
        TokenKind::String(s) => {
            state.tokens.advance();
            Ok(AstNode {
                id: state.next_id(),
                variant: AstNodeVariant::Literal {
                    lit: Literal::String(s),
                },
                span: token.span,
            })
        }

        TokenKind::LBrack => parse_array_literal(state),
        TokenKind::LBrace => parse_record_literal(state),

        kind => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Symbol("foo"),
                found: kind.clone(),
            },
            span: token.span,
        }),
    }
}

fn parse_function_call<'src>(state: &ParserState<'src, '_>) -> Result<'src, Vec<AstNode<'src>>> {
    // Can have >= 0 arguments
    let mut args = vec![];

    loop {
        // Stop when encounter a closing parentheses
        if state.tokens.current_is(&TokenKind::RParen) {
            return Ok(args);
        }

        // Parse argument
        args.push(parse(state)?);

        // Continue if encounter "," or break out of loop if encounter ")"
        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RParen => continue,

            kind => {
                // Error if encountering neither "," or ")"
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }
}

fn parse_array_literal<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "["
    state.tokens.skip(&TokenKind::LBrack)?;

    // Expecting type notation
    let elem_tn = tn::parse(state)?;

    // Can have >= 0 elements
    let mut elems = vec![];
    loop {
        // Stop when encounter a closing bracket
        if state.tokens.current_is(&TokenKind::RBrack) {
            break;
        }

        // Parse element
        elems.push(parse(state)?);

        // Continue if encounter "," or break out of loop if encounter ")"
        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RBrack => continue,

            kind => {
                // Error if encountering neither "," or "]"
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RBrack,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }

    let end = state.tokens.current().span.end;

    // Expecting "]"
    state.tokens.skip(&TokenKind::RBrack)?;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::Literal {
            lit: Literal::Array {
                elem_tn: Box::new(elem_tn),
                elems,
            },
        },
        span: start..end,
    })
}

fn parse_record_literal<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "{"
    state.tokens.skip(&TokenKind::LBrace)?;

    // Can have >= 0 fields
    let mut fields = vec![];
    loop {
        // Stop when encounter a closing bracket
        if state.tokens.current_is(&TokenKind::RBrace) {
            break;
        }

        // Parse field name
        let field_name = sym::parse(state, "field name")?;

        // Expecting colon
        state.tokens.skip(&TokenKind::Colon)?;

        // Parse field value
        let field_value = parse(state)?;

        fields.push((field_name, field_value));

        // Continue if encounter "," or break out of loop if encounter ")"
        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RBrace => continue,

            kind => {
                // Error if encountering neither "," or "]"
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RBrack,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }

    let end = state.tokens.current().span.end;

    // Expecting "]"
    state.tokens.skip(&TokenKind::RBrace)?;

    Ok(AstNode {
        id: state.next_id(),
        variant: AstNodeVariant::Literal {
            lit: Literal::Record { fields },
        },
        span: start..end,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, AstNodeVariant, Literal, TypeNotation},
        lexer::{self, token::TokenKind},
        parser::{
            error::{ParsingError, ParsingErrorVariant},
            parse,
            test_util::{assert_ast, assert_is_err},
        },
    };

    #[test]
    fn integer_literals() {
        assert_ast(
            "2147483647 + -2147483648;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Add {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(2147483647),
                        },
                        span: 0..10,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-2147483648),
                        },
                        span: 13..24,
                    }),
                },
                span: 0..24,
            },
        );
    }

    #[test]
    fn exceeding_integer_literals_limit() {
        assert_is_err("2147483648;");
        assert_is_err("-2147483649;");
    }

    #[test]
    fn math_expression() {
        assert_ast(
            "abc + 512 * 200 - abc / 3;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Sub {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Add {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "abc" },
                                span: 0..3,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Mul {
                                    lhs: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Literal {
                                            lit: Literal::Int(512),
                                        },
                                        span: 6..9,
                                    }),
                                    rhs: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Literal {
                                            lit: Literal::Int(200),
                                        },
                                        span: 12..15,
                                    }),
                                },
                                span: 6..15,
                            }),
                        },
                        span: 0..15,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Div {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "abc" },
                                span: 18..21,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(3),
                                },
                                span: 24..25,
                            }),
                        },
                        span: 18..25,
                    }),
                },
                span: 0..25,
            },
        );
    }

    #[test]
    fn variable_assignment() {
        assert_ast(
            "abc = 123 * x;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Assign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "abc" },
                        span: 0..3,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Mul {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(123),
                                },
                                span: 6..9,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "x" },
                                span: 12..13,
                            }),
                        },
                        span: 6..13,
                    }),
                },
                span: 0..13,
            },
        );
    }

    #[test]
    fn variable_assignment_with_negative_value() {
        assert_ast(
            "x = (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Assign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 5..7,
                    }),
                },
                span: 0..8,
            },
        );
    }

    #[test]
    fn variable_add_assign() {
        assert_ast(
            "x += (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::AddAssign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 6..8,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_sub_assign() {
        assert_ast(
            "x -= (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::SubAssign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 6..8,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_mul_assign() {
        assert_ast(
            "x *= (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::MulAssign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 6..8,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_div_assign() {
        assert_ast(
            "x /= (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::DivAssign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 6..8,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_mod_assign() {
        assert_ast(
            "x %= (-5);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::ModAssign {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "x" },
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(-5),
                        },
                        span: 6..8,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn float_values_mod_expression() {
        assert_ast(
            "50.0 % 2.0;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Mod {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Float(50.0),
                        },
                        span: 0..4,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Float(2.0),
                        },
                        span: 7..10,
                    }),
                },
                span: 0..10,
            },
        );
    }

    #[test]
    fn left_grouped_expression() {
        assert_ast(
            "(123 - 53) * 7;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Mul {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Sub {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(123),
                                },
                                span: 1..4,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(53),
                                },
                                span: 7..9,
                            }),
                        },
                        span: 1..9,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(7),
                        },
                        span: 13..14,
                    }),
                },
                span: 0..14,
            },
        );
    }

    #[test]
    fn right_grouped_expression() {
        assert_ast(
            "123 + (foo - 50);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Add {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(123),
                        },
                        span: 0..3,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Sub {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "foo" },
                                span: 7..10,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(50),
                                },
                                span: 13..15,
                            }),
                        },
                        span: 7..15,
                    }),
                },
                span: 0..16,
            },
        );
    }

    #[test]
    fn nested_grouped_expression() {
        assert_ast(
            "(((75)));",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Int(75),
                },
                span: 3..5,
            },
        );
    }

    #[test]
    fn math_expression_function_call() {
        assert_ast(
            "abc(123, 50 + 2) * 7;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Mul {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::FunctionCall {
                            callee: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "abc" },
                                span: 0..3,
                            }),
                            args: vec![
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Literal {
                                        lit: Literal::Int(123),
                                    },
                                    span: 4..7,
                                },
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Add {
                                        lhs: Box::new(AstNode {
                                            id: 0,
                                            variant: AstNodeVariant::Literal {
                                                lit: Literal::Int(50),
                                            },
                                            span: 9..11,
                                        }),
                                        rhs: Box::new(AstNode {
                                            id: 0,
                                            variant: AstNodeVariant::Literal {
                                                lit: Literal::Int(2),
                                            },
                                            span: 14..15,
                                        }),
                                    },
                                    span: 9..15,
                                },
                            ],
                        },
                        span: 0..16,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(7),
                        },
                        span: 19..20,
                    }),
                },
                span: 0..20,
            },
        );
    }

    #[test]
    fn nested_function_call() {
        assert_ast(
            "abc(xyz(123, 456),);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::FunctionCall {
                    callee: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "abc" },
                        span: 0..3,
                    }),
                    args: vec![AstNode {
                        id: 0,
                        variant: AstNodeVariant::FunctionCall {
                            callee: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "xyz" },
                                span: 4..7,
                            }),
                            args: vec![
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Literal {
                                        lit: Literal::Int(123),
                                    },
                                    span: 8..11,
                                },
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Literal {
                                        lit: Literal::Int(456),
                                    },
                                    span: 13..16,
                                },
                            ],
                        },
                        span: 4..17,
                    }],
                },
                span: 0..19,
            },
        );
    }

    #[test]
    fn math_expression_negative_numbers() {
        assert_ast(
            "-abc + (-(5)) * -(-7);",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Add {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Neg {
                            expr: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "abc" },
                                span: 1..4,
                            }),
                        },
                        span: 0..4,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Mul {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Neg {
                                    expr: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Literal {
                                            lit: Literal::Int(5),
                                        },
                                        span: 10..11,
                                    }),
                                },
                                span: 8..12,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Neg {
                                    expr: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Literal {
                                            lit: Literal::Int(-7),
                                        },
                                        span: 18..20,
                                    }),
                                },
                                span: 16..21,
                            }),
                        },
                        span: 7..21,
                    }),
                },
                span: 0..21,
            },
        );
    }

    #[test]
    fn negating_function_call() {
        assert_ast(
            "-(-(abc)(-foo));",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Neg {
                    expr: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Neg {
                            expr: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::FunctionCall {
                                    callee: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Symbol { name: "abc" },
                                        span: 4..7,
                                    }),
                                    args: vec![AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Neg {
                                            expr: Box::new(AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::Symbol { name: "foo" },
                                                span: 10..13,
                                            }),
                                        },
                                        span: 9..13,
                                    }],
                                },
                                span: 3..14,
                            }),
                        },
                        span: 2..14,
                    }),
                },
                span: 0..15,
            },
        );
    }

    #[test]
    fn field_access() {
        assert_ast(
            "foo.bar;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::FieldAccess {
                    object: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 0..3,
                    }),
                    field: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "bar" },
                        span: 4..7,
                    }),
                },
                span: 0..7,
            },
        );
    }

    #[test]
    fn accessing_field_from_function_call_result() {
        assert_ast(
            "foo().bar;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::FieldAccess {
                    object: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::FunctionCall {
                            callee: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "foo" },
                                span: 0..3,
                            }),
                            args: vec![],
                        },
                        span: 0..5,
                    }),
                    field: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "bar" },
                        span: 6..9,
                    }),
                },
                span: 0..9,
            },
        );
    }

    #[test]
    fn index_access() {
        assert_ast(
            "foo[3];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::IndexAccess {
                    object: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Symbol { name: "foo" },
                        span: 0..3,
                    }),
                    index: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(3),
                        },
                        span: 4..5,
                    }),
                },
                span: 0..6,
            },
        )
    }

    #[test]
    fn nested_index_access() {
        assert_ast(
            "foo[3][10];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::IndexAccess {
                    object: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::IndexAccess {
                            object: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "foo" },
                                span: 0..3,
                            }),
                            index: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(3),
                                },
                                span: 4..5,
                            }),
                        },
                        span: 0..6,
                    }),
                    index: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Int(10),
                        },
                        span: 7..9,
                    }),
                },
                span: 0..10,
            },
        )
    }

    #[test]
    fn boolean_literal() {
        assert_ast(
            "true;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Bool(true),
                },
                span: 0..4,
            },
        );
    }

    #[test]
    fn char_literal() {
        assert_ast(
            "'a';",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Char('a'),
                },
                span: 0..3,
            },
        );
    }

    #[test]
    fn string_literal() {
        assert_ast(
            r#""abc def 012";"#,
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::String(String::from("abc def 012")),
                },
                span: 0..13,
            },
        );
    }

    #[test]
    fn comparison_and_equality_expressions() {
        assert_ast(
            "1 >= 5 == true;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Eq {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Gte {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(1),
                                },
                                span: 0..1,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Int(5),
                                },
                                span: 5..6,
                            }),
                        },
                        span: 0..6,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Bool(true),
                        },
                        span: 10..14,
                    }),
                },
                span: 0..14,
            },
        );
    }

    #[test]
    fn logical_and_or_expression() {
        assert_ast(
            "false || !false && true;",
            AstNode {
                id: 0,
                variant: AstNodeVariant::And {
                    lhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Or {
                            lhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Bool(false),
                                },
                                span: 0..5,
                            }),
                            rhs: Box::new(AstNode {
                                id: 0,
                                variant: AstNodeVariant::Not {
                                    expr: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::Literal {
                                            lit: Literal::Bool(false),
                                        },
                                        span: 10..15,
                                    }),
                                },
                                span: 9..15,
                            }),
                        },
                        span: 0..15,
                    }),
                    rhs: Box::new(AstNode {
                        id: 0,
                        variant: AstNodeVariant::Literal {
                            lit: Literal::Bool(true),
                        },
                        span: 19..23,
                    }),
                },
                span: 0..23,
            },
        );
    }

    #[test]
    fn empty_array() {
        assert_ast(
            "[int];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Array {
                        elem_tn: Box::new(AstNode {
                            id: 0,
                            variant: AstNodeVariant::TypeNotation {
                                tn: TypeNotation::Symbol("int"),
                            },
                            span: 1..4,
                        }),
                        elems: vec![],
                    },
                },
                span: 0..5,
            },
        )
    }

    #[test]
    fn array_with_single_element() {
        assert_ast(
            "[int 5];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Array {
                        elem_tn: Box::new(AstNode {
                            id: 0,
                            variant: AstNodeVariant::TypeNotation {
                                tn: TypeNotation::Symbol("int"),
                            },
                            span: 1..4,
                        }),
                        elems: vec![AstNode {
                            id: 0,
                            variant: AstNodeVariant::Literal {
                                lit: Literal::Int(5),
                            },
                            span: 5..6,
                        }],
                    },
                },
                span: 0..7,
            },
        );
    }

    #[test]
    fn empty_nested_arrays() {
        assert_ast(
            "[[]int];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Array {
                        elem_tn: Box::new(AstNode {
                            id: 0,
                            variant: AstNodeVariant::TypeNotation {
                                tn: TypeNotation::Array {
                                    elem_tn: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("int"),
                                        },
                                        span: 3..6,
                                    }),
                                },
                            },
                            span: 1..6,
                        }),
                        elems: vec![],
                    },
                },
                span: 0..7,
            },
        )
    }

    #[test]
    fn nested_arrays() {
        assert_ast(
            "[[]int [int]];",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Array {
                        elem_tn: Box::new(AstNode {
                            id: 0,
                            variant: AstNodeVariant::TypeNotation {
                                tn: TypeNotation::Array {
                                    elem_tn: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("int"),
                                        },
                                        span: 3..6,
                                    }),
                                },
                            },
                            span: 1..6,
                        }),
                        elems: vec![AstNode {
                            id: 0,
                            variant: AstNodeVariant::Literal {
                                lit: Literal::Array {
                                    elem_tn: Box::new(AstNode {
                                        id: 0,
                                        variant: AstNodeVariant::TypeNotation {
                                            tn: TypeNotation::Symbol("int"),
                                        },
                                        span: 8..11,
                                    }),
                                    elems: vec![],
                                },
                            },
                            span: 7..12,
                        }],
                    },
                },
                span: 0..13,
            },
        )
    }

    #[test]
    fn empty_record() {
        assert_ast(
            "{};",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Record { fields: vec![] },
                },
                span: 0..2,
            },
        )
    }

    #[test]
    fn record_with_fields() {
        assert_ast(
            "{ foo: 5, bar: true };",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Record {
                        fields: vec![
                            (
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Symbol { name: "foo" },
                                    span: 2..5,
                                },
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Literal {
                                        lit: Literal::Int(5),
                                    },
                                    span: 7..8,
                                },
                            ),
                            (
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Symbol { name: "bar" },
                                    span: 10..13,
                                },
                                AstNode {
                                    id: 0,
                                    variant: AstNodeVariant::Literal {
                                        lit: Literal::Bool(true),
                                    },
                                    span: 15..19,
                                },
                            ),
                        ],
                    },
                },
                span: 0..21,
            },
        );
    }

    #[test]
    fn record_with_nested_values() {
        assert_ast(
            "{ foo: { bar: true } };",
            AstNode {
                id: 0,
                variant: AstNodeVariant::Literal {
                    lit: Literal::Record {
                        fields: vec![(
                            AstNode {
                                id: 0,
                                variant: AstNodeVariant::Symbol { name: "foo" },
                                span: 2..5,
                            },
                            AstNode {
                                id: 0,
                                variant: AstNodeVariant::Literal {
                                    lit: Literal::Record {
                                        fields: vec![(
                                            AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::Symbol { name: "bar" },
                                                span: 9..12,
                                            },
                                            AstNode {
                                                id: 0,
                                                variant: AstNodeVariant::Literal {
                                                    lit: Literal::Bool(true),
                                                },
                                                span: 14..18,
                                            },
                                        )],
                                    },
                                },
                                span: 7..20,
                            },
                        )],
                    },
                },
                span: 0..22,
            },
        )
    }

    #[test]
    fn grouped_expression_with_missing_close_tag() {
        let input = "(123 + 5;";
        let tokens = lexer::lex(input).unwrap();
        let result = parse(tokens);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            ParsingError {
                variant: ParsingErrorVariant::UnexpectedToken {
                    expect: TokenKind::RParen,
                    found: TokenKind::Semicolon
                },
                span: 8..9,
            }
        );
    }
}
