use super::{
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    tn::TypeNotationParser,
    Result,
};
use crate::{
    ast::{AstNode, Literal},
    lexer::token::TokenKind,
};

pub struct ExpressionParser<'a> {
    state: &'a ParserState<'a>,
}

impl<'a> ExpressionParser<'a> {
    pub const fn new(state: &'a ParserState) -> Self {
        Self { state }
    }
}

impl ExpressionParser<'_> {
    pub fn parse(&self) -> Result<AstNode> {
        self.parse_assignment()
    }

    fn parse_assignment(&self) -> Result<AstNode> {
        // Parse first term
        let lhs = self.parse_logical_and_or_expression()?;

        // Expecting "=", "+=", "-=", "*=", "/=", or "%=" (optional)
        match self.state.tokens.current_kind() {
            TokenKind::Assign => {
                self.state.tokens.skip(&TokenKind::Assign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Assign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::AddAssign => {
                self.state.tokens.skip(&TokenKind::AddAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::AddAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::SubAssign => {
                self.state.tokens.skip(&TokenKind::SubAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::SubAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::MulAssign => {
                self.state.tokens.skip(&TokenKind::MulAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::MulAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::DivAssign => {
                self.state.tokens.skip(&TokenKind::DivAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::DivAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::ModAssign => {
                self.state.tokens.skip(&TokenKind::ModAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::ModAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            _ => Ok(lhs),
        }
    }

    fn parse_logical_and_or_expression(&self) -> Result<AstNode> {
        // Parse first term
        let mut lhs = self.parse_equality_expression()?;

        loop {
            // Expecting "||" or "&&" (both are optional)
            match self.state.tokens.current_kind() {
                TokenKind::Or => {
                    self.state.tokens.skip(&TokenKind::Or)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Or {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::And => {
                    self.state.tokens.skip(&TokenKind::And)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::And {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_equality_expression(&self) -> Result<AstNode> {
        // Parse first term
        let mut lhs = self.parse_comparison_expression()?;

        loop {
            // Expecting "==" or "!=" (both are optional)
            match self.state.tokens.current_kind() {
                TokenKind::Eq => {
                    self.state.tokens.skip(&TokenKind::Eq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Eq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Neq => {
                    self.state.tokens.skip(&TokenKind::Neq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Neq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_comparison_expression(&self) -> Result<AstNode> {
        // Parse first term
        let lhs = self.parse_additive_expression()?;

        // Expecting ">", ">=", "<" or "<=" (all are optional)
        match self.state.tokens.current_kind() {
            TokenKind::Gt => {
                self.state.tokens.skip(&TokenKind::Gt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Gte => {
                self.state.tokens.skip(&TokenKind::Gte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lt => {
                self.state.tokens.skip(&TokenKind::Lt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lte => {
                self.state.tokens.skip(&TokenKind::Lte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            _ => Ok(lhs),
        }
    }

    fn parse_additive_expression(&self) -> Result<AstNode> {
        // Parse first term
        let mut lhs = self.parse_multiplicative_expression()?;

        loop {
            // Expecting "+" or "-" (both are optional)
            match self.state.tokens.current_kind() {
                TokenKind::Add => {
                    self.state.tokens.skip(&TokenKind::Add)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Add {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Sub => {
                    self.state.tokens.skip(&TokenKind::Sub)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Sub {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_multiplicative_expression(&self) -> Result<AstNode> {
        // Parse first term
        let mut lhs = self.parse_unary_expression()?;

        loop {
            // Expecting "*", "/" or "%" (all are optional)
            match self.state.tokens.current_kind() {
                TokenKind::Mul => {
                    self.state.tokens.skip(&TokenKind::Mul)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mul {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Div => {
                    self.state.tokens.skip(&TokenKind::Div)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Div {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Mod => {
                    self.state.tokens.skip(&TokenKind::Mod)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mod {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_unary_expression(&self) -> Result<AstNode> {
        //  Prefixed by >= 0 "negation" or "not" expression
        if self.state.tokens.current_is(&TokenKind::Sub) {
            return self.parse_prefix_expression(&TokenKind::Sub);
        } else if self.state.tokens.current_is(&TokenKind::Not) {
            return self.parse_prefix_expression(&TokenKind::Not);
        }

        // Parse primary expression
        let mut expr = self.parse_primary_expression()?;

        // Followed by >= 0 function call, field access, or indexed access
        //
        // TODO: field access
        loop {
            match self.state.tokens.current_kind() {
                TokenKind::LParen => {
                    let callee_start = expr.span().start;

                    // Expecting "("
                    self.state.tokens.skip(&TokenKind::LParen)?;

                    let args = self.parse_function_call()?;

                    let span = callee_start..self.state.tokens.current().span.end;

                    // Expecting ")"
                    self.state.tokens.skip(&TokenKind::RParen)?;

                    expr = AstNode::FunctionCall {
                        callee: Box::new(expr.unwrap_group()),
                        args,
                        span,
                    };
                }

                TokenKind::LBrack => {
                    let callee_start = expr.span().start;

                    // Expecting "("
                    self.state.tokens.skip(&TokenKind::LBrack)?;

                    // Expecting expression
                    let index = self.parse()?;

                    let span = callee_start..self.state.tokens.current().span.end;

                    // Expecting ")"
                    self.state.tokens.skip(&TokenKind::RBrack)?;

                    expr = AstNode::IndexAccess {
                        object: Box::new(expr.unwrap_group()),
                        index: Box::new(index),
                        span,
                    };
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_prefix_expression(&self, token: &TokenKind) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;
        self.state.tokens.skip(token)?;

        let expr = self.parse_unary_expression()?;
        let span = start..expr.span().end;

        match token {
            TokenKind::Sub => Ok(AstNode::Neg {
                expr: Box::new(expr.unwrap_group()),
                span,
            }),
            TokenKind::Not => Ok(AstNode::Not {
                expr: Box::new(expr.unwrap_group()),
                span,
            }),

            _ => unreachable!(),
        }
    }

    fn parse_primary_expression(&self) -> Result<AstNode> {
        let token = self.state.tokens.current();

        match token.kind {
            TokenKind::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.state.tokens.skip(&TokenKind::LParen)?;

                let expr = self.parse()?;

                let span = lparen_start..self.state.tokens.current().span.end;
                self.state.tokens.skip(&TokenKind::RParen)?;

                Ok(AstNode::Group {
                    expr: Box::new(expr),
                    span,
                })
            }

            // Expecting either symbols or literals
            TokenKind::Symbol(name) => {
                self.state.tokens.advance();
                Ok(AstNode::Symbol {
                    name,
                    span: token.span,
                })
            }
            TokenKind::Int(n) => {
                self.state.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Int(n),
                    span: token.span,
                })
            }
            TokenKind::Float(n) => {
                self.state.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Float(n),
                    span: token.span,
                })
            }
            TokenKind::Bool(b) => {
                self.state.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Bool(b),
                    span: token.span,
                })
            }
            TokenKind::Char(c) => {
                self.state.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Char(c),
                    span: token.span,
                })
            }
            TokenKind::String(s) => {
                self.state.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::String(s),
                    span: token.span,
                })
            }

            TokenKind::LBrack => self.parse_array_literal(),

            kind => Err(ParsingError {
                variant: ParsingErrorVariant::UnexpectedToken {
                    expect: TokenKind::Symbol(String::from("foo")),
                    found: kind.clone(),
                },
                span: token.span,
            }),
        }
    }

    fn parse_function_call(&self) -> Result<Vec<AstNode>> {
        // Can have >= 0 arguments
        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses
            if self.state.tokens.current_is(&TokenKind::RParen) {
                return Ok(args);
            }

            // Parse argument
            args.push(self.parse()?);

            // Continue if encounter "," or break out of loop if encounter ")"
            match self.state.tokens.current_kind() {
                TokenKind::Comma => {
                    self.state.tokens.skip(&TokenKind::Comma)?;
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
                        span: self.state.tokens.current().span,
                    });
                }
            }
        }
    }

    fn parse_array_literal(&self) -> Result<AstNode> {
        let start = self.state.tokens.current().span.start;

        // Expecting "["
        self.state.tokens.skip(&TokenKind::LBrack)?;

        // Expecting type notation
        let elem_tn = TypeNotationParser::new(self.state).parse()?;

        // Can have >= 0 elements
        let mut elems = vec![];
        loop {
            // Stop when encounter a closing bracket
            if self.state.tokens.current_is(&TokenKind::RBrack) {
                break;
            }

            // Parse element
            elems.push(self.parse()?);

            // Continue if encounter "," or break out of loop if encounter ")"
            match self.state.tokens.current_kind() {
                TokenKind::Comma => {
                    self.state.tokens.skip(&TokenKind::Comma)?;
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
                        span: self.state.tokens.current().span,
                    });
                }
            }
        }

        let end = self.state.tokens.current().span.end;

        // Expecting "]"
        self.state.tokens.skip(&TokenKind::RBrack)?;

        Ok(AstNode::Literal {
            lit: Literal::Array {
                elem_tn: Box::new(elem_tn),
                elems,
            },
            span: start..end,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal, TypeNotation},
        lexer::{self, token::TokenKind},
        parser::{
            error::{ParsingError, ParsingErrorVariant},
            parse,
            test_util::assert_ast,
        },
    };

    #[test]
    fn math_expression() {
        assert_ast(
            "abc + 512 * 200 - abc / 3;",
            AstNode::Sub {
                lhs: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Symbol {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(512),
                            span: 6..9,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Int(200),
                            span: 12..15,
                        }),
                        span: 6..15,
                    }),
                    span: 0..15,
                }),
                rhs: Box::new(AstNode::Div {
                    lhs: Box::new(AstNode::Symbol {
                        name: String::from("abc"),
                        span: 18..21,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(3),
                        span: 24..25,
                    }),
                    span: 18..25,
                }),
                span: 0..25,
            },
        );
    }

    #[test]
    fn variable_assignment() {
        assert_ast(
            "abc = 123 * x;",
            AstNode::Assign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(123),
                        span: 6..9,
                    }),
                    rhs: Box::new(AstNode::Symbol {
                        name: String::from("x"),
                        span: 12..13,
                    }),
                    span: 6..13,
                }),
                span: 0..13,
            },
        );
    }

    #[test]
    fn variable_assignment_with_negative_value() {
        assert_ast(
            "x = (-5);",
            AstNode::Assign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 6..7,
                    }),
                    span: 5..7,
                }),
                span: 0..8,
            },
        );
    }

    #[test]
    fn variable_add_assign() {
        assert_ast(
            "x += (-5);",
            AstNode::AddAssign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_sub_assign() {
        assert_ast(
            "x -= (-5);",
            AstNode::SubAssign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_mul_assign() {
        assert_ast(
            "x *= (-5);",
            AstNode::MulAssign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_div_assign() {
        assert_ast(
            "x /= (-5);",
            AstNode::DivAssign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn variable_mod_assign() {
        assert_ast(
            "x %= (-5);",
            AstNode::ModAssign {
                lhs: Box::new(AstNode::Symbol {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 7..8,
                    }),
                    span: 6..8,
                }),
                span: 0..9,
            },
        );
    }

    #[test]
    fn float_values_mod_expression() {
        assert_ast(
            "50.0 % 2.0;",
            AstNode::Mod {
                lhs: Box::new(AstNode::Literal {
                    lit: Literal::Float(50.0),
                    span: 0..4,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Float(2.0),
                    span: 7..10,
                }),
                span: 0..10,
            },
        );
    }

    #[test]
    fn left_grouped_expression() {
        assert_ast(
            "(123 - 53) * 7;",
            AstNode::Mul {
                lhs: Box::new(AstNode::Sub {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(123),
                        span: 1..4,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(53),
                        span: 7..9,
                    }),
                    span: 1..9,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Int(7),
                    span: 13..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn right_grouped_expression() {
        assert_ast(
            "123 + (foo - 50);",
            AstNode::Add {
                lhs: Box::new(AstNode::Literal {
                    lit: Literal::Int(123),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Sub {
                    lhs: Box::new(AstNode::Symbol {
                        name: String::from("foo"),
                        span: 7..10,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(50),
                        span: 13..15,
                    }),
                    span: 7..15,
                }),
                span: 0..16,
            },
        );
    }

    #[test]
    fn nested_grouped_expression() {
        assert_ast(
            "(((75)));",
            AstNode::Literal {
                lit: Literal::Int(75),
                span: 3..5,
            },
        );
    }

    #[test]
    fn math_expression_function_call() {
        assert_ast(
            "abc(123, 50 + 2) * 7;",
            AstNode::Mul {
                lhs: Box::new(AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    args: vec![
                        AstNode::Literal {
                            lit: Literal::Int(123),
                            span: 4..7,
                        },
                        AstNode::Add {
                            lhs: Box::new(AstNode::Literal {
                                lit: Literal::Int(50),
                                span: 9..11,
                            }),
                            rhs: Box::new(AstNode::Literal {
                                lit: Literal::Int(2),
                                span: 14..15,
                            }),
                            span: 9..15,
                        },
                    ],
                    span: 0..16,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Int(7),
                    span: 19..20,
                }),
                span: 0..20,
            },
        );
    }

    #[test]
    fn nested_function_call() {
        assert_ast(
            "abc(xyz(123, 456),);",
            AstNode::FunctionCall {
                callee: Box::new(AstNode::Symbol {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                args: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: String::from("xyz"),
                        span: 4..7,
                    }),
                    args: vec![
                        AstNode::Literal {
                            lit: Literal::Int(123),
                            span: 8..11,
                        },
                        AstNode::Literal {
                            lit: Literal::Int(456),
                            span: 13..16,
                        },
                    ],
                    span: 4..17,
                }],
                span: 0..19,
            },
        );
    }

    #[test]
    fn math_expression_negative_numbers() {
        assert_ast(
            "-abc + (-(5)) * -(-7);",
            AstNode::Add {
                lhs: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::Symbol {
                        name: String::from("abc"),
                        span: 1..4,
                    }),
                    span: 0..4,
                }),
                rhs: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Neg {
                        expr: Box::new(AstNode::Literal {
                            lit: Literal::Int(5),
                            span: 10..11,
                        }),
                        span: 8..12,
                    }),
                    rhs: Box::new(AstNode::Neg {
                        expr: Box::new(AstNode::Neg {
                            expr: Box::new(AstNode::Literal {
                                lit: Literal::Int(7),
                                span: 19..20,
                            }),
                            span: 18..20,
                        }),
                        span: 16..21,
                    }),
                    span: 7..21,
                }),
                span: 0..21,
            },
        );
    }

    #[test]
    fn negating_function_call() {
        assert_ast(
            "-(-(abc)(-foo));",
            AstNode::Neg {
                expr: Box::new(AstNode::Neg {
                    expr: Box::new(AstNode::FunctionCall {
                        callee: Box::new(AstNode::Symbol {
                            name: String::from("abc"),
                            span: 4..7,
                        }),
                        args: vec![AstNode::Neg {
                            expr: Box::new(AstNode::Symbol {
                                name: String::from("foo"),
                                span: 10..13,
                            }),
                            span: 9..13,
                        }],
                        span: 3..14,
                    }),
                    span: 2..14,
                }),
                span: 0..15,
            },
        );
    }

    #[test]
    fn index_access() {
        assert_ast(
            "foo[3];",
            AstNode::IndexAccess {
                object: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 0..3,
                }),
                index: Box::new(AstNode::Literal {
                    lit: Literal::Int(3),
                    span: 4..5,
                }),
                span: 0..6,
            },
        )
    }

    #[test]
    fn nested_index_access() {
        assert_ast(
            "foo[3][10];",
            AstNode::IndexAccess {
                object: Box::new(AstNode::IndexAccess {
                    object: Box::new(AstNode::Symbol {
                        name: String::from("foo"),
                        span: 0..3,
                    }),
                    index: Box::new(AstNode::Literal {
                        lit: Literal::Int(3),
                        span: 4..5,
                    }),
                    span: 0..6,
                }),
                index: Box::new(AstNode::Literal {
                    lit: Literal::Int(10),
                    span: 7..9,
                }),
                span: 0..10,
            },
        )
    }

    #[test]
    fn boolean_literal() {
        assert_ast(
            "true;",
            AstNode::Literal {
                lit: Literal::Bool(true),
                span: 0..4,
            },
        );
    }

    #[test]
    fn char_literal() {
        assert_ast(
            "'a';",
            AstNode::Literal {
                lit: Literal::Char('a'),
                span: 0..3,
            },
        );
    }

    #[test]
    fn string_literal() {
        assert_ast(
            r#""abc def 012";"#,
            AstNode::Literal {
                lit: Literal::String(String::from("abc def 012")),
                span: 0..13,
            },
        );
    }

    #[test]
    fn comparison_and_equality_expressions() {
        assert_ast(
            "1 >= 5 == true;",
            AstNode::Eq {
                lhs: Box::new(AstNode::Gte {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(1),
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 5..6,
                    }),
                    span: 0..6,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 10..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn logical_and_or_expression() {
        assert_ast(
            "false || !false && true;",
            AstNode::And {
                lhs: Box::new(AstNode::Or {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Bool(false),
                        span: 0..5,
                    }),
                    rhs: Box::new(AstNode::Not {
                        expr: Box::new(AstNode::Literal {
                            lit: Literal::Bool(false),
                            span: 10..15,
                        }),
                        span: 9..15,
                    }),
                    span: 0..15,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Bool(true),
                    span: 19..23,
                }),
                span: 0..23,
            },
        );
    }

    #[test]
    fn empty_array() {
        assert_ast(
            "[int];",
            AstNode::Literal {
                lit: Literal::Array {
                    elem_tn: Box::new(AstNode::TypeNotation {
                        tn: TypeNotation::Symbol(String::from("int")),
                        span: 1..4,
                    }),
                    elems: vec![],
                },
                span: 0..5,
            },
        )
    }

    #[test]
    fn array_with_single_element() {
        assert_ast(
            "[int 5];",
            AstNode::Literal {
                lit: Literal::Array {
                    elem_tn: Box::new(AstNode::TypeNotation {
                        tn: TypeNotation::Symbol(String::from("int")),
                        span: 1..4,
                    }),
                    elems: vec![AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 5..6,
                    }],
                },
                span: 0..7,
            },
        );
    }

    #[test]
    fn empty_nested_arrays() {
        assert_ast(
            "[[]int];",
            AstNode::Literal {
                lit: Literal::Array {
                    elem_tn: Box::new(AstNode::TypeNotation {
                        tn: TypeNotation::Array {
                            elem_tn: Box::new(AstNode::TypeNotation {
                                tn: TypeNotation::Symbol(String::from("int")),
                                span: 3..6,
                            }),
                        },
                        span: 1..6,
                    }),
                    elems: vec![],
                },
                span: 0..7,
            },
        )
    }

    #[test]
    fn nested_arrays() {
        assert_ast(
            "[[]int [int]];",
            AstNode::Literal {
                lit: Literal::Array {
                    elem_tn: Box::new(AstNode::TypeNotation {
                        tn: TypeNotation::Array {
                            elem_tn: Box::new(AstNode::TypeNotation {
                                tn: TypeNotation::Symbol(String::from("int")),
                                span: 3..6,
                            }),
                        },
                        span: 1..6,
                    }),
                    elems: vec![AstNode::Literal {
                        lit: Literal::Array {
                            elem_tn: Box::new(AstNode::TypeNotation {
                                tn: TypeNotation::Symbol(String::from("int")),
                                span: 8..11,
                            }),
                            elems: vec![],
                        },
                        span: 7..12,
                    }],
                },
                span: 0..13,
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
