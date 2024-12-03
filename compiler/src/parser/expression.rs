use super::{error::ParsingError, stream::TokenStream, Result};
use crate::{
    ast::{AstNode, Literal},
    lexer::TokenKind,
};

pub struct ExpressionParser<'a> {
    tokens: &'a TokenStream,
}

impl<'a> ExpressionParser<'a> {
    pub const fn new(tokens: &'a TokenStream) -> Self {
        Self { tokens }
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
        match self.tokens.current_kind() {
            TokenKind::Assign => {
                self.tokens.skip(&TokenKind::Assign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Assign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::AddAssign => {
                self.tokens.skip(&TokenKind::AddAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::AddAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::SubAssign => {
                self.tokens.skip(&TokenKind::SubAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::SubAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::MulAssign => {
                self.tokens.skip(&TokenKind::MulAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::MulAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::DivAssign => {
                self.tokens.skip(&TokenKind::DivAssign)?;

                let rhs = self.parse_logical_and_or_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::DivAssign {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::ModAssign => {
                self.tokens.skip(&TokenKind::ModAssign)?;

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
            match self.tokens.current_kind() {
                TokenKind::Or => {
                    self.tokens.skip(&TokenKind::Or)?;

                    let rhs = self.parse_equality_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Or {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::And => {
                    self.tokens.skip(&TokenKind::And)?;

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
            match self.tokens.current_kind() {
                TokenKind::Eq => {
                    self.tokens.skip(&TokenKind::Eq)?;

                    let rhs = self.parse_comparison_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Eq {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Neq => {
                    self.tokens.skip(&TokenKind::Neq)?;

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
        match self.tokens.current_kind() {
            TokenKind::Gt => {
                self.tokens.skip(&TokenKind::Gt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Gte => {
                self.tokens.skip(&TokenKind::Gte)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Gte {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lt => {
                self.tokens.skip(&TokenKind::Lt)?;

                let rhs = self.parse_additive_expression()?;
                let span = lhs.span().start..rhs.span().end;

                Ok(AstNode::Lt {
                    lhs: Box::new(lhs.unwrap_group()),
                    rhs: Box::new(rhs.unwrap_group()),
                    span,
                })
            }
            TokenKind::Lte => {
                self.tokens.skip(&TokenKind::Lte)?;

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
            match self.tokens.current_kind() {
                TokenKind::Add => {
                    self.tokens.skip(&TokenKind::Add)?;

                    let rhs = self.parse_multiplicative_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Add {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Sub => {
                    self.tokens.skip(&TokenKind::Sub)?;

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
            match self.tokens.current_kind() {
                TokenKind::Mul => {
                    self.tokens.skip(&TokenKind::Mul)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Mul {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Div => {
                    self.tokens.skip(&TokenKind::Div)?;

                    let rhs = self.parse_unary_expression()?;
                    let span = lhs.span().start..rhs.span().end;

                    lhs = AstNode::Div {
                        lhs: Box::new(lhs.unwrap_group()),
                        rhs: Box::new(rhs.unwrap_group()),
                        span,
                    };
                }
                TokenKind::Mod => {
                    self.tokens.skip(&TokenKind::Mod)?;

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
        if self.tokens.current_is(&TokenKind::Sub) {
            return self.parse_prefix_expression(&TokenKind::Sub);
        } else if self.tokens.current_is(&TokenKind::Not) {
            return self.parse_prefix_expression(&TokenKind::Not);
        }

        // Parse primary expression
        let mut child = self.parse_primary_expression()?;

        // Followed by >= 0 function call, field access, or indexed access
        //
        // TODO: field access
        loop {
            match self.tokens.current_kind() {
                TokenKind::LParen => {
                    let callee_start = child.span().start;

                    // Expecting "("
                    self.tokens.skip(&TokenKind::LParen)?;

                    let args = self.parse_function_call()?;

                    let span = callee_start..self.tokens.current().span.end;

                    // Expecting ")"
                    self.tokens.skip(&TokenKind::RParen)?;

                    child = AstNode::FunctionCall {
                        callee: Box::new(child.unwrap_group()),
                        args,
                        span,
                    };
                }

                TokenKind::LBrack => {
                    let callee_start = child.span().start;

                    // Expecting "("
                    self.tokens.skip(&TokenKind::LBrack)?;

                    // Expecting expression
                    let index = self.parse()?;

                    let span = callee_start..self.tokens.current().span.end;

                    // Expecting ")"
                    self.tokens.skip(&TokenKind::RBrack)?;

                    child = AstNode::IndexAccess {
                        object: Box::new(child.unwrap_group()),
                        index: Box::new(index),
                        span,
                    };
                }

                _ => break,
            }
        }

        Ok(child)
    }

    fn parse_prefix_expression(&self, token: &TokenKind) -> Result<AstNode> {
        let start = self.tokens.current().span.start;
        self.tokens.skip(token)?;

        let child = self.parse_unary_expression()?;
        let span = start..child.span().end;

        match token {
            TokenKind::Sub => Ok(AstNode::Neg {
                child: Box::new(child.unwrap_group()),
                span,
            }),
            TokenKind::Not => Ok(AstNode::Not {
                child: Box::new(child.unwrap_group()),
                span,
            }),

            _ => unreachable!(),
        }
    }

    fn parse_primary_expression(&self) -> Result<AstNode> {
        let token = self.tokens.current();

        match token.kind {
            TokenKind::LParen => {
                // Parse group expression

                let lparen_start = token.span.start;
                self.tokens.skip(&TokenKind::LParen)?;

                let expression = self.parse()?;

                let span = lparen_start..self.tokens.current().span.end;
                self.tokens.skip(&TokenKind::RParen)?;

                Ok(AstNode::Group {
                    child: Box::new(expression),
                    span,
                })
            }

            // Expecting either identifier or literals
            TokenKind::Identifier(name) => {
                self.tokens.advance();
                Ok(AstNode::Identifier {
                    name,
                    span: token.span,
                })
            }
            TokenKind::Integer(n) => {
                self.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Integer(n),
                    span: token.span,
                })
            }
            TokenKind::Float(n) => {
                self.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Float(n),
                    span: token.span,
                })
            }
            TokenKind::BooleanTrue => {
                self.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Boolean(true),
                    span: token.span,
                })
            }
            TokenKind::BooleanFalse => {
                self.tokens.advance();
                Ok(AstNode::Literal {
                    lit: Literal::Boolean(false),
                    span: token.span,
                })
            }

            TokenKind::LBrack => self.parse_array_literal(),

            kind => Err(ParsingError::UnexpectedToken {
                expect: TokenKind::Identifier(String::from("foo")),
                found: kind.clone(),
                span: token.span,
            }),
        }
    }

    fn parse_function_call(&self) -> Result<Vec<AstNode>> {
        // Can have >= 0 arguments
        let mut args = vec![];

        loop {
            // Stop when encounter a closing parentheses
            if self.tokens.current_is(&TokenKind::RParen) {
                return Ok(args);
            }

            // Parse argument
            args.push(self.parse()?);

            // Continue if encounter "," or break out of loop if encounter ")"
            match self.tokens.current_kind() {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RParen => continue,

                kind => {
                    // Error if encountering neither "," or ")"
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RParen,
                        found: kind.clone(),
                        span: self.tokens.current().span,
                    });
                }
            }
        }
    }

    fn parse_array_literal(&self) -> Result<AstNode> {
        let start = self.tokens.current().span.start;

        // Expecting "["
        self.tokens.skip(&TokenKind::LBrack)?;

        // Can have >= 0 elements
        let mut elems = vec![];
        loop {
            // Stop when encounter a closing bracket
            if self.tokens.current_is(&TokenKind::RBrack) {
                break;
            }

            // Parse element
            elems.push(self.parse()?);

            // Continue if encounter "," or break out of loop if encounter ")"
            match self.tokens.current_kind() {
                TokenKind::Comma => {
                    self.tokens.skip(&TokenKind::Comma)?;
                    continue;
                }

                TokenKind::RBrack => continue,

                kind => {
                    // Error if encountering neither "," or ")"
                    return Err(ParsingError::UnexpectedToken {
                        expect: TokenKind::RBrack,
                        found: kind.clone(),
                        span: self.tokens.current().span,
                    });
                }
            }
        }

        let end = self.tokens.current().span.end;

        // Expecting "]"
        self.tokens.skip(&TokenKind::RBrack)?;

        Ok(AstNode::Literal {
            lit: Literal::Array(elems),
            span: start..end,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{AstNode, Literal},
        lexer::{self, TokenKind},
        parser::{error::ParsingError, parse, test_util::parse_and_assert_result},
    };

    #[test]
    fn math_expression() {
        parse_and_assert_result(
            "abc + 512 * 200 - abc / 3;",
            AstNode::Sub {
                lhs: Box::new(AstNode::Add {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    rhs: Box::new(AstNode::Mul {
                        lhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(512),
                            span: 6..9,
                        }),
                        rhs: Box::new(AstNode::Literal {
                            lit: Literal::Integer(200),
                            span: 12..15,
                        }),
                        span: 6..15,
                    }),
                    span: 0..15,
                }),
                rhs: Box::new(AstNode::Div {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 18..21,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(3),
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
        parse_and_assert_result(
            "abc = 123 * x;",
            AstNode::Assign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 6..9,
                    }),
                    rhs: Box::new(AstNode::Identifier {
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
        parse_and_assert_result(
            "x = (-5);",
            AstNode::Assign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
            "x += (-5);",
            AstNode::AddAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
            "x -= (-5);",
            AstNode::SubAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
            "x *= (-5);",
            AstNode::MulAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
            "x /= (-5);",
            AstNode::DivAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
            "x %= (-5);",
            AstNode::ModAssign {
                lhs: Box::new(AstNode::Identifier {
                    name: String::from("x"),
                    span: 0..1,
                }),
                rhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
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
        parse_and_assert_result(
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
        parse_and_assert_result(
            "(123 - 53) * 7;",
            AstNode::Mul {
                lhs: Box::new(AstNode::Sub {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(123),
                        span: 1..4,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(53),
                        span: 7..9,
                    }),
                    span: 1..9,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Integer(7),
                    span: 13..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn right_grouped_expression() {
        parse_and_assert_result(
            "123 + (foo - 50);",
            AstNode::Add {
                lhs: Box::new(AstNode::Literal {
                    lit: Literal::Integer(123),
                    span: 0..3,
                }),
                rhs: Box::new(AstNode::Sub {
                    lhs: Box::new(AstNode::Identifier {
                        name: String::from("foo"),
                        span: 7..10,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(50),
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
        parse_and_assert_result(
            "(((75)));",
            AstNode::Literal {
                lit: Literal::Integer(75),
                span: 3..5,
            },
        );
    }

    #[test]
    fn math_expression_function_call() {
        parse_and_assert_result(
            "abc(123, 50 + 2) * 7;",
            AstNode::Mul {
                lhs: Box::new(AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 0..3,
                    }),
                    args: vec![
                        AstNode::Literal {
                            lit: Literal::Integer(123),
                            span: 4..7,
                        },
                        AstNode::Add {
                            lhs: Box::new(AstNode::Literal {
                                lit: Literal::Integer(50),
                                span: 9..11,
                            }),
                            rhs: Box::new(AstNode::Literal {
                                lit: Literal::Integer(2),
                                span: 14..15,
                            }),
                            span: 9..15,
                        },
                    ],
                    span: 0..16,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Integer(7),
                    span: 19..20,
                }),
                span: 0..20,
            },
        );
    }

    #[test]
    fn nested_function_call() {
        parse_and_assert_result(
            "abc(xyz(123, 456),);",
            AstNode::FunctionCall {
                callee: Box::new(AstNode::Identifier {
                    name: String::from("abc"),
                    span: 0..3,
                }),
                args: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Identifier {
                        name: String::from("xyz"),
                        span: 4..7,
                    }),
                    args: vec![
                        AstNode::Literal {
                            lit: Literal::Integer(123),
                            span: 8..11,
                        },
                        AstNode::Literal {
                            lit: Literal::Integer(456),
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
        parse_and_assert_result(
            "-abc + (-(5)) * -(-7);",
            AstNode::Add {
                lhs: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::Identifier {
                        name: String::from("abc"),
                        span: 1..4,
                    }),
                    span: 0..4,
                }),
                rhs: Box::new(AstNode::Mul {
                    lhs: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Literal {
                            lit: Literal::Integer(5),
                            span: 10..11,
                        }),
                        span: 8..12,
                    }),
                    rhs: Box::new(AstNode::Neg {
                        child: Box::new(AstNode::Neg {
                            child: Box::new(AstNode::Literal {
                                lit: Literal::Integer(7),
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
        parse_and_assert_result(
            "-(-(abc)(-foo));",
            AstNode::Neg {
                child: Box::new(AstNode::Neg {
                    child: Box::new(AstNode::FunctionCall {
                        callee: Box::new(AstNode::Identifier {
                            name: String::from("abc"),
                            span: 4..7,
                        }),
                        args: vec![AstNode::Neg {
                            child: Box::new(AstNode::Identifier {
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
        parse_and_assert_result(
            "foo[3];",
            AstNode::IndexAccess {
                object: Box::new(AstNode::Identifier {
                    name: String::from("foo"),
                    span: 0..3,
                }),
                index: Box::new(AstNode::Literal {
                    lit: Literal::Integer(3),
                    span: 4..5,
                }),
                span: 0..6,
            },
        )
    }

    #[test]
    fn nested_index_access() {
        parse_and_assert_result(
            "foo[3][10];",
            AstNode::IndexAccess {
                object: Box::new(AstNode::IndexAccess {
                    object: Box::new(AstNode::Identifier {
                        name: String::from("foo"),
                        span: 0..3,
                    }),
                    index: Box::new(AstNode::Literal {
                        lit: Literal::Integer(3),
                        span: 4..5,
                    }),
                    span: 0..6,
                }),
                index: Box::new(AstNode::Literal {
                    lit: Literal::Integer(10),
                    span: 7..9,
                }),
                span: 0..10,
            },
        )
    }

    #[test]
    fn boolean_literal() {
        parse_and_assert_result(
            "true;",
            AstNode::Literal {
                lit: Literal::Boolean(true),
                span: 0..4,
            },
        );
    }

    #[test]
    fn comparison_and_equality_expressions() {
        parse_and_assert_result(
            "1 >= 5 == true;",
            AstNode::Eq {
                lhs: Box::new(AstNode::Gte {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(1),
                        span: 0..1,
                    }),
                    rhs: Box::new(AstNode::Literal {
                        lit: Literal::Integer(5),
                        span: 5..6,
                    }),
                    span: 0..6,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Boolean(true),
                    span: 10..14,
                }),
                span: 0..14,
            },
        );
    }

    #[test]
    fn logical_and_or_expression() {
        parse_and_assert_result(
            "false || !false && true;",
            AstNode::And {
                lhs: Box::new(AstNode::Or {
                    lhs: Box::new(AstNode::Literal {
                        lit: Literal::Boolean(false),
                        span: 0..5,
                    }),
                    rhs: Box::new(AstNode::Not {
                        child: Box::new(AstNode::Literal {
                            lit: Literal::Boolean(false),
                            span: 10..15,
                        }),
                        span: 9..15,
                    }),
                    span: 0..15,
                }),
                rhs: Box::new(AstNode::Literal {
                    lit: Literal::Boolean(true),
                    span: 19..23,
                }),
                span: 0..23,
            },
        );
    }

    #[test]
    fn empty_array() {
        parse_and_assert_result(
            "[];",
            AstNode::Literal {
                lit: Literal::Array(vec![]),
                span: 0..2,
            },
        )
    }

    #[test]
    fn array_with_single_element() {
        parse_and_assert_result(
            "[5,];",
            AstNode::Literal {
                lit: Literal::Array(vec![AstNode::Literal {
                    lit: Literal::Integer(5),
                    span: 1..2,
                }]),
                span: 0..4,
            },
        );
    }

    #[test]
    fn nested_arrays() {
        parse_and_assert_result(
            "[[], []];",
            AstNode::Literal {
                lit: Literal::Array(vec![
                    AstNode::Literal {
                        lit: Literal::Array(vec![]),
                        span: 1..3,
                    },
                    AstNode::Literal {
                        lit: Literal::Array(vec![]),
                        span: 5..7,
                    },
                ]),
                span: 0..8,
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
            ParsingError::UnexpectedToken {
                expect: TokenKind::RParen,
                found: TokenKind::Semicolon,
                span: 8..9,
            }
        );
    }
}
