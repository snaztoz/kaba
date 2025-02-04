use super::{
    block,
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    tn, Result,
};
use crate::{
    ast::{AstNode, FunctionParam},
    lexer::token::TokenKind,
};

pub fn parse(state: &ParserState) -> Result<AstNode> {
    let start = state.tokens.current().span.start;

    // Expecting "fn" keyword
    state.tokens.skip(&TokenKind::Fn)?;

    // Expecting symbol
    let sym_id = state.next_symbol_id();
    let sym = parse_sym(state)?;

    // Expecting "("
    state.tokens.skip(&TokenKind::LParen)?;

    // Expecting >= 0 function parameter declaration(s)
    let params = parse_params(state)?;

    // Expecting ")"
    state.tokens.skip(&TokenKind::RParen)?;

    // Expecting return type notation (optional)
    let return_tn = parse_return_tn(state)?;

    // Expecting function body
    let scope_id = state.next_scope_id();
    let block = block::parse(state)?;

    Ok(AstNode::FunctionDefinition {
        params,
        sym: Box::new(sym),
        sym_id,
        return_tn: return_tn.map(Box::new),
        body: block.body,
        scope_id,
        span: start..block.span.end,
    })
}

fn parse_sym(state: &ParserState) -> Result<AstNode> {
    let sym = match state.tokens.current_kind() {
        TokenKind::Symbol(name) => Ok(AstNode::Symbol {
            name,
            span: state.tokens.current().span,
        }),

        kind => Err(ParsingError {
            variant: ParsingErrorVariant::UnexpectedToken {
                expect: TokenKind::Symbol(String::from("function_name")),
                found: kind.clone(),
            },
            span: state.tokens.current().span,
        }),
    };

    state.tokens.advance();

    sym
}

fn parse_params(state: &ParserState) -> Result<Vec<FunctionParam>> {
    let mut params = vec![];
    while !state.tokens.current_is(&TokenKind::RParen) {
        // Expecting symbol
        let sym_id = state.next_symbol_id();
        let sym = parse_sym(state)?;

        // Expecting ":"
        state.tokens.skip(&TokenKind::Colon)?;

        // Expecting type notation
        let tn = tn::parse(state)?;

        params.push(FunctionParam { sym, sym_id, tn });

        // Expecting either "," or ")"

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RParen => continue,

            kind => {
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

    Ok(params)
}

fn parse_return_tn(state: &ParserState) -> Result<Option<AstNode>> {
    if !state.tokens.current_is(&TokenKind::Colon) {
        return Ok(None);
    }

    state.tokens.skip(&TokenKind::Colon)?;

    Ok(Some(tn::parse(state)?))
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
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: None,
                body: vec![],
                scope_id: 2,
                span: 0..11,
            },
        );
    }

    #[test]
    fn function_definition_with_parameters_and_trailing_comma() {
        assert_ast(
            "fn foo(x: int, y: bool,) {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: String::from("x"),
                            span: 7..8,
                        },
                        sym_id: 2,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("int")),
                            span: 10..13,
                        },
                    },
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: String::from("y"),
                            span: 15..16,
                        },
                        sym_id: 3,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol(String::from("bool")),
                            span: 18..22,
                        },
                    },
                ],
                return_tn: None,
                body: vec![],
                scope_id: 2,
                span: 0..27,
            },
        );
    }

    #[test]
    fn function_definition_with_parameter_and_body() {
        assert_ast(
            "fn write(x: int) { print(x); }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("write"),
                    span: 3..8,
                }),
                sym_id: 1,
                params: vec![FunctionParam {
                    sym: AstNode::Symbol {
                        name: String::from("x"),
                        span: 9..10,
                    },
                    sym_id: 2,
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Symbol(String::from("int")),
                        span: 12..15,
                    },
                }],
                return_tn: None,
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: String::from("print"),
                        span: 19..24,
                    }),
                    args: vec![AstNode::Symbol {
                        name: String::from("x"),
                        span: 25..26,
                    }],
                    span: 19..27,
                }],
                scope_id: 2,
                span: 0..30,
            },
        );
    }

    #[test]
    fn function_definition_with_return_statement() {
        assert_ast(
            "fn foo(): int { return 5; }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: String::from("foo"),
                    span: 3..6,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Symbol(String::from("int")),
                    span: 10..13,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 23..24,
                    })),
                    span: 16..24,
                }],
                scope_id: 2,
                span: 0..27,
            },
        );
    }
}
