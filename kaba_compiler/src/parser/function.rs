use super::{
    block,
    error::{ParsingError, ParsingErrorVariant},
    state::ParserState,
    sym, tn, Result,
};
use crate::{
    ast::{AstNode, FunctionParam},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "fn" keyword
    state.tokens.skip(&TokenKind::Def)?;

    // Expecting symbol
    let sym_id = state.next_symbol_id();
    let sym = sym::parse(state, "function name")?;

    // Expecting parameters (optional)
    let params = if state.tokens.current_is(&TokenKind::LParen) {
        // Expecting >= 0 function parameter declaration(s)
        state.tokens.skip(&TokenKind::LParen)?;
        let params = parse_params(state)?;
        state.tokens.skip(&TokenKind::RParen)?;

        params
    } else {
        vec![]
    };

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

fn parse_params<'src>(state: &ParserState<'src, '_>) -> Result<'src, Vec<FunctionParam<'src>>> {
    let mut params = vec![];
    while !state.tokens.current_is(&TokenKind::RParen) {
        // Expecting symbol
        let sym_id = state.next_symbol_id();
        let sym = sym::parse(state, "parameter name")?;

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

fn parse_return_tn<'src>(state: &ParserState<'src, '_>) -> Result<'src, Option<AstNode<'src>>> {
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
            "def foo {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "foo",
                    span: 4..7,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: None,
                body: vec![],
                scope_id: 2,
                span: 0..10,
            },
        );
    }

    #[test]
    fn function_definition_with_zero_parameters_but_with_parentheses() {
        assert_ast(
            "def foo() {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "foo",
                    span: 4..7,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: None,
                body: vec![],
                scope_id: 2,
                span: 0..12,
            },
        );
    }

    #[test]
    fn function_definition_with_parameters_and_trailing_comma() {
        assert_ast(
            "def foo(x: int, y: bool,) {}",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "foo",
                    span: 4..7,
                }),
                sym_id: 1,
                params: vec![
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: "x",
                            span: 8..9,
                        },
                        sym_id: 2,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol("int"),
                            span: 11..14,
                        },
                    },
                    FunctionParam {
                        sym: AstNode::Symbol {
                            name: "y",
                            span: 16..17,
                        },
                        sym_id: 3,
                        tn: AstNode::TypeNotation {
                            tn: TypeNotation::Symbol("bool"),
                            span: 19..23,
                        },
                    },
                ],
                return_tn: None,
                body: vec![],
                scope_id: 2,
                span: 0..28,
            },
        );
    }

    #[test]
    fn function_definition_with_parameter_and_body() {
        assert_ast(
            "def write(x: int) { print(x); }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "write",
                    span: 4..9,
                }),
                sym_id: 1,
                params: vec![FunctionParam {
                    sym: AstNode::Symbol {
                        name: "x",
                        span: 10..11,
                    },
                    sym_id: 2,
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Symbol("int"),
                        span: 13..16,
                    },
                }],
                return_tn: None,
                body: vec![AstNode::FunctionCall {
                    callee: Box::new(AstNode::Symbol {
                        name: "print",
                        span: 20..25,
                    }),
                    args: vec![AstNode::Symbol {
                        name: "x",
                        span: 26..27,
                    }],
                    span: 20..28,
                }],
                scope_id: 2,
                span: 0..31,
            },
        );
    }

    #[test]
    fn function_definition_with_return_statement() {
        assert_ast(
            "def foo: int { return 5; }",
            AstNode::FunctionDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "foo",
                    span: 4..7,
                }),
                sym_id: 1,
                params: vec![],
                return_tn: Some(Box::new(AstNode::TypeNotation {
                    tn: TypeNotation::Symbol("int"),
                    span: 9..12,
                })),
                body: vec![AstNode::Return {
                    expr: Some(Box::new(AstNode::Literal {
                        lit: Literal::Int(5),
                        span: 22..23,
                    })),
                    span: 15..23,
                }],
                scope_id: 2,
                span: 0..26,
            },
        );
    }
}
