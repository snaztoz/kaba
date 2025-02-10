use super::{
    error::{ParsingError, ParsingErrorVariant, Result},
    state::ParserState,
    sym, tn,
};
use crate::{
    ast::{AstNode, RecordField},
    lexer::token::TokenKind,
};

pub fn parse<'src>(state: &ParserState<'src, '_>) -> Result<'src, AstNode<'src>> {
    let start = state.tokens.current().span.start;

    // Expecting "record" keyword
    state.tokens.skip(&TokenKind::Record)?;

    // Parse symbol
    let sym_id = state.next_symbol_id();
    let sym = sym::parse(state, "record name")?;

    state.tokens.skip(&TokenKind::LBrace)?;
    let fields = parse_fields(state)?;

    let end = state.tokens.current().span.end;
    state.tokens.skip(&TokenKind::RBrace)?;

    Ok(AstNode::RecordDefinition {
        sym: Box::new(sym),
        sym_id,
        fields,
        span: start..end,
    })
}

fn parse_fields<'src>(state: &ParserState<'src, '_>) -> Result<'src, Vec<RecordField<'src>>> {
    let mut fields = vec![];

    while !state.tokens.current_is(&TokenKind::RBrace) {
        // Expecting symbol
        let sym_id = state.next_symbol_id();
        let sym = sym::parse(state, "field name")?;

        // Expecting ":"
        state.tokens.skip(&TokenKind::Colon)?;

        // Expecting type notation
        let tn = tn::parse(state)?;

        fields.push(RecordField { sym, sym_id, tn });

        // Expecting either "," or "}"

        match state.tokens.current_kind() {
            TokenKind::Comma => {
                state.tokens.skip(&TokenKind::Comma)?;
                continue;
            }

            TokenKind::RBrace => continue,

            kind => {
                return Err(ParsingError {
                    variant: ParsingErrorVariant::UnexpectedToken {
                        expect: TokenKind::RBrace,
                        found: kind.clone(),
                    },
                    span: state.tokens.current().span,
                });
            }
        }
    }

    Ok(fields)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::TypeNotation, parser::test_util::assert_ast};

    #[test]
    fn empty_record() {
        assert_ast(
            "record Rec {}",
            AstNode::RecordDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "Rec",
                    span: 7..10,
                }),
                sym_id: 1,
                fields: vec![],
                span: 0..13,
            },
        );
    }

    #[test]
    fn record_with_single_field() {
        assert_ast(
            "record User { name: string, }",
            AstNode::RecordDefinition {
                sym: Box::new(AstNode::Symbol {
                    name: "User",
                    span: 7..11,
                }),
                sym_id: 1,
                fields: vec![RecordField {
                    sym: AstNode::Symbol {
                        name: "name",
                        span: 14..18,
                    },
                    sym_id: 2,
                    tn: AstNode::TypeNotation {
                        tn: TypeNotation::Symbol("string"),
                        span: 20..26,
                    },
                }],
                span: 0..29,
            },
        );
    }
}
