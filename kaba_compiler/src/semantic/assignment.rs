use super::{
    error::{Result, SemanticError, SemanticErrorVariant},
    expression,
    state::AnalyzerState,
    types::{assert, Type},
};
use crate::{ast::AstNode, AstNodeVariant};

/// Analyze assignment statements.
///
/// It handles the analyzing of plain and shorthand assignment operations.
///
/// ### ✅ Valid Examples
///
/// * Plain assignment:
///
/// ```text
/// var x = 5;
/// x = 10;
/// ```
///
/// * Shorthand assignments:
///
/// ```text
/// var x = 1;
/// x += 2;
/// x -= 1;
/// x *= 10;
/// x /= 5;
/// x %= 2;
/// ```
///
/// ### ❌ Invalid Examples
///
/// * Variable can't be assigned with a value with incompatible type:
///
/// ```text
/// var x = 5;
/// x = 3.14;
/// ```
///
/// * Shorthand assignments (currently) only support value with number types
///   (integer and float):
///
/// ```text
/// var x = true;
/// x += false;
/// ```
pub fn analyze(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    analyze_lhs(node)?;

    match &node.variant {
        AstNodeVariant::Assign { .. } => analyze_assignment(state, node),

        AstNodeVariant::AddAssign { .. }
        | AstNodeVariant::SubAssign { .. }
        | AstNodeVariant::MulAssign { .. }
        | AstNodeVariant::DivAssign { .. }
        | AstNodeVariant::ModAssign { .. } => analyze_shorthand_assignment(state, node),

        _ => unreachable!(),
    }
}

fn analyze_assignment(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let (lhs, rhs) = if let AstNodeVariant::Assign { lhs, rhs } = &node.variant {
        (lhs, rhs)
    } else {
        unreachable!()
    };

    let lhs_t = expression::analyze(state, lhs)?;
    let rhs_t = expression::analyze(state, rhs)?;

    assert::is_assignable(&rhs_t, &lhs_t, || node.span.clone())?;

    Ok(Type::Void)
}

fn analyze_shorthand_assignment(state: &AnalyzerState, node: &AstNode) -> Result<Type> {
    let (lhs, rhs) = match &node.variant {
        AstNodeVariant::AddAssign { lhs, rhs }
        | AstNodeVariant::SubAssign { lhs, rhs }
        | AstNodeVariant::MulAssign { lhs, rhs }
        | AstNodeVariant::DivAssign { lhs, rhs }
        | AstNodeVariant::ModAssign { lhs, rhs } => (lhs, rhs),

        _ => unreachable!(),
    };

    let lhs_t = expression::analyze(state, lhs)?;
    let rhs_t = expression::analyze(state, rhs)?;

    assert::is_number(&lhs_t, || lhs.span.clone())?;
    assert::is_number(&rhs_t, || rhs.span.clone())?;
    assert::is_assignable(&rhs_t, &lhs_t, || node.span.clone())?;

    Ok(Type::Void)
}

fn analyze_lhs(node: &AstNode) -> Result<()> {
    let lhs = unwrap_lhs(node);

    if !lhs.variant.is_lval() {
        return Err(SemanticError {
            variant: SemanticErrorVariant::InvalidLValue,
            span: lhs.span.clone(),
        });
    }

    Ok(())
}

fn unwrap_lhs<'src, 'a>(node: &'a AstNode<'src>) -> &'a AstNode<'src> {
    match &node.variant {
        AstNodeVariant::Assign { lhs, .. }
        | AstNodeVariant::AddAssign { lhs, .. }
        | AstNodeVariant::SubAssign { lhs, .. }
        | AstNodeVariant::MulAssign { lhs, .. }
        | AstNodeVariant::DivAssign { lhs, .. }
        | AstNodeVariant::ModAssign { lhs, .. } => lhs,

        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn assigning_variables() {
        assert_is_ok(indoc! {"
                def main {
                    var x = 0;
                    x = 10;

                    var y: float = 0.0;
                    y = 5.0;

                    var z = false;
                    z = true;
                }
            "})
    }

    #[test]
    fn assigning_variable_using_shorthand_forms() {
        assert_is_ok(indoc! {"
                def main {
                    var i = 0;
                    i += 1;
                    i -= 2;
                    i *= 3;
                    i /= 4;
                    i %= 5;
                }
            "})
    }

    #[test]
    fn mod_assign_with_float_value() {
        assert_is_ok(indoc! {"
                def main {
                    var i = 5.0;
                    i %= 2.5;
                }
            "})
    }

    #[test]
    fn assigning_value_with_non_existing_variable() {
        assert_is_err(indoc! {"
                def main {
                    var x: float = 5.0;
                    x = y;
                }
            "})
    }

    #[test]
    fn assigning_overflowed_value() {
        assert_is_err(indoc! {"
                def main {
                    var x: sbyte = 0;
                    x = 128;
                }
            "})
    }

    #[test]
    fn using_math_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                def main {
                    1 + 1 = 5;
                }
            "})
    }

    #[test]
    fn using_boolean_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                def main {
                    true || false = false;
                }
            "})
    }

    #[test]
    fn using_integer_grouped_expression_as_lhs_in_assignment() {
        assert_is_err(indoc! {"
                def main {
                    (50) = true;
                }
            "})
    }

    #[test]
    fn using_boolean_type_in_shorthand_assignment() {
        assert_is_err(indoc! {"
                def main {
                    true += true;
                }
            "})
    }

    //
    // Arrays
    //

    #[test]
    fn assign_to_array_element() {
        assert_is_ok(indoc! {"
                def main {
                    var arr = [bool true, false, true];

                    arr[1] = true;
                }
            "});
    }

    #[test]
    fn shorthand_assign_to_array_element() {
        assert_is_ok(indoc! {"
                def main {
                    var arr = [float 0.5, 1.1, 2.3];

                    arr[0] += 5.5;
                }
            "});
    }
}
