use super::{
    body::BodyChecker,
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::{Scope, ScopeStack},
    types::Type,
};
use crate::ast::AstNode;

/// Checker for `each` loop statement.
///
/// ### ✅ Valid Examples
///
/// * The provided expression must evaluates to an array:
///
/// ```text
/// each [0, 1, 2] as n do
///     # ...
/// end
/// ```
///
/// * With `break` or `continue` statement:
///
/// ```text
/// each [0, 1, 2] as n do
///     if !false do
///         break;
///     end
/// end
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided expression can't evaluates to types other than array:
///
/// ```text
/// each true as n do
///     # Invalid
/// end
/// ```
pub struct EachLoopChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> EachLoopChecker<'a> {
    pub const fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl EachLoopChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let expr_t = ExpressionChecker::new(self.ss, self.iterable()).check()?;

        Type::assert_iterable(&expr_t, || self.iterable().span().clone())?;

        // Make sure the array element's type is known
        if expr_t.is_array_with_unknown_elem_t() {
            return Err(Error::UnableToInferType {
                span: self.elem_id().span().clone(),
            });
        }

        let elem_id = self.elem_id().unwrap_identifier().0;
        let elem_t = expr_t.unwrap_array().as_ref().unwrap().as_ref();

        // Check all statements inside the body with a new scope

        self.ss.with_scope(Scope::new_loop_scope(), || {
            self.ss
                .save_symbol_or_else(&elem_id, elem_t.clone(), || unreachable!())
                .unwrap();

            BodyChecker::new(self.ss, self.node).check()
        })?;

        Ok(Type::new("Void"))
    }

    fn iterable(&self) -> &AstNode {
        if let AstNode::Each { iterable, .. } = self.node {
            iterable
        } else {
            unreachable!()
        }
    }

    fn elem_id(&self) -> &AstNode {
        if let AstNode::Each { elem_id, .. } = self.node {
            elem_id
        } else {
            unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn each_loop_statements() {
        assert_is_ok(indoc! {"
                fn main() do
                    each [1, 2, 3] as n do
                        debug n;
                    end

                    var arr = [4, 5, 6];
                    each arr as n do
                        debug n;
                    end
                end
            "});
    }

    #[test]
    fn each_loop_statement_with_non_array_expression() {
        assert_is_err(indoc! {"
                fn main() do
                    each true as n do
                        debug n;
                    end
                end
            "});
    }

    #[test]
    fn accessing_elem_id_outside_scope() {
        assert_is_err(indoc! {"
                fn main() do
                    each [1, 2] as n do
                    end

                    debug n;
                end
            "});
    }

    #[test]
    fn iterating_over_an_empty_array_of_unknown_type() {
        assert_is_err(indoc! {"
                fn main() do
                    each [] as n do
                    end
                end
            "});
    }
}
