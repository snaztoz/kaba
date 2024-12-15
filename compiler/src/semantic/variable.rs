use super::{
    error::{Error, Result},
    expression::ExpressionAnalyzer,
    state::SharedState,
    tn::TypeNotationAnalyzer,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Analyzer for variable declaration statement.
///
/// ### ✅ Valid Examples
///
/// * Below is the example of a valid, full-form variable declaration:
///
/// ```text
/// var x: Int = 5;
/// ```
///
/// * The type can also be inferred:
///
/// ```text
/// var x = 99;
/// ```
///
/// ### ❌ Invalid Examples
///
/// * Variable can't be created without providing the initial value:
///
/// ```text
/// var x: Int;
/// ```
///
/// * If type notation presents, the provided initial value must also be
///   assignable to the type:
///
/// ```text
/// var x: Int = 5.0;
/// ```
pub struct VariableDeclarationAnalyzer<'a> {
    node: &'a AstNode,
    state: &'a SharedState,
}

impl<'a> VariableDeclarationAnalyzer<'a> {
    pub const fn new(node: &'a AstNode, state: &'a SharedState) -> Self {
        Self { node, state }
    }
}

impl VariableDeclarationAnalyzer<'_> {
    pub fn analyze(&self) -> Result<Type> {
        let val_t = ExpressionAnalyzer::new(self.val(), self.state).analyze()?;

        let var_t = if let Some(tn) = self.tn() {
            self.analyze_tn(tn, &val_t)?
        } else {
            if val_t.is_array_with_unknown_elem_t() {
                return Err(Error::UnableToInferType {
                    span: self.id().span().clone(),
                });
            }
            val_t.morph_default()
        };

        self.save_symbol(&self.id_string(), var_t, self.span())?;

        Ok(Type::Void)
    }

    fn analyze_tn(&self, tn: &AstNode, val_t: &Type) -> Result<Type> {
        let t = TypeNotationAnalyzer::new(tn, self.state).analyze()?;

        // Check if the value type is compatible with the variable
        Type::assert_assignable(val_t, &t, || self.span().clone())?;

        Ok(t)
    }

    fn id(&self) -> &AstNode {
        if let AstNode::VariableDeclaration { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn id_string(&self) -> String {
        if let AstNode::VariableDeclaration { id, .. } = self.node {
            id.unwrap_identifier().0
        } else {
            unreachable!()
        }
    }

    fn tn(&self) -> Option<&AstNode> {
        if let AstNode::VariableDeclaration { tn, .. } = self.node {
            tn.as_deref()
        } else {
            unreachable!()
        }
    }

    fn val(&self) -> &AstNode {
        if let AstNode::VariableDeclaration { val, .. } = self.node {
            val
        } else {
            unreachable!()
        }
    }

    fn span(&self) -> &Span {
        if let AstNode::VariableDeclaration { span, .. } = self.node {
            span
        } else {
            unreachable!()
        }
    }

    fn save_symbol(&self, id: &str, t: Type, span: &Span) -> Result<()> {
        self.state
            .save_sym_or_else(id, t, || Error::SymbolAlreadyExist {
                id: String::from(id),
                span: span.clone(),
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn declaring_variable_with_type_annotation_and_initial_value() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x: Int = 5;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_type_inferring() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x = 5;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_float_literal() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x = -0.5;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_bool_literal() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x = true;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_void_type() {
        assert_is_err(indoc! {"
                fn main() do
                    var x: Void = 5;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_type() {
        assert_is_err(indoc! {"
                fn main() do
                    var x: Int = 5.0;
                end
            "})
    }

    #[test]
    fn declaring_variable_with_non_existing_type() {
        assert_is_err(indoc! {"
                fn main() do
                    var x: NonExistingType = 10;
                end
            "})
    }

    #[test]
    fn redeclaring_variable_in_the_same_scope() {
        assert_is_err(indoc! {"
                fn main() do
                    var x = 5;
                    var x = 10;
                end
            "})
    }

    //
    // Functions
    //

    #[test]
    fn declaring_variable_with_function_pointer_as_value() {
        assert_is_ok(indoc! {"
                fn main() do
                    var x: () -> Int = produce;

                    debug x();
                end

                fn produce(): Int do
                    return 5;
                end
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_param_type() {
        assert_is_err(indoc! {"
                fn main() do
                end

                fn produce(f: (NotExist) -> Void) do
                end
            "});
    }

    #[test]
    fn declaring_callable_type_variable_with_non_existing_return_type() {
        assert_is_err(indoc! {"
                fn main() do
                end

                fn produce(f: () -> NotExist) do
                end
            "});
    }

    //
    // Arrays
    //

    #[test]
    fn declaring_variable_with_array_literal() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr = [1];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal() {
        assert_is_err(indoc! {"
                fn main() do
                    var arr = [];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_array_type_notation() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr: []Int = [5, 9, 10];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_empty_array_literal_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr: []Int = [];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_nested_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr: [][]Int = [[5, 9, 10], [1, 2, 3]];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_nested_empty_arrays_and_type_notation() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr: [][][]Int = [[], []];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_array_literal_of_function_types() {
        assert_is_ok(indoc! {"
                fn main() do
                    var arr = [five, six];
                end

                fn five(): Int do
                    return 5;
                end

                fn six(): Int do
                    return 6;
                end
            "});
    }

    #[test]
    fn declaring_variable_with_incompatible_element_types() {
        assert_is_err(indoc! {"
                fn main() do
                    var arr = [1, 0.5,];
                end
            "});
    }

    #[test]
    fn declaring_variable_with_non_existing_array_type() {
        assert_is_err(indoc! {"
                fn main() do
                    var arr: []NotExist = [];
                end
            "})
    }
}
