use super::{
    body::BodyChecker,
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::{Scope, ScopeStack},
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for a single statement.
///
/// It checks simple statements, such as loop control checking, and also acts as
/// an aggregate for another (more specific) statement checkers, such as the
/// AssignmentChecker.
pub struct StatementChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> StatementChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl StatementChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationChecker::new(self.ss, self.node).check()
            }

            AstNode::If { .. } => ConditionalBranchChecker::new(self.ss, self.node).check(),

            AstNode::While { .. } => WhileLoopChecker::new(self.ss, self.node).check(),

            AstNode::Break { span } | AstNode::Continue { span } => self.check_loop_control(span),

            AstNode::FunctionDefinition { id, .. } => {
                return Err(Error::FunctionDefinitionNotInGlobal {
                    span: id.span().clone(),
                })
            }

            AstNode::Return { expr, span } => self.check_return(expr, span),

            AstNode::Debug { expr, span } => self.check_debug(expr, span),

            AstNode::Assign { .. }
            | AstNode::AddAssign { .. }
            | AstNode::SubAssign { .. }
            | AstNode::MulAssign { .. }
            | AstNode::DivAssign { .. }
            | AstNode::ModAssign { .. } => AssignmentChecker::new(self.ss, self.node).check(),

            expr => ExpressionChecker::new(self.ss, expr).check(),
        }
    }

    fn check_loop_control(&self, span: &Span) -> Result<Type> {
        if !self.ss.is_inside_loop() {
            return Err(Error::UnexpectedLoopControl { span: span.clone() });
        }

        Ok(Type::new("Void"))
    }

    fn check_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| ExpressionChecker::new(self.ss, expr).check().unwrap())
            .unwrap_or(Type::new("Void"));

        let return_t = self
            .ss
            .current_function_return_type()
            .ok_or_else(|| Error::UnexpectedReturnStatement { span: span.clone() })?;

        Type::assert_assignable(&expr_t, &return_t, || span.clone())
            .map_err(|err| Error::ReturnTypeMismatch {
                expect: return_t.clone(),
                get: expr_t,
                span: err.span().clone(),
            })
            .map(|_| return_t)
    }

    fn check_debug(&self, expr: &AstNode, span: &Span) -> Result<Type> {
        let expr_t = ExpressionChecker::new(self.ss, expr).check()?;
        if expr_t.is_void() {
            return Err(Error::DebugVoid { span: span.clone() });
        }

        Ok(Type::new("Void"))
    }
}

/// Checker for variable declaration statement.
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
struct VariableDeclarationChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> VariableDeclarationChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl VariableDeclarationChecker<'_> {
    fn check(&self) -> Result<Type> {
        let var_t = self.tn().map(Type::from_type_notation);
        let val_t = ExpressionChecker::new(self.ss, self.val()).check()?;

        if let Some(var_t) = &var_t {
            self.check_var_tn(var_t, &val_t)?;
        }

        self.save_symbol(&self.id_string(), var_t.unwrap_or(val_t), self.span())?;

        Ok(Type::new("Void"))
    }

    fn check_var_tn(&self, var_t: &Type, val_t: &Type) -> Result<()> {
        // The provided type must exist in the current scope
        if !self.ss.has_type(var_t) {
            let (id, span) = self.tn().unwrap().unwrap_type_notation();
            return Err(Error::TypeNotExist { id, span });
        }

        // Variable should not have "Void" type
        if var_t.is_void() {
            return Err(Error::VoidTypeVariable {
                span: self.tn().unwrap().span().clone(),
            });
        }

        // Check whether the value's type is compatible with the variable
        Type::assert_assignable(val_t, var_t, || self.span().clone())
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
        self.ss
            .save_symbol_or_else(id, t, || Error::VariableAlreadyExist {
                id: String::from(id),
                span: span.clone(),
            })
    }
}

/// Checker for conditional branch statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// if true do
///     # ...
/// end
/// ```
///
/// * This statement can have multiple branches:
///
/// ```text
/// if false do
///     # Won't be executed
/// else if !true do
///     # Won't be executed
/// else do
///     # Will be executed
/// end
/// ```
///
/// * It can be the last statement of a function:
///
/// ```text
/// fn foo(): Int do
///     if false do
///         return 5;
///     else do
///         return 99;
///     end
/// end
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided condition can't be any other than expression that returns a
///   boolean:
///
/// ```text
/// if 1 + 1 do
///     # Invalid
/// end
/// ```
///
/// * If a conditional statement branch returns a value (when it is residing
///   inside a function), then any other branch in the same statement must also
///   returning values as well to make it be able to be the last statement in
///   the function:
///
/// ```text
/// fn foo(): Int do
///     if !true do
///         return 1;
///     else do
///         # Error: This branch should returns something!
///     end
/// end
/// ```
struct ConditionalBranchChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> ConditionalBranchChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl ConditionalBranchChecker<'_> {
    fn check(&self) -> Result<Type> {
        let cond_t = ExpressionChecker::new(self.ss, self.cond()).check()?;
        Type::assert_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        if self.or_else().is_none() {
            // Non-exhaustive branches, set to "Void"
            return Ok(Type::new("Void"));
        }

        match self.or_else().unwrap() {
            AstNode::If { .. } => {
                // All conditional branches must returning a value (exhaustive)
                // for this statement to be considered as returning value

                let branch_return_t =
                    ConditionalBranchChecker::new(self.ss, self.or_else().unwrap()).check()?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
                }
            }

            AstNode::Else { .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
                    BodyChecker::new(self.ss, self.or_else().unwrap()).check()
                })?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
                }
            }

            _ => unreachable!(),
        }
    }

    fn cond(&self) -> &AstNode {
        if let AstNode::If { cond, .. } = self.node {
            cond
        } else {
            unreachable!()
        }
    }

    fn or_else(&self) -> Option<&AstNode> {
        if let AstNode::If { or_else, .. } = self.node {
            or_else.as_deref()
        } else {
            unreachable!()
        }
    }
}

/// Checker for `while` loop statement.
///
/// ### ✅ Valid Examples
///
/// * The provided condition must be an expression that returns a boolean:
///
/// ```text
/// while true do
///     # ...
/// end
/// ```
///
/// * With `break` or `continue` statement:
///
/// ```text
/// while true do
///     if !false do
///         break;
///     end
/// end
/// ```
///
/// ### ❌ Invalid Examples
///
/// * The provided condition can't be any other than expression that returns a
///   boolean:
///
/// ```text
/// while 1 + 1 do
///     # Invalid
/// end
/// ```
struct WhileLoopChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> WhileLoopChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl WhileLoopChecker<'_> {
    fn check(&self) -> Result<Type> {
        // Expecting boolean type for the condition

        let cond_t = ExpressionChecker::new(self.ss, self.cond()).check()?;
        Type::assert_boolean(&cond_t, || self.cond().span().clone())?;

        // Check all statements inside the body with a new scope

        self.ss.with_scope(Scope::new_loop_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        Ok(Type::new("Void"))
    }

    fn cond(&self) -> &AstNode {
        if let AstNode::While { cond, .. } = self.node {
            cond
        } else {
            unreachable!()
        }
    }
}

/// Checker for assignment statements.
///
/// This checker handles the checking of plain assignment and also shorthand
/// assignment.
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
struct AssignmentChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> AssignmentChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl AssignmentChecker<'_> {
    fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::Assign { lhs, rhs, span } => self.check_assignment(lhs, rhs, span),

            AstNode::AddAssign { lhs, rhs, span }
            | AstNode::SubAssign { lhs, rhs, span }
            | AstNode::MulAssign { lhs, rhs, span }
            | AstNode::DivAssign { lhs, rhs, span }
            | AstNode::ModAssign { lhs, rhs, span } => {
                self.check_shorthand_assignment(lhs, rhs, span)
            }

            _ => unreachable!(),
        }
    }

    fn check_assignment(&self, lhs: &AstNode, rhs: &AstNode, span: &Span) -> Result<Type> {
        if !lhs.is_assignable() {
            return Err(Error::InvalidAssignmentLhs {
                lhs: lhs.to_string(),
                span: lhs.span().clone(),
            });
        }

        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::new("Void"))
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ss, lhs).check()?;
        let rhs_t = ExpressionChecker::new(self.ss, rhs).check()?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        self.check_assignment(lhs, rhs, span)
    }
}
