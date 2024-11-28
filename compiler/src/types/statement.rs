use super::{
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::{Scope, ScopeStack},
    typ::Type,
};
use crate::ast::AstNode;
use logos::Span;

/// Checker for function declarations.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDeclarationChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDeclarationChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDeclarationChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let id = self.unwrap_identifier();
        let params = self.unwrap_params();
        let return_t = self.unwrap_return_type();

        let mut params_t = vec![];
        for (_, tn) in params {
            let t = Type::from_type_notation(tn);

            // Parameter type must exist in the current scope
            if !self.ss.has_type(&t) {
                let (id, span) = tn.unwrap_type_notation();
                return Err(Error::TypeNotExist { id, span });
            }

            // Parameter should not have "Void" type
            if t.is_void() {
                return Err(Error::VoidTypeVariable {
                    span: tn.span().clone(),
                });
            }

            params_t.push(t);
        }

        let return_t = return_t.as_ref().map_or(Ok(Type::new("Void")), |tn| {
            let t = Type::from_type_notation(tn);
            if self.ss.has_type(&t) {
                Ok(t)
            } else {
                let (id, span) = tn.unwrap_type_notation();
                Err(Error::TypeNotExist { id, span })
            }
        })?;

        let fn_t = Type::Callable {
            params_t,
            return_t: Box::new(return_t.clone()),
        };

        let (id, id_span) = id.unwrap_identifier();
        self.ss
            .save_symbol_or_else(&id, fn_t.clone(), || Error::FunctionAlreadyExist {
                id: id.clone(),
                span: id_span,
            })?;

        Ok(fn_t)
    }

    fn unwrap_identifier(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn unwrap_params(&self) -> &[(AstNode, AstNode)] {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
        } else {
            unreachable!()
        }
    }

    fn unwrap_return_type(&self) -> Option<&AstNode> {
        if let AstNode::FunctionDefinition { return_t, .. } = self.node {
            return_t.as_deref()
        } else {
            unreachable!()
        }
    }
}

/// Checker for function definition.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the ScopeStack.
pub struct FunctionDefinitionChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> FunctionDefinitionChecker<'a> {
    pub fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl FunctionDefinitionChecker<'_> {
    pub fn check(&self) -> Result<Type> {
        let id = self.unwrap_identifier();
        let fn_t = self.unwrap_type();
        let params_id = self.unwrap_param_identifiers();

        let (params_t, return_t) = if let Type::Callable { params_t, return_t } = fn_t {
            (params_t, return_t)
        } else {
            unreachable!()
        };

        let params = params_id.iter().cloned().zip(params_t.iter());

        // Entering new scope
        self.ss
            .with_scope(Scope::new_function_scope(*return_t.clone()), || {
                for ((id, id_span), t) in params {
                    self.ss.save_symbol_or_else(&id, t.clone(), || {
                        Error::VariableAlreadyExist {
                            id: id.clone(),
                            span: id_span.clone(),
                        }
                    })?;
                }

                // Check function body
                //
                // We do this last in order to accommodate features such as
                // recursive function call.

                let body_t = BodyChecker::new(self.ss, self.node).check()?;

                if !return_t.is_void() && body_t.is_void() {
                    return Err(Error::FunctionNotReturningValue {
                        expect: *return_t.clone(),
                        span: id.span().clone(),
                    });
                }

                Ok(())
            })?;

        Ok(Type::new("Void"))
    }

    fn unwrap_identifier(&self) -> &AstNode {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            id
        } else {
            unreachable!()
        }
    }

    fn unwrap_type(&self) -> Type {
        if let AstNode::FunctionDefinition { id, .. } = self.node {
            let id_str = &id.unwrap_identifier().0;
            self.ss.get_symbol_type(id_str).unwrap()
        } else {
            unreachable!()
        }
    }

    fn unwrap_param_identifiers(&self) -> Vec<(String, Span)> {
        if let AstNode::FunctionDefinition { params, .. } = self.node {
            params
                .iter()
                .map(|p| p.0.unwrap_identifier())
                .collect::<Vec<_>>()
        } else {
            unreachable!()
        }
    }
}

/// Checker for a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this checker will call
/// the StatementChecker on each statement found in current body.
struct BodyChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> BodyChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl BodyChecker<'_> {
    fn check(&self) -> Result<Type> {
        let body = self.unwrap_body();
        let mut body_t = Type::new("Void");

        for stmt in body {
            let t = StatementChecker::new(self.ss, stmt).check()?;
            if body_t.is_void() {
                body_t = t;
            }
        }

        Ok(body_t)
    }

    fn unwrap_body(&self) -> &[AstNode] {
        match self.node {
            AstNode::FunctionDefinition { body, .. }
            | AstNode::If { body, .. }
            | AstNode::Else { body, .. }
            | AstNode::While { body, .. } => body,

            _ => unreachable!(),
        }
    }
}

/// Checker for a single statement.
///
/// It checks simple statements, such as loop control checking, and also acts as
/// an aggregate for another (more specific) statement checkers, such as the
/// AssignmentChecker.
struct StatementChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> StatementChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl StatementChecker<'_> {
    fn check(&self) -> Result<Type> {
        match self.node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationChecker::new(self.ss, self.node).check()
            }

            AstNode::If { .. } => ConditionalBranchChecker::new(self.ss, self.node).check(),

            AstNode::While { .. } => LoopChecker::new(self.ss, self.node).check(),

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
        let id = self.unwrap_identifier_string();
        let tn = self.unwrap_type();
        let val = self.unwrap_value_type();
        let span = self.unwrap_span();

        let var_t = tn.map(Type::from_type_notation);
        let val_t = ExpressionChecker::new(self.ss, val).check()?;

        if let Some(var_t) = &var_t {
            // The provided type must exist in the current scope
            if !self.ss.has_type(var_t) {
                let (id, span) = tn.unwrap().unwrap_type_notation();
                return Err(Error::TypeNotExist { id, span });
            }

            // Variable should not have "Void" type
            if var_t.is_void() {
                return Err(Error::VoidTypeVariable {
                    span: tn.unwrap().span().clone(),
                });
            }

            // Check whether the value's type is compatible with the variable
            Type::assert_assignable(&val_t, var_t, || span.clone())?;
        }

        self.save_symbol(&id, var_t.unwrap_or(val_t), span)?;

        Ok(Type::new("Void"))
    }

    fn unwrap_identifier_string(&self) -> String {
        if let AstNode::VariableDeclaration { id, .. } = self.node {
            id.unwrap_identifier().0
        } else {
            unreachable!()
        }
    }

    fn unwrap_type(&self) -> Option<&AstNode> {
        if let AstNode::VariableDeclaration { tn, .. } = self.node {
            tn.as_deref()
        } else {
            unreachable!()
        }
    }

    fn unwrap_value_type(&self) -> &AstNode {
        if let AstNode::VariableDeclaration { val, .. } = self.node {
            val
        } else {
            unreachable!()
        }
    }

    fn unwrap_span(&self) -> &Span {
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
        let cond = self.unwrap_condition();
        let or_else = self.unwrap_else_branch();

        let cond_t = ExpressionChecker::new(self.ss, cond).check()?;
        Type::assert_boolean(&cond_t, || cond.span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        if or_else.is_none() {
            return Ok(Type::new("Void"));
        }

        match or_else.unwrap() {
            AstNode::If { .. } => {
                // All conditional branches must returning a value for this whole
                // statement to be considered as returning value

                let branch_return_t =
                    ConditionalBranchChecker::new(self.ss, or_else.unwrap()).check()?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
                }
            }

            AstNode::Else { .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t = self.ss.with_scope(Scope::new_conditional_scope(), || {
                    BodyChecker::new(self.ss, or_else.unwrap()).check()
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

    fn unwrap_condition(&self) -> &AstNode {
        if let AstNode::If { cond, .. } = self.node {
            cond
        } else {
            unreachable!()
        }
    }

    fn unwrap_else_branch(&self) -> Option<&AstNode> {
        if let AstNode::If { or_else, .. } = self.node {
            or_else.as_deref()
        } else {
            unreachable!()
        }
    }
}

/// Checker for loop statement.
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
struct LoopChecker<'a> {
    ss: &'a ScopeStack,
    node: &'a AstNode,
}

impl<'a> LoopChecker<'a> {
    fn new(ss: &'a ScopeStack, node: &'a AstNode) -> Self {
        Self { ss, node }
    }
}

impl LoopChecker<'_> {
    fn check(&self) -> Result<Type> {
        let cond = self.unwrap_condition();

        // Expecting boolean type for the condition

        let cond_t = ExpressionChecker::new(self.ss, cond).check()?;
        Type::assert_boolean(&cond_t, || cond.span().clone())?;

        // Check all statements inside the body with a new scope

        self.ss.with_scope(Scope::new_loop_scope(), || {
            BodyChecker::new(self.ss, self.node).check()
        })?;

        Ok(Type::new("Void"))
    }

    fn unwrap_condition(&self) -> &AstNode {
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
