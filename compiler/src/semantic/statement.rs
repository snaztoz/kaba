use super::{
    context::Context,
    error::{Error, Result},
    expression::ExpressionChecker,
    scope::Scope,
    types::Type,
};
use crate::ast::AstNode;
use logos::Span;

// Provide type aliases for `unwrap_node` methods
type Identifier<'a> = &'a AstNode;
type IdentifierString = String;
type TypeNotation<'a> = Option<&'a AstNode>;
type ValueType<'a> = &'a AstNode;
type ParamIdentifiers = Vec<(String, Span)>;
type Body<'a> = &'a [AstNode];
type ConditionExpression<'a> = &'a AstNode;
type ConditionalElseBranch<'a> = Option<&'a AstNode>;

/// Semantic checker for function definition.
///
/// This checker assumes that the data from function declarations (i.e. function
/// signature informations) are already stored in the Context.
pub struct FunctionDefinitionChecker<'a> {
    ctx: &'a Context,
}

impl<'a> FunctionDefinitionChecker<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    pub fn check(&self, node: &AstNode) -> Result<Type> {
        let (id, fn_t, params_id) = self.unwrap_node(node);

        let (params_t, return_t) = if let Type::Callable { params_t, return_t } = fn_t {
            (params_t, return_t)
        } else {
            unreachable!()
        };

        let params = params_id.iter().cloned().zip(params_t.iter());

        // Entering new scope
        self.ctx
            .with_scope(Scope::new_function_scope(*return_t.clone()), || {
                for ((id, id_span), t) in params {
                    self.ctx.save_symbol_or_else(&id, t.clone(), || {
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

                let body_t = BodyChecker::new(self.ctx).check(node)?;

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

    fn unwrap_node(&self, node: &'a AstNode) -> (Identifier, Type, ParamIdentifiers) {
        if let AstNode::FunctionDefinition { id, params, .. } = node {
            let id_str = &id.unwrap_identifier().0;
            let t = self.ctx.get_symbol_type(id_str).unwrap();
            let params_id = params
                .iter()
                .map(|p| p.0.unwrap_identifier())
                .collect::<Vec<_>>();

            (id, t, params_id)
        } else {
            unreachable!()
        }
    }
}

/// Semantic checker for a statement body.
///
/// Statement bodies are consist of `>= 0` statements, so this checker will call
/// the StatementChecker on each statement found in current body.
struct BodyChecker<'a> {
    ctx: &'a Context,
}

impl<'a> BodyChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        let body = self.unwrap_node(node);
        let mut body_t = Type::new("Void");

        for stmt in body {
            let t = StatementChecker::new(self.ctx).check(stmt)?;
            if body_t.is_void() {
                body_t = t;
            }
        }

        Ok(body_t)
    }

    fn unwrap_node(&self, node: &'a AstNode) -> Body {
        match node {
            AstNode::FunctionDefinition { body, .. }
            | AstNode::If { body, .. }
            | AstNode::Else { body, .. }
            | AstNode::While { body, .. } => body,

            _ => unreachable!(),
        }
    }
}

/// Semantic checker for a single statement.
///
/// It checks simple statements, such as loop control checking, and also acts as
/// an aggregate for another (more specific) statement checkers, such as the
/// AssignmentChecker.
struct StatementChecker<'a> {
    ctx: &'a Context,
}

impl<'a> StatementChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        match node {
            AstNode::VariableDeclaration { .. } => {
                VariableDeclarationChecker::new(self.ctx).check(node)
            }

            AstNode::If { .. } => ConditionalBranchChecker::new(self.ctx).check(node),

            AstNode::While { .. } => LoopChecker::new(self.ctx).check(node),

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
            | AstNode::ModAssign { .. } => AssignmentChecker::new(self.ctx).check(node),

            expr => ExpressionChecker::new(self.ctx).check(expr),
        }
    }

    fn check_loop_control(&self, span: &Span) -> Result<Type> {
        if !self.ctx.is_inside_loop() {
            return Err(Error::UnexpectedLoopControl { span: span.clone() });
        }
        Ok(Type::new("Void"))
    }

    fn check_return(&self, expr: &Option<Box<AstNode>>, span: &Span) -> Result<Type> {
        let expr_t = expr
            .as_ref()
            .map(|expr| ExpressionChecker::new(self.ctx).check(expr).unwrap())
            .unwrap_or(Type::new("Void"));

        let return_t = self
            .ctx
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
        let expr_t = ExpressionChecker::new(self.ctx).check(expr)?;
        if expr_t.is_void() {
            return Err(Error::DebugVoid { span: span.clone() });
        }
        Ok(Type::new("Void"))
    }
}

/// Semantic checker for variable declaration statement.
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
    ctx: &'a Context,
}

impl<'a> VariableDeclarationChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        let (id, tn, val, span) = self.unwrap_node(node);

        let var_t = tn.map(Type::from_type_notation);
        let val_t = ExpressionChecker::new(self.ctx).check(val)?;

        if let Some(var_t) = &var_t {
            // The provided type must exist in the current scope
            if !self.ctx.has_type(var_t) {
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

    fn unwrap_node(&self, node: &'a AstNode) -> (IdentifierString, TypeNotation, ValueType, &Span) {
        if let AstNode::VariableDeclaration { id, tn, val, span } = node {
            (id.unwrap_identifier().0, tn.as_deref(), val, span)
        } else {
            unreachable!()
        }
    }

    fn save_symbol(&self, id: &str, t: Type, span: &Span) -> Result<()> {
        self.ctx
            .save_symbol_or_else(id, t, || Error::VariableAlreadyExist {
                id: String::from(id),
                span: span.clone(),
            })
    }
}

/// Semantic checker for conditional branch statement.
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
    ctx: &'a Context,
}

impl<'a> ConditionalBranchChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        let (cond, or_else) = self.unwrap_node(node);

        let cond_t = ExpressionChecker::new(self.ctx).check(cond)?;
        Type::assert_boolean(&cond_t, || cond.span().clone())?;

        // Check all statements inside the body with a new scope

        let return_t = self.ctx.with_scope(Scope::new_conditional_scope(), || {
            BodyChecker::new(self.ctx).check(node)
        })?;

        if or_else.is_none() {
            return Ok(Type::new("Void"));
        }

        match or_else.unwrap() {
            AstNode::If { .. } => {
                // All conditional branches must returning a value for this whole
                // statement to be considered as returning value

                let branch_return_t = self.check(or_else.unwrap())?;

                if !return_t.is_void() && !branch_return_t.is_void() {
                    Ok(return_t)
                } else {
                    Ok(Type::new("Void"))
                }
            }

            AstNode::Else { .. } => {
                // Check all statements inside the body with a new scope

                let branch_return_t =
                    self.ctx.with_scope(Scope::new_conditional_scope(), || {
                        BodyChecker::new(self.ctx).check(or_else.unwrap())
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

    fn unwrap_node(&self, node: &'a AstNode) -> (ConditionExpression, ConditionalElseBranch) {
        if let AstNode::If { cond, or_else, .. } = node {
            (cond, or_else.as_deref())
        } else {
            unreachable!()
        }
    }
}

/// Semantic checker for loop statement.
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
    ctx: &'a Context,
}

impl<'a> LoopChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        let cond = self.unwrap_node(node);

        // Expecting boolean type for the condition

        let cond_t = ExpressionChecker::new(self.ctx).check(cond)?;
        Type::assert_boolean(&cond_t, || cond.span().clone())?;

        // Check all statements inside the body with a new scope

        self.ctx.with_scope(Scope::new_loop_scope(), || {
            BodyChecker::new(self.ctx).check(node)
        })?;

        Ok(Type::new("Void"))
    }

    fn unwrap_node(&self, node: &'a AstNode) -> ConditionExpression {
        if let AstNode::While { cond, .. } = node {
            cond
        } else {
            unreachable!()
        }
    }
}

/// Semantic checker for assignment statements.
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
    ctx: &'a Context,
}

impl<'a> AssignmentChecker<'a> {
    fn new(ctx: &'a Context) -> Self {
        Self { ctx }
    }

    fn check(&self, node: &AstNode) -> Result<Type> {
        match node {
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

        let lhs_t = ExpressionChecker::new(self.ctx).check(lhs)?;
        let rhs_t = ExpressionChecker::new(self.ctx).check(rhs)?;

        Type::assert_assignable(&rhs_t, &lhs_t, || span.clone())?;

        Ok(Type::new("Void"))
    }

    fn check_shorthand_assignment(
        &self,
        lhs: &AstNode,
        rhs: &AstNode,
        span: &Span,
    ) -> Result<Type> {
        let lhs_t = ExpressionChecker::new(self.ctx).check(lhs)?;
        let rhs_t = ExpressionChecker::new(self.ctx).check(rhs)?;

        Type::assert_number(&lhs_t, || lhs.span().clone())?;
        Type::assert_number(&rhs_t, || rhs.span().clone())?;

        self.check_assignment(lhs, rhs, span)
    }
}
