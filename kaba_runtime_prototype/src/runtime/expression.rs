use super::{
    assignment::AssignmentRunner,
    error::Result,
    state::{Object, RuntimeState},
    value::RuntimeValue,
};
use crate::runtime::body::BodyRunner;
use kaba_compiler::{AstNode, AstNodeVariant, FunctionParam, Literal};
use std::collections::HashMap;

pub struct ExpressionRunner<'src, 'a> {
    ast: &'a AstNode<'src>,
    root: &'a AstNode<'src>,

    state: &'a RuntimeState<'a>,
}

impl<'src, 'a> ExpressionRunner<'src, 'a> {
    pub fn new(
        ast: &'a AstNode<'src>,
        root: &'a AstNode<'src>,
        state: &'a RuntimeState<'a>,
    ) -> Self {
        Self { ast, root, state }
    }

    fn run_function_call(&self, callee: &'a AstNode, args: &'a [AstNode]) -> Result<RuntimeValue> {
        let f_ptr = match &callee.variant {
            AstNodeVariant::Symbol { name, .. } => self.state.get_value(name).unwrap(),

            AstNodeVariant::FunctionCall { callee, args, .. } => {
                self.run_function_call(callee, args)?
            }
            AstNodeVariant::IndexAccess { object, index, .. } => {
                self.run_index_access(object, index)?
            }

            c => todo!("{c}"),
        };

        let mut evaluated_args: Vec<RuntimeValue> = vec![];
        for arg in args {
            let val = ExpressionRunner::new(arg, self.root, self.state).run()?;
            evaluated_args.push(val);
        }

        self.run_function_ptr_call(f_ptr, &evaluated_args)
    }

    fn run_field_access(&self, object: &'a AstNode, field: &'a AstNode) -> Result<RuntimeValue> {
        let object = ExpressionRunner::new(object, self.root, self.state).run()?;
        let field = field.variant.as_sym_name();

        if let RuntimeValue::Record(ptr) = object {
            let rec = &self.state.objects_arena.borrow()[ptr];
            return Ok(rec.as_record()[field].clone());
        }

        unreachable!()
    }

    fn run_index_access(&self, object: &'a AstNode, index: &'a AstNode) -> Result<RuntimeValue> {
        let object_arr = ExpressionRunner::new(object, self.root, self.state).run()?;
        let index = ExpressionRunner::new(index, self.root, self.state).run()?;

        if let RuntimeValue::Array(ptr) = object_arr {
            if let RuntimeValue::Int(i) = index {
                let arr = &self.state.objects_arena.borrow()[ptr];
                let i = usize::try_from(i).unwrap();

                if i >= arr.as_array().len() {
                    todo!("index out of bound")
                }

                return Ok(arr.as_array()[i].clone());
            }
        }

        unreachable!()
    }

    fn literal_to_value(&self, lit: &'a Literal) -> Result<RuntimeValue> {
        let val = match lit {
            Literal::Void => RuntimeValue::Void,

            Literal::Bool(b) => RuntimeValue::Bool(*b),
            Literal::Int(n) => RuntimeValue::Int(*n),
            Literal::Float(n) => RuntimeValue::Float(*n),
            Literal::Char(c) => RuntimeValue::Char(*c),
            Literal::String(s) => RuntimeValue::String(s.clone()),

            Literal::Array { elems: arr, .. } => {
                let mut elems = vec![];
                for elem in arr {
                    let val = ExpressionRunner::new(elem, self.root, self.state).run()?;
                    elems.push(val);
                }

                self.state
                    .objects_arena
                    .borrow_mut()
                    .push(Object::Array(elems));
                let ptr = self.state.objects_arena.borrow().len() - 1;

                RuntimeValue::Array(ptr)
            }

            Literal::Record { fields } => {
                let mut f = HashMap::new();
                for (name, val) in fields {
                    let name = name.variant.as_sym_name();
                    let val = ExpressionRunner::new(val, self.root, self.state).run()?;
                    f.insert(String::from(name), val);
                }

                self.state
                    .objects_arena
                    .borrow_mut()
                    .push(Object::Record(f));
                let ptr = self.state.objects_arena.borrow().len() - 1;

                RuntimeValue::Record(ptr)
            }
        };

        Ok(val)
    }
}

impl ExpressionRunner<'_, '_> {
    pub fn run(&self) -> Result<RuntimeValue> {
        match &self.ast.variant {
            AstNodeVariant::Assign { .. }
            | AstNodeVariant::AddAssign { .. }
            | AstNodeVariant::SubAssign { .. }
            | AstNodeVariant::MulAssign { .. }
            | AstNodeVariant::DivAssign { .. }
            | AstNodeVariant::ModAssign { .. } => {
                AssignmentRunner::new(self.ast, self.root, self.state).run()
            }

            AstNodeVariant::Or { lhs, rhs, .. }
            | AstNodeVariant::And { lhs, rhs, .. }
            | AstNodeVariant::Eq { lhs, rhs, .. }
            | AstNodeVariant::Neq { lhs, rhs, .. }
            | AstNodeVariant::Gt { lhs, rhs, .. }
            | AstNodeVariant::Gte { lhs, rhs, .. }
            | AstNodeVariant::Lt { lhs, rhs, .. }
            | AstNodeVariant::Lte { lhs, rhs, .. }
            | AstNodeVariant::Add { lhs, rhs, .. }
            | AstNodeVariant::Sub { lhs, rhs, .. }
            | AstNodeVariant::Mul { lhs, rhs, .. }
            | AstNodeVariant::Div { lhs, rhs, .. }
            | AstNodeVariant::Mod { lhs, rhs, .. } => {
                let lhs_val = ExpressionRunner::new(lhs, self.root, self.state).run()?;
                let rhs_val = ExpressionRunner::new(rhs, self.root, self.state).run()?;

                Ok(self.run_binary_op(&lhs_val, &rhs_val))
            }

            AstNodeVariant::Not { expr, .. } | AstNodeVariant::Neg { expr, .. } => {
                let val = ExpressionRunner::new(expr, self.root, self.state).run()?;

                Ok(self.run_unary_op(&val))
            }

            AstNodeVariant::FunctionCall { callee, args, .. } => {
                self.run_function_call(callee, args)
            }
            AstNodeVariant::FieldAccess { object, field } => self.run_field_access(object, field),
            AstNodeVariant::IndexAccess { object, index, .. } => {
                self.run_index_access(object, index)
            }

            AstNodeVariant::Symbol { name, .. } => self.state.get_value(name),
            AstNodeVariant::Literal { lit, .. } => self.literal_to_value(lit),

            _ => unreachable!(),
        }
    }

    fn run_binary_op(&self, lhs_val: &RuntimeValue, rhs_val: &RuntimeValue) -> RuntimeValue {
        match &self.ast.variant {
            AstNodeVariant::Or { .. } => self.run_or(lhs_val, rhs_val),
            AstNodeVariant::And { .. } => self.run_and(lhs_val, rhs_val),
            AstNodeVariant::Eq { .. } => self.run_eq(lhs_val, rhs_val),
            AstNodeVariant::Neq { .. } => self.run_neq(lhs_val, rhs_val),
            AstNodeVariant::Gt { .. } => self.run_gt(lhs_val, rhs_val),
            AstNodeVariant::Gte { .. } => self.run_gte(lhs_val, rhs_val),
            AstNodeVariant::Lt { .. } => self.run_lt(lhs_val, rhs_val),
            AstNodeVariant::Lte { .. } => self.run_lte(lhs_val, rhs_val),
            AstNodeVariant::Add { .. } => self.math_add(lhs_val, rhs_val),
            AstNodeVariant::Sub { .. } => self.math_sub(lhs_val, rhs_val),
            AstNodeVariant::Mul { .. } => self.math_mul(lhs_val, rhs_val),
            AstNodeVariant::Div { .. } => self.math_div(lhs_val, rhs_val),
            AstNodeVariant::Mod { .. } => self.math_mod(lhs_val, rhs_val),

            _ => unreachable!(),
        }
    }

    fn run_unary_op(&self, val: &RuntimeValue) -> RuntimeValue {
        match &self.ast.variant {
            AstNodeVariant::Not { .. } => self.run_not(val),
            AstNodeVariant::Neg { .. } => self.math_neg(val),

            _ => unreachable!(),
        }
    }

    fn run_or(&self, lhs_val: &RuntimeValue, rhs_val: &RuntimeValue) -> RuntimeValue {
        // Use short-circuiting
        match (lhs_val, rhs_val) {
            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => RuntimeValue::Bool(*l || *r),
            _ => RuntimeValue::Bool(false),
        }
    }

    fn run_and(&self, lhs_val: &RuntimeValue, rhs_val: &RuntimeValue) -> RuntimeValue {
        // Use short-circuiting
        match (lhs_val, rhs_val) {
            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => RuntimeValue::Bool(*l && *r),
            _ => RuntimeValue::Bool(false),
        }
    }

    fn run_not(&self, child: &RuntimeValue) -> RuntimeValue {
        match child {
            RuntimeValue::Bool(b) => RuntimeValue::Bool(!b),
            _ => unreachable!(),
        }
    }

    fn run_eq(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs == rhs)
    }

    fn run_neq(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs != rhs)
    }

    fn run_gt(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs > rhs)
    }

    fn run_gte(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs >= rhs)
    }

    fn run_lt(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs < rhs)
    }

    fn run_lte(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        RuntimeValue::Bool(lhs <= rhs)
    }

    fn math_add(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l.wrapping_add(*r)),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l + r),
            _ => unreachable!(),
        }
    }

    fn math_sub(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l.wrapping_sub(*r)),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l - r),
            _ => unreachable!(),
        }
    }

    fn math_mul(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l.wrapping_mul(*r)),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l * r),
            _ => unreachable!(),
        }
    }

    fn math_div(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l.wrapping_div(*r)),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l / r),
            _ => unreachable!(),
        }
    }

    fn math_mod(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l.wrapping_rem(*r)),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l % r),
            _ => unreachable!(),
        }
    }

    fn math_neg(&self, child: &RuntimeValue) -> RuntimeValue {
        match child {
            RuntimeValue::Int(n) => RuntimeValue::Int(-n),
            RuntimeValue::Float(n) => RuntimeValue::Float(-n),
            _ => unreachable!(),
        }
    }

    pub fn run_function_ptr_call(
        &self,
        f_ptr: RuntimeValue,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue> {
        if let RuntimeValue::Function(ptr) = f_ptr {
            let top_level_body = if let AstNodeVariant::Program { body, .. } = &self.root.variant {
                body
            } else {
                unreachable!()
            };

            let f = top_level_body.get(ptr).unwrap();

            if let AstNodeVariant::FunctionDefinition { params, .. } = &f.variant {
                self.state.ss.borrow_mut().push(HashMap::new());

                for (i, FunctionParam { sym, .. }) in params.iter().enumerate() {
                    let sym_name = sym.variant.as_sym_name();
                    let val = &args[i];
                    self.state.store_value(sym_name, val.clone());
                }

                BodyRunner::new(f, self.root, self.state).run()?;
                self.state.resume_execution();

                let val = self.state.return_value();
                self.state.ss.borrow_mut().pop();

                return Ok(val.clone());
            }
        }

        unreachable!();
    }
}
