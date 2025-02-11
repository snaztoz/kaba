use super::{
    error::Result, expression::ExpressionRunner, state::RuntimeState, value::RuntimeValue,
};
use kaba_compiler::{AstNode, AstNodeVariant};

pub struct AssignmentRunner<'src, 'a> {
    ast: &'a AstNode<'src>,
    root: &'a AstNode<'src>,

    state: &'a RuntimeState<'a>,
}

impl<'src, 'a> AssignmentRunner<'src, 'a> {
    pub fn new(
        ast: &'a AstNode<'src>,
        root: &'a AstNode<'src>,
        state: &'a RuntimeState<'a>,
    ) -> Self {
        Self { ast, root, state }
    }

    fn assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                self.state.update_value(name, val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                arr[*index] = val;
            }
        }

        Ok(RuntimeValue::Void)
    }

    fn add_assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                let old_val = self.state.get_value(name)?;
                let new_val = self.math_add(&old_val, &val);

                self.state.update_value(name, new_val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                let old_val = &arr[*index];
                let new_val = self.math_add(old_val, &val);

                arr[*index] = new_val;
            }
        }

        Ok(RuntimeValue::Void)
    }

    fn sub_assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                let old_val = self.state.get_value(name)?;
                let new_val = self.math_sub(&old_val, &val);

                self.state.update_value(name, new_val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                let old_val = &arr[*index];
                let new_val = self.math_sub(old_val, &val);

                arr[*index] = new_val;
            }
        }

        Ok(RuntimeValue::Void)
    }

    fn mul_assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                let old_val = self.state.get_value(name)?;
                let new_val = self.math_mul(&old_val, &val);

                self.state.update_value(name, new_val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                let old_val = &arr[*index];
                let new_val = self.math_mul(old_val, &val);

                arr[*index] = new_val;
            }
        }

        Ok(RuntimeValue::Void)
    }

    fn div_assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                let old_val = self.state.get_value(name)?;
                let new_val = self.math_div(&old_val, &val);

                self.state.update_value(name, new_val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                let old_val = &arr[*index];
                let new_val = self.math_div(old_val, &val);

                arr[*index] = new_val;
            }
        }

        Ok(RuntimeValue::Void)
    }

    fn mod_assign(&self, lhs: &Lhs, val: RuntimeValue) -> Result<RuntimeValue> {
        match lhs {
            Lhs::Identifier(name) => {
                let old_val = self.state.get_value(name)?;
                let new_val = self.math_mod(&old_val, &val);

                self.state.update_value(name, new_val)?;
            }

            Lhs::ArrayIndex { arr_ptr, index } => {
                let arr = &mut self.state.array_arena.borrow_mut()[*arr_ptr];
                if *index >= arr.len() {
                    todo!("index out of bound")
                }

                let old_val = &arr[*index];
                let new_val = self.math_mod(old_val, &val);

                arr[*index] = new_val;
            }
        }

        Ok(RuntimeValue::Void)
    }
}

impl AssignmentRunner<'_, '_> {
    pub fn run(&self) -> Result<RuntimeValue> {
        let lhs_node = match &self.ast.variant {
            AstNodeVariant::Assign { lhs, .. }
            | AstNodeVariant::AddAssign { lhs, .. }
            | AstNodeVariant::SubAssign { lhs, .. }
            | AstNodeVariant::MulAssign { lhs, .. }
            | AstNodeVariant::DivAssign { lhs, .. }
            | AstNodeVariant::ModAssign { lhs, .. } => lhs,

            _ => unreachable!(),
        };

        let lhs = match &lhs_node.as_ref().variant {
            AstNodeVariant::Symbol { name, .. } => Lhs::Identifier(name.to_string()),

            AstNodeVariant::IndexAccess { object, index, .. } => {
                let arr = ExpressionRunner::new(object, self.root, self.state).run()?;
                let idx = ExpressionRunner::new(index, self.root, self.state).run()?;

                Lhs::ArrayIndex {
                    arr_ptr: arr.unwrap_array_ptr(),
                    index: idx.unwrap_integer(),
                }
            }

            _ => unreachable!(),
        };

        let val = match &self.ast.variant {
            AstNodeVariant::Assign { rhs, .. }
            | AstNodeVariant::AddAssign { rhs, .. }
            | AstNodeVariant::SubAssign { rhs, .. }
            | AstNodeVariant::MulAssign { rhs, .. }
            | AstNodeVariant::DivAssign { rhs, .. }
            | AstNodeVariant::ModAssign { rhs, .. } => {
                ExpressionRunner::new(rhs, self.root, self.state).run()?
            }

            _ => unreachable!(),
        };

        match &self.ast.variant {
            AstNodeVariant::Assign { .. } => self.assign(&lhs, val),
            AstNodeVariant::AddAssign { .. } => self.add_assign(&lhs, val),
            AstNodeVariant::SubAssign { .. } => self.sub_assign(&lhs, val),
            AstNodeVariant::MulAssign { .. } => self.mul_assign(&lhs, val),
            AstNodeVariant::DivAssign { .. } => self.div_assign(&lhs, val),
            AstNodeVariant::ModAssign { .. } => self.mod_assign(&lhs, val),

            _ => unreachable!(),
        }
    }

    fn math_add(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l + r),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l + r),
            _ => unreachable!(),
        }
    }

    fn math_sub(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l - r),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l - r),
            _ => unreachable!(),
        }
    }

    fn math_mul(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l * r),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l * r),
            _ => unreachable!(),
        }
    }

    fn math_div(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l / r),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l / r),
            _ => unreachable!(),
        }
    }

    fn math_mod(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match (lhs, rhs) {
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => RuntimeValue::Int(l % r),
            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => RuntimeValue::Float(l % r),
            _ => unreachable!(),
        }
    }
}

enum Lhs {
    Identifier(String),
    ArrayIndex { arr_ptr: usize, index: usize },
}
