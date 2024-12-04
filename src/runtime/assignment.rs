use super::{expression::ExpressionRunner, state::RuntimeState, value::RuntimeValue, Result};
use compiler::ast::AstNode;

pub struct AssignmentRunner<'a> {
    ast: &'a AstNode,
    root: &'a AstNode,

    state: &'a RuntimeState<'a>,
}

impl<'a> AssignmentRunner<'a> {
    pub fn new(ast: &'a AstNode, root: &'a AstNode, state: &'a RuntimeState<'a>) -> Self {
        Self { ast, root, state }
    }

    fn assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        match lhs {
            AstNode::Identifier { name, .. } => {
                let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
                self.state.update_value(name, val)?;
            }
            _ => todo!("more expression for value assignment"),
        }
        Ok(RuntimeValue::Void)
    }

    fn add_assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.state.get_value(&name)?;
        let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
        let new_val = self.math_add(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.state.update_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn sub_assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.state.get_value(&name)?;
        let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
        let new_val = self.math_sub(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.state.update_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn mul_assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.state.get_value(&name)?;
        let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
        let new_val = self.math_mul(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.state.update_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn div_assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.state.get_value(&name)?;
        let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
        let new_val = self.math_div(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.state.update_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }

    fn mod_assign(&self, lhs: &AstNode, rhs: &'a AstNode) -> Result<RuntimeValue> {
        let (name, _) = lhs.unwrap_identifier();
        let old_val = self.state.get_value(&name)?;
        let val = ExpressionRunner::new(rhs, self.root, self.state).run()?;
        let new_val = self.math_mod(&old_val, &val);

        match lhs {
            AstNode::Identifier { name, .. } => {
                self.state.update_value(name, new_val)?;
            }
            _ => todo!("more expression for value assignment"),
        }

        Ok(RuntimeValue::Void)
    }
}

impl AssignmentRunner<'_> {
    pub fn run(&self) -> Result<RuntimeValue> {
        match self.ast {
            AstNode::Assign { lhs, rhs, .. } => self.assign(lhs, rhs),
            AstNode::AddAssign { lhs, rhs, .. } => self.add_assign(lhs, rhs),
            AstNode::SubAssign { lhs, rhs, .. } => self.sub_assign(lhs, rhs),
            AstNode::MulAssign { lhs, rhs, .. } => self.mul_assign(lhs, rhs),
            AstNode::DivAssign { lhs, rhs, .. } => self.div_assign(lhs, rhs),
            AstNode::ModAssign { lhs, rhs, .. } => self.mod_assign(lhs, rhs),

            _ => unreachable!(),
        }
    }

    fn math_add(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l + r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) + r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l + f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l + r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_sub(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l - r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) - r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l - f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l - r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_mul(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l * r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) * r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l * f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l * r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_div(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l / r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) / r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l / f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l / r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn math_mod(&self, lhs: &RuntimeValue, rhs: &RuntimeValue) -> RuntimeValue {
        match lhs {
            RuntimeValue::Integer(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Integer(l % r),
                RuntimeValue::Float(r) => RuntimeValue::Float(f64::from(*l) % r),
                _ => unreachable!(),
            },
            RuntimeValue::Float(l) => match rhs {
                RuntimeValue::Integer(r) => RuntimeValue::Float(l % f64::from(*r)),
                RuntimeValue::Float(r) => RuntimeValue::Float(l % r),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
