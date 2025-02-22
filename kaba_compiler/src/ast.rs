//! This module contains representation of the AST of Kaba program.
//!
//! Root of the tree will always be the [`AstNode<'src>::Program`] that may contains
//! `>= 0` statements.

use logos::Span;
use std::fmt::Display;

pub type NodeId = u32;

#[derive(Debug)]
pub struct AstNode<'src> {
    pub id: NodeId,
    pub variant: AstNodeVariant<'src>,
    pub span: Span,
}

impl AstNode<'_> {
    pub const fn is_lval(&self) -> bool {
        matches!(
            self.variant,
            AstNodeVariant::Symbol { .. } | AstNodeVariant::IndexAccess { .. },
        )
    }

    pub const fn is_function_definition(&self) -> bool {
        matches!(self.variant, AstNodeVariant::FunctionDefinition { .. })
    }

    pub const fn is_record_definition(&self) -> bool {
        matches!(self.variant, AstNodeVariant::RecordDefinition { .. })
    }

    pub fn into_group_inner(self) -> Self {
        if let AstNodeVariant::Group { expr } = self.variant {
            expr.into_group_inner()
        } else {
            self
        }
    }
}

impl Display for AstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl PartialEq for AstNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.variant.eq(&other.variant)
            && self.span.start == other.span.start
            && self.span.end == other.span.end
    }
}

/// The representation of each node that make up a whole Kaba AST.
#[derive(Debug, PartialEq)]
pub enum AstNodeVariant<'src> {
    // The root of all other AstNode<'src> variants
    Program {
        body: Vec<AstNode<'src>>,
    },

    VariableDeclaration {
        sym: Box<AstNode<'src>>,
        tn: Option<Box<AstNode<'src>>>,
        val: Box<AstNode<'src>>,
    },

    If {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
        or_else: Option<Box<AstNode<'src>>>,
    },

    Else {
        body: Vec<AstNode<'src>>,
    },

    While {
        cond: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
    },

    Each {
        elem_sym: Box<AstNode<'src>>,
        iterable: Box<AstNode<'src>>,
        body: Vec<AstNode<'src>>,
    },

    Break,

    Continue,

    FunctionDefinition {
        sym: Box<AstNode<'src>>,
        params: Vec<FunctionParam<'src>>,
        return_tn: Option<Box<AstNode<'src>>>,
        body: Vec<AstNode<'src>>,
    },

    Return {
        expr: Option<Box<AstNode<'src>>>,
    },

    Debug {
        expr: Box<AstNode<'src>>,
    },

    RecordDefinition {
        sym: Box<AstNode<'src>>,
        fields: Vec<RecordField<'src>>,
    },

    Assign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    AddAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    SubAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    MulAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    DivAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    ModAssign {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Or {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    And {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Eq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Neq {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Gt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Gte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Lt {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Lte {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Add {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Sub {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Mul {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Div {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Mod {
        lhs: Box<AstNode<'src>>,
        rhs: Box<AstNode<'src>>,
    },

    Not {
        expr: Box<AstNode<'src>>,
    },

    Neg {
        expr: Box<AstNode<'src>>,
    },

    FunctionCall {
        callee: Box<AstNode<'src>>,
        args: Vec<AstNode<'src>>,
    },

    FieldAccess {
        object: Box<AstNode<'src>>,
        field: Box<AstNode<'src>>,
    },

    IndexAccess {
        object: Box<AstNode<'src>>,
        index: Box<AstNode<'src>>,
    },

    // This variant is only used as the span information holder and then will be
    // removed. It won't be present in the final resulting ASTs.
    Group {
        expr: Box<AstNode<'src>>,
    },

    Symbol {
        name: &'src str,
    },

    TypeNotation {
        tn: TypeNotation<'src>,
    },

    Literal {
        lit: Literal<'src>,
    },
}

impl AstNodeVariant<'_> {
    pub fn as_sym(&self) -> &AstNode {
        match self {
            Self::FunctionDefinition { sym, .. }
            | Self::RecordDefinition { sym, .. }
            | Self::VariableDeclaration { sym, .. } => sym,
            _ => unreachable!(),
        }
    }

    pub fn as_sym_name(&self) -> &str {
        if let AstNodeVariant::Symbol { name } = &self {
            name
        } else {
            unreachable!()
        }
    }

    pub fn as_variable_declaration_tn(&self) -> Option<&AstNode<'_>> {
        if let Self::VariableDeclaration { tn, .. } = self {
            tn.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn as_variable_declaration_val(&self) -> &AstNode<'_> {
        if let Self::VariableDeclaration { val, .. } = self {
            val
        } else {
            unreachable!()
        }
    }

    pub fn as_body_statements(&self) -> &[AstNode] {
        match self {
            Self::Program { body, .. }
            | Self::FunctionDefinition { body, .. }
            | Self::If { body, .. }
            | Self::Else { body, .. }
            | Self::While { body, .. }
            | Self::Each { body, .. } => body,
            _ => unreachable!(),
        }
    }

    pub fn as_function_params(&self) -> &[FunctionParam] {
        if let Self::FunctionDefinition { params, .. } = self {
            params
        } else {
            unreachable!()
        }
    }

    pub fn as_function_return_tn(&self) -> Option<&AstNode> {
        if let Self::FunctionDefinition { return_tn, .. } = self {
            return_tn.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn as_function_call_args(&self) -> &[AstNode<'_>] {
        if let Self::FunctionCall { args, .. } = self {
            args
        } else {
            unreachable!()
        }
    }

    pub fn as_exec_cond(&self) -> &AstNode<'_> {
        match self {
            Self::If { cond, .. } | Self::While { cond, .. } => cond,
            _ => unreachable!(),
        }
    }

    pub fn as_or_else_branch(&self) -> Option<&AstNode<'_>> {
        if let Self::If { or_else, .. } = &self {
            or_else.as_deref()
        } else {
            unreachable!()
        }
    }

    pub fn as_each_loop_elem_sym(&self) -> &AstNode<'_> {
        if let Self::Each { elem_sym, .. } = self {
            elem_sym
        } else {
            unreachable!()
        }
    }

    pub fn as_each_loop_iterable(&self) -> &AstNode<'_> {
        if let Self::Each { iterable, .. } = self {
            iterable
        } else {
            unreachable!()
        }
    }

    pub fn as_record_fields(&self) -> &[RecordField<'_>] {
        if let Self::RecordDefinition { fields, .. } = &self {
            fields
        } else {
            unreachable!()
        }
    }

    pub fn as_accessed_object(&self) -> &AstNode<'_> {
        match self {
            Self::FieldAccess { object, .. }
            | Self::IndexAccess { object, .. }
            | Self::FunctionCall { callee: object, .. } => object,
            _ => unreachable!(),
        }
    }

    pub fn as_accessed_field(&self) -> &AstNode<'_> {
        match self {
            Self::FieldAccess { field, .. } | Self::IndexAccess { index: field, .. } => field,
            _ => unreachable!(),
        }
    }

    pub fn as_lhs(&self) -> &AstNode<'_> {
        match self {
            Self::Eq { lhs, .. }
            | Self::Neq { lhs, .. }
            | Self::Or { lhs, .. }
            | Self::And { lhs, .. }
            | Self::Gt { lhs, .. }
            | Self::Gte { lhs, .. }
            | Self::Lt { lhs, .. }
            | Self::Lte { lhs, .. }
            | Self::Add { lhs, .. }
            | Self::Sub { lhs, .. }
            | Self::Mul { lhs, .. }
            | Self::Div { lhs, .. }
            | Self::Mod { lhs, .. }
            | Self::Assign { lhs, .. }
            | Self::AddAssign { lhs, .. }
            | Self::SubAssign { lhs, .. }
            | Self::MulAssign { lhs, .. }
            | Self::DivAssign { lhs, .. }
            | Self::ModAssign { lhs, .. } => lhs,

            _ => unreachable!(),
        }
    }

    pub fn as_rhs(&self) -> &AstNode<'_> {
        match self {
            Self::Eq { rhs, .. }
            | Self::Neq { rhs, .. }
            | Self::Or { rhs, .. }
            | Self::And { rhs, .. }
            | Self::Gt { rhs, .. }
            | Self::Gte { rhs, .. }
            | Self::Lt { rhs, .. }
            | Self::Lte { rhs, .. }
            | Self::Add { rhs, .. }
            | Self::Sub { rhs, .. }
            | Self::Mul { rhs, .. }
            | Self::Div { rhs, .. }
            | Self::Mod { rhs, .. } => rhs,

            _ => unreachable!(),
        }
    }

    pub fn as_literal(&self) -> &Literal<'_> {
        match self {
            Self::Literal { lit } => lit,
            _ => unreachable!(),
        }
    }

    pub fn as_child_expr(&self) -> &AstNode<'_> {
        match self {
            Self::Not { expr } | Self::Neg { expr } => expr,
            _ => unreachable!(),
        }
    }
}

impl Display for AstNodeVariant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration { .. } => {
                write!(f, "variable declaration")
            }
            Self::If { .. } => {
                write!(f, "`if` statement")
            }
            Self::Else { .. } => {
                write!(f, "`else` statement")
            }
            Self::While { .. } => {
                write!(f, "`while` statement")
            }
            Self::Each { .. } => {
                write!(f, "`each` statement")
            }
            Self::FunctionDefinition { .. } => {
                write!(f, "function definition")
            }
            Self::Break { .. } => {
                write!(f, "`break` statement")
            }
            Self::Continue { .. } => {
                write!(f, "`continue` statement")
            }
            Self::Return { .. } => {
                write!(f, "`return` statement")
            }
            Self::Debug { .. } => {
                write!(f, "`debug` statement")
            }
            Self::RecordDefinition { .. } => {
                write!(f, "record definition")
            }
            Self::Assign { .. } => {
                write!(f, "`assign` expression")
            }
            Self::AddAssign { .. } => {
                write!(f, "`add assign` expression")
            }
            Self::SubAssign { .. } => {
                write!(f, "`sub assign` expression")
            }
            Self::MulAssign { .. } => {
                write!(f, "`mul assign` expression")
            }
            Self::DivAssign { .. } => {
                write!(f, "`div assign` expression")
            }
            Self::ModAssign { .. } => {
                write!(f, "`mod assign` expression")
            }
            Self::Or { .. } => {
                write!(f, "`or` expression")
            }
            Self::And { .. } => {
                write!(f, "`and` expression")
            }
            Self::Eq { .. } => {
                write!(f, "`equal` expression")
            }
            Self::Neq { .. } => {
                write!(f, "`not equal` expression")
            }
            Self::Gt { .. } => {
                write!(f, "`greater than` expression")
            }
            Self::Gte { .. } => {
                write!(f, "`greater than or equal` expression")
            }
            Self::Lt { .. } => {
                write!(f, "`less than` expression")
            }
            Self::Lte { .. } => {
                write!(f, "`less than or equal` expression")
            }
            Self::Add { .. } => {
                write!(f, "`addition` expression")
            }
            Self::Sub { .. } => {
                write!(f, "`subtraction` expression")
            }
            Self::Mul { .. } => {
                write!(f, "`multiplication` expression")
            }
            Self::Div { .. } => {
                write!(f, "`division` expression")
            }
            Self::Mod { .. } => {
                write!(f, "`modulo` expression")
            }
            Self::Not { .. } => {
                write!(f, "`not` expression")
            }
            Self::Neg { .. } => {
                write!(f, "`negation` expression")
            }
            Self::FunctionCall { .. } => {
                write!(f, "function call expression")
            }
            Self::FieldAccess { .. } => {
                write!(f, "field access expression")
            }
            Self::IndexAccess { .. } => {
                write!(f, "index access expression")
            }
            Self::Symbol { .. } => {
                write!(f, "symbol")
            }
            Self::TypeNotation { .. } => {
                write!(f, "type notation")
            }
            Self::Literal { .. } => {
                write!(f, "value literal")
            }

            Self::Program { .. } | Self::Group { .. } => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam<'src> {
    pub sym: AstNode<'src>,
    pub tn: AstNode<'src>,
}

#[derive(Debug, PartialEq)]
pub struct RecordField<'src> {
    pub sym: AstNode<'src>,
    pub tn: AstNode<'src>,
}

#[derive(Debug, PartialEq)]
pub enum TypeNotation<'src> {
    Symbol(&'src str),

    Array {
        elem_tn: Box<AstNode<'src>>,
    },

    Callable {
        params_tn: Vec<AstNode<'src>>,
        return_tn: Box<AstNode<'src>>,
    },
}

impl Display for TypeNotation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Symbol(sym) => write!(f, "{sym}"),

            Self::Array { elem_tn } => {
                write!(f, "[]{elem_tn}")
            }

            Self::Callable {
                params_tn,
                return_tn,
            } => {
                let joined = params_tn
                    .iter()
                    .map(|tn| tn.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({joined}) -> {return_tn}")
            }
        }
    }
}

/// The representation of each value that may exists in a Kaba source code,
/// such as integer or string.
#[derive(Debug, PartialEq)]
pub enum Literal<'src> {
    // A temporary value while the runtime is still using a tree-walk
    // interpreter mode
    Void,

    Bool(bool),
    Int(i32),
    Float(f32),
    Char(char),
    String(String),

    Array {
        elem_tn: Box<AstNode<'src>>,
        elems: Vec<AstNode<'src>>,
    },

    Record {
        fields: Vec<(AstNode<'src>, AstNode<'src>)>,
    },
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),

            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::String(s) => write!(f, "{s}"),

            Self::Array { elems, .. } => {
                let joined = elems
                    .iter()
                    .map(|tn| tn.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "[{joined}]")
            }

            Self::Record { fields } => {
                let joined = fields
                    .iter()
                    .map(|(name, val)| format!("{name}: {val}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "{{ {joined} }}")
            }
        }
    }
}
