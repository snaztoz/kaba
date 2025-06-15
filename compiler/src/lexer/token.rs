use super::{error::LexingError, rule};
use logos::{Logos, Span};
use std::fmt::Display;

/// A wrapper around raw [`TokenKind`] that also stores the metadata information
/// of the actual token, such as its position inside the source code.
#[derive(Clone, Debug, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: Span,
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = LexingError)]
#[rustfmt::skip]
pub enum TokenKind<'src> {
    #[regex("[a-zA-Z0-9_]+", rule::lex_symbol)]
    Symbol(&'src str),

    // Literals

    #[regex(r"true|false", callback = rule::lex_bool)]
    Bool(bool),

    #[token("\'", callback = rule::lex_char)]
    Char(char),

    #[regex(r"[0-9]+\.[0-9]+", callback = rule::lex_float)]
    Float(f32),

    #[regex("[0-9]+", priority = 2, callback = rule::lex_integer)]
    Int(u32),

    #[token("\"", callback = rule::lex_string)]
    String(String),

    // Keywords

    #[token("break")]    Break,
    #[token("continue")] Continue,
    #[token("debug")]    Debug,
    #[token("def")]      Def,
    #[token("each")]     Each,
    #[token("else")]     Else,
    #[token("if")]       If,
    #[token("in")]       In,
    #[token("new")]      New,
    #[token("record")]   Record,
    #[token("return")]   Return,
    #[token("var")]      Var,
    #[token("while")]    While,

    // Signs and operators

    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("*")] Mul,
    #[token("/")] Div,
    #[token("%")] Mod,

    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token(",")] Comma,
    #[token(".")] Dot,

    #[token("{")] LBrace,
    #[token("[")] LBrack,
    #[token("(")] LParen,
    #[token("}")] RBrace,
    #[token("]")] RBrack,
    #[token(")")] RParen,

    #[token("=")]  Assign,
    #[token("+=")] AddAssign,
    #[token("-=")] SubAssign,
    #[token("*=")] MulAssign,
    #[token("/=")] DivAssign,
    #[token("%=")] ModAssign,

    #[token("==")] Eq,
    #[token("!=")] Neq,
    #[token(">")]  Gt,
    #[token(">=")] Gte,
    #[token("<")]  Lt,
    #[token("<=")] Lte,

    #[token("&&")] And,
    #[token("||")] Or,
    #[token("!")]  Not,

    #[token("->")] RightPoint,

    // Comments

    #[token("//", callback = rule::lex_comment)]
    Comment(&'src str),

    // This will always be appended as the last token inside token list
    Eof,
}

impl TokenKind<'_> {
    pub const fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "boolean `{b}` literal"),
            Self::Char(_) => write!(f, "char literal"),
            Self::Float(_) => write!(f, "float literal"),
            Self::Int(_) => write!(f, "integer literal"),
            Self::String(_) => write!(f, "string literal"),
            Self::Symbol(s) => write!(f, "{s}"),

            Self::Break => write!(f, "`break` keyword"),
            Self::Continue => write!(f, "`continue` keyword"),
            Self::Debug => write!(f, "`debug` keyword"),
            Self::Def => write!(f, "`def` keyword"),
            Self::Each => write!(f, "`each` keyword"),
            Self::Else => write!(f, "`else` keyword"),
            Self::If => write!(f, "`if` keyword"),
            Self::In => write!(f, "`in` keyword"),
            Self::New => write!(f, "`new` keyword"),
            Self::Record => write!(f, "`record` keyword"),
            Self::Return => write!(f, "`return` keyword"),
            Self::Var => write!(f, "`var` keyword"),
            Self::While => write!(f, "`while` keyword"),

            Self::Add => write!(f, "addition operator (`+`)"),
            Self::Sub => write!(f, "subtraction operator (`-`)"),
            Self::Mul => write!(f, "multiplication operator (`*`)"),
            Self::Div => write!(f, "division operator (`/`)"),
            Self::Mod => write!(f, "modulo operator (`%`)"),

            Self::Colon => write!(f, "colon (`:`)"),
            Self::Semicolon => write!(f, "semicolon (`;`)"),
            Self::Comma => write!(f, "comma (`,`)"),
            Self::Dot => write!(f, "dot (`.`)"),

            Self::LBrace => write!(f, "left brace (`{{`)"),
            Self::LBrack => write!(f, "left bracket (`[`)"),
            Self::LParen => write!(f, "left parentheses (`(`)"),
            Self::RBrace => write!(f, "right bracket (`}}`)"),
            Self::RBrack => write!(f, "right bracket (`]`)"),
            Self::RParen => write!(f, "right parentheses (`)`)"),

            Self::Assign => write!(f, "assign operator (`=`)"),
            Self::AddAssign => write!(f, "add assign operator (`+=`)"),
            Self::SubAssign => write!(f, "sub assign operator (`-=`)"),
            Self::MulAssign => write!(f, "mul assign operator (`*=`)"),
            Self::DivAssign => write!(f, "div assign operator (`/=`)"),
            Self::ModAssign => write!(f, "mod assign operator (`%=`)"),

            Self::Eq => write!(f, "equal operator (`==`)"),
            Self::Neq => write!(f, "not equal operator (`!=`)"),
            Self::Gt => write!(f, "greater than operator (`>`)"),
            Self::Gte => write!(f, "greater than or equal operator (`>=`)"),
            Self::Lt => write!(f, "less than operator (`<`)"),
            Self::Lte => write!(f, "less than or equal operator (`<=`)"),

            Self::And => write!(f, "logical and operator (`&&`)"),
            Self::Or => write!(f, "logical or operator (`||`)"),
            Self::Not => write!(f, "logical not operator (`!`)"),

            Self::RightPoint => write!(f, "right pointing operator (`->`)"),

            Self::Comment(_) => write!(f, "comment"),

            Self::Eof => write!(f, "end-of-file (EOF)"),
        }
    }
}
