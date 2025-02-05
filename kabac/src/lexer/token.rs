use super::{error::LexingError, rule};
use logos::{Logos, Span};
use std::fmt::Display;

/// A wrapper around raw [`TokenKind`] that also store the metadata information
/// of a token, such as its actual position inside the source code.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// The list of all token kinds that may exists in a valid Kaba source code.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+", error = LexingError)]
#[rustfmt::skip]
pub enum TokenKind {
    #[regex("[a-zA-Z0-9_]+", rule::lex_symbol)]
    Symbol(String),

    //
    // Literals
    //

    #[regex("[0-9]+", priority = 2, callback = rule::lex_integer)]
    Int(u32),

    #[regex(r"[0-9]+\.[0-9]+", callback = rule::lex_float)]
    Float(f32),

    #[regex(r"true|false", callback = rule::lex_bool)]
    Bool(bool),

    #[token("\'", callback = rule::lex_char)]
    Char(char),

    #[token("\"", callback = rule::lex_string)]
    String(String),

    //
    // Keywords
    //

    #[token("var")]      Var,
    #[token("if")]       If,
    #[token("else")]     Else,
    #[token("while")]    While,
    #[token("each")]     Each,
    #[token("break")]    Break,
    #[token("continue")] Continue,
    #[token("in")]       In,
    #[token("def")]      Def,
    #[token("return")]   Return,
    #[token("record")]   Record,
    #[token("debug")]    Debug,

    //
    // Signs and operators
    //

    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("*")] Mul,
    #[token("/")] Div,
    #[token("%")] Mod,

    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token(",")] Comma,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBrack,
    #[token("]")] RBrack,
    #[token("{")] LBrace,
    #[token("}")] RBrace,

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

    #[token("||")] Or,
    #[token("&&")] And,
    #[token("!")]  Not,

    #[token("->")] RightPoint,

    // Comments

    #[token("//", callback = rule::lex_comment)]
    Comment(String),

    // This will always be appended as the last token inside token list
    Eof,
}

impl TokenKind {
    pub const fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Int(_) => write!(f, "integer literal"),
            Self::Float(_) => write!(f, "float literal"),
            Self::Bool(b) => write!(f, "boolean `{b}` literal"),
            Self::Char(_) => write!(f, "char literal"),
            Self::String(_) => write!(f, "string literal"),

            Self::Var => write!(f, "`var` keyword"),
            Self::If => write!(f, "`if` keyword"),
            Self::Else => write!(f, "`else` keyword"),
            Self::While => write!(f, "`while` keyword"),
            Self::Each => write!(f, "`each` keyword"),
            Self::Break => write!(f, "`break` keyword"),
            Self::Continue => write!(f, "`continue` keyword"),
            Self::In => write!(f, "`in` keyword"),
            Self::Def => write!(f, "`def` keyword"),
            Self::Return => write!(f, "`return` keyword"),
            Self::Record => write!(f, "`record` keyword"),
            Self::Debug => write!(f, "`debug` keyword"),

            Self::Add => write!(f, "addition operator (`+`)"),
            Self::Sub => write!(f, "subtraction operator (`-`)"),
            Self::Mul => write!(f, "multiplication operator (`*`)"),
            Self::Div => write!(f, "division operator (`/`)"),
            Self::Mod => write!(f, "modulo operator (`%`)"),
            Self::Colon => write!(f, "colon (`:`)"),
            Self::Semicolon => write!(f, "semicolon (`;`)"),
            Self::Comma => write!(f, "comma (`,`)"),
            Self::LParen => write!(f, "left parentheses (`(`)"),
            Self::RParen => write!(f, "right parentheses (`)`)"),
            Self::LBrack => write!(f, "left bracket (`[`)"),
            Self::RBrack => write!(f, "right bracket (`]`)"),
            Self::LBrace => write!(f, "left brace (`{{`)"),
            Self::RBrace => write!(f, "right bracket (`}}`)"),
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
            Self::Or => write!(f, "logical or operator (`||`)"),
            Self::And => write!(f, "logical and operator (`&&`)"),
            Self::Not => write!(f, "logical not operator (`!`)"),

            Self::RightPoint => write!(f, "right pointing operator (`->`)"),

            Self::Comment(_) => write!(f, "comment"),

            Self::Eof => write!(f, "end-of-file (EOF)"),
        }
    }
}
