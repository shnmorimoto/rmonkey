use std::{collections::HashMap, fmt, sync::Mutex};

use once_cell::sync::Lazy;

pub struct Token {
    pub literal: String,
    pub type_kind: TokenType,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type: {}, Literal: {}", self.type_kind, self.literal)
    }
}

static KEYWORDS: Lazy<Mutex<HashMap<&str, TokenType>>> = Lazy::new(|| {
    let mut keywords = HashMap::new();
    keywords.insert("fn", TokenType::Function);
    keywords.insert("let", TokenType::Let);
    keywords.insert("true", TokenType::True);
    keywords.insert("false", TokenType::False);
    keywords.insert("if", TokenType::If);
    keywords.insert("else", TokenType::Else);
    keywords.insert("return", TokenType::Return);
    Mutex::new(keywords)
});

impl Token {
    pub fn new(type_kind: TokenType, literal: impl Into<String>) -> Self {
        Token {
            literal: literal.into(),
            type_kind: type_kind,
        }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        match KEYWORDS.lock().unwrap().get(&ident) {
            Some(key) => {
                return *key;
            }
            None => {
                return TokenType::Ident;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Literal
    Ident,
    Int,

    // Operator
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    // Delimiter
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keyword
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;
        match self {
            Illegal => write!(f, "ILLEGAL"),
            Eof => write!(f, "EOF"),
            Ident => write!(f, "IDENT"),
            Int => write!(f, "INT"),
            Assign => write!(f, "="),
            Plus => write!(f, "+"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Lparen => write!(f, "("),
            Rparen => write!(f, ")"),
            Lbrace => write!(f, "{{"),
            Rbrace => write!(f, "}}"),
            Function => write!(f, "FUNCTION"),
            Let => write!(f, "FUNCTION"),
            Minus => write!(f, "-"),
            Bang => write!(f, "!"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            True => write!(f, "TRUE"),
            False => write!(f, "FALSE"),
            If => write!(f, "IF"),
            Else => write!(f, "ELSE"),
            Return => write!(f, "RETURN"),
            Eq => write!(f, "EQ"),
            NotEq => write!(f, "NOT_EQ"),
        }
    }
}
