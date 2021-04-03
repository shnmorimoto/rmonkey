use std::fmt;

pub struct Token {
    pub literal: String,
    pub type_kind: TokenType,
}

impl Token {
    pub fn new(type_kind: TokenType, literal: String) -> Self {
        Token { literal, type_kind }
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
        }
    }
}
