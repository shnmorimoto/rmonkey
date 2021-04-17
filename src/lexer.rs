use crate::token::*;

pub struct Lexer {
    input: String,
    postion: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Self {
        let mut lexer = Lexer {
            input: input.into(),
            postion: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.postion = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        } else {
            return self.input.chars().nth(self.read_position).unwrap();
        }
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;

        self.skip_whilte_space();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::Eq, "==");
                } else {
                    tok = Token::new(TokenType::Assign, self.ch);
                }
            }
            ';' => {
                tok = Token::new(TokenType::Semicolon, self.ch);
            }
            '(' => {
                tok = Token::new(TokenType::Lparen, self.ch);
            }
            ')' => {
                tok = Token::new(TokenType::Rparen, self.ch);
            }
            ',' => {
                tok = Token::new(TokenType::Comma, self.ch);
            }
            '+' => {
                tok = Token::new(TokenType::Plus, self.ch);
            }
            '-' => {
                tok = Token::new(TokenType::Minus, self.ch);
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::new(TokenType::NotEq, "!=");
                } else {
                    tok = Token::new(TokenType::Bang, self.ch);
                }
            }
            '/' => {
                tok = Token::new(TokenType::Slash, self.ch);
            }
            '*' => {
                tok = Token::new(TokenType::Asterisk, self.ch);
            }
            '<' => {
                tok = Token::new(TokenType::Lt, self.ch);
            }
            '>' => {
                tok = Token::new(TokenType::Gt, self.ch);
            }
            '{' => {
                tok = Token::new(TokenType::Lbrace, self.ch);
            }
            '}' => {
                tok = Token::new(TokenType::Rbrace, self.ch);
            }
            '\0' => {
                tok = Token::new(TokenType::Eof, "");
            }
            _ => {
                if self.is_letter() {
                    let literal = self.read_identifier();
                    let type_kind = Token::lookup_ident(&literal);
                    return Token::new(type_kind, literal);
                } else if self.is_digit() {
                    return Token::new(TokenType::Int, self.read_number());
                } else {
                    tok = Token::new(TokenType::Illegal, self.ch);
                }
            }
        }
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.postion;
        while self.is_letter() {
            self.read_char();
        }
        self.input[position..self.postion].to_string()
    }

    fn is_letter(&self) -> bool {
        ('a' <= self.ch && self.ch <= 'z') || ('A' <= self.ch && self.ch <= 'Z') || (self.ch == '_')
    }

    fn skip_whilte_space(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.postion;
        while self.is_digit() {
            self.read_char();
        }
        self.input[position..self.postion].to_string()
    }

    fn is_digit(&self) -> bool {
        '0' <= self.ch && self.ch <= '9'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";
        let tests = vec![
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Function, "fn"),
            Token::new(TokenType::Lparen, "("),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::Rparen, ")"),
            Token::new(TokenType::Lbrace, "{"),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Rbrace, "}"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "result"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::Lparen, "("),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::Rparen, ")"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Asterisk, "*"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Gt, ">"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::Lparen, "("),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Rparen, ")"),
            Token::new(TokenType::Lbrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::True, "true"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Rbrace, "}"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::Lbrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::False, "false"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Rbrace, "}"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Eq, "=="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::NotEq, "!="),
            Token::new(TokenType::Int, "9"),
            Token::new(TokenType::Semicolon, ";"),
        ];

        let mut lexer = Lexer::new(input);

        for (i, tt) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                tok.type_kind, tt.type_kind,
                "test[{}] - tokentype wrong. expected={}, got={}, ch={}",
                i, tt.type_kind, tok.type_kind, tok.literal
            );
            assert_eq!(
                tok.literal, tt.literal,
                "test[{}] - literal wrong. expected={}, got={}",
                i, tt.literal, tok.literal
            )
        }
    }
}
