use crate::token::*;

struct Lexer {
    input: String,
    postion: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
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

    pub fn next_token(&mut self) -> Token {
        let tok: Token;

        self.skip_whilte_space();

        match self.ch {
            '=' => {
                tok = Token::new(TokenType::Assign, self.ch.to_string());
            }
            ';' => {
                tok = Token::new(TokenType::Semicolon, self.ch.to_string());
            }
            '(' => {
                tok = Token::new(TokenType::Lparen, self.ch.to_string());
            }
            ')' => {
                tok = Token::new(TokenType::Rparen, self.ch.to_string());
            }
            ',' => {
                tok = Token::new(TokenType::Comma, self.ch.to_string());
            }
            '+' => {
                tok = Token::new(TokenType::Plus, self.ch.to_string());
            }
            '{' => {
                tok = Token::new(TokenType::Lbrace, self.ch.to_string());
            }
            '}' => {
                tok = Token::new(TokenType::Rbrace, self.ch.to_string());
            }
            '\0' => {
                tok = Token::new(TokenType::Eof, "".to_string());
            }
            _ => {
                if self.is_letter() {
                    let literal = self.read_identifier();
                    let type_kind = Token::lookup_ident(&literal);
                    return Token::new(type_kind, literal);
                } else if self.is_digit() {
                    return Token::new(TokenType::Int, self.read_number());
                } else {
                    tok = Token::new(TokenType::Illegal, self.ch.to_string());
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
";
        let tests = vec![
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Function, "fn".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Lbrace, "{".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Rbrace, "}".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "result".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

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
