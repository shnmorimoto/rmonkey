use crate::token::*;

struct Lexer {
    input: String,
    postion: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            postion: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.postion = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let tok: Token;
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
                tok = Token::new(TokenType::Illegal, "".to_string());
            }
        }
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "=+(){},;";
        let tests = vec![
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Lbrace, "{".to_string()),
            Token::new(TokenType::Rbrace, "}".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Eof, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, tt) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                tok.type_kind, tt.type_kind,
                "test[{}] - tokentype wrong. expected={}, got={}",
                i, tt.type_kind, tok.type_kind
            );
            assert_eq!(
                tok.literal, tt.literal,
                "test[{}] - literal wrong. expected={}, got={}",
                i, tt.literal, tok.literal
            )
        }
    }
}
