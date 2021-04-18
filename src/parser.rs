use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use itertools::Itertools;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGRATER,
    SUM,
    PRDOCUT,
    PREFIX,
    CALL,
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let errors = vec![];
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let parser = Parser {
            lexer,
            cur_token,
            peek_token,
            errors,
        };
        parser
    }

    fn peek_error(&mut self, t: TokenType) {
        let s = format!(
            "expected next token to be {}, got {} instead",
            t, self.peek_token
        );
        self.errors.push(s);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.type_kind {
            TokenType::Let => {
                return self.parse_let_statement();
            }
            TokenType::Return => {
                return self.parse_return_statement();
            }
            _ => {
                return self.parse_expression_statement();
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let ident = Ident(self.cur_token.literal.clone());

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let {
            identifier: ident.clone(),
            expression: Expression::Identifier(ident.clone()),
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let ident = Ident(self.cur_token.literal.clone());

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return {
            return_value: Expression::Identifier(ident.clone()),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        match expression {
            Some(expression) => Some(Statement::Expression { expression }),
            None => None,
        }
    }

    fn parse_expression(&self, p: Precedence) -> Option<Expression> {
        match self.cur_token.type_kind {
            TokenType::Ident => Some(self.parse_identifier()),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier(Ident::new(self.cur_token.literal.clone()))
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.type_kind == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.type_kind == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        } else {
            self.peek_error(t);
            return false;
        }
    }

    fn parser_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        let mut program = Program::new(statements);

        while self.cur_token.type_kind != TokenType::Eof {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        program
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parser_program();
        check_parser_errors(&parser);

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        match program.statements.get(0).unwrap() {
            Statement::Expression { expression } => match expression {
                Expression::Identifier(ident) => {
                    assert_eq!(
                        "foobar",
                        &ident.to_string(),
                        "ident value not {}, got={}",
                        "foobar",
                        &ident.to_string()
                    );
                }
                _ => assert!(false, "exp not identifier got={}", expression),
            },
            _ => assert!(
                false,
                "exp not identifier got={}",
                program.statements.get(0).unwrap()
            ),
        }
    }
    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parser_program();
        check_parser_errors(&parser);

        assert_eq!(
            3,
            program.statements.len(),
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        for stmt in program.statements.iter() {
            match stmt {
                Statement::Return { return_value: _ } => (),
                _ => {
                    assert!(false, "s not Return. got={}", stmt);
                }
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parser_program();
        check_parser_errors(&parser);

        assert_ne!(
            0,
            program.statements.len(),
            "parse_program() returned 0 size"
        );

        assert_eq!(
            3,
            program.statements.len(),
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, tt.to_string());
        }
    }

    fn test_let_statement(s: &Statement, name: String) {
        match s {
            Statement::Let {
                identifier,
                expression: _,
            } => {
                assert_eq!(
                    identifier.to_string(),
                    name,
                    "let statement identifier not '{}', got={}",
                    name,
                    identifier
                );
            }
            _ => {
                assert!(false, "s not Let. got={}", s);
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        if p.errors.is_empty() {
            return ();
        }
        let mut errors_message = p.errors.iter().map(|e| format!("parser error: {}", e));
        assert!(
            false,
            "parser has {} errors\n{}",
            p.errors.len(),
            errors_message.join("\n"),
        );
    }
}
