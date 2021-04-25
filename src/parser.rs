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

impl Precedence {
    fn get_precedence(token: TokenType) -> Self {
        match token {
            TokenType::Eq => Precedence::EQUALS,
            TokenType::NotEq => Precedence::EQUALS,
            TokenType::Lt => Precedence::LESSGRATER,
            TokenType::Gt => Precedence::LESSGRATER,
            TokenType::Plus => Precedence::SUM,
            TokenType::Minus => Precedence::SUM,
            TokenType::Slash => Precedence::PRDOCUT,
            TokenType::Asterisk => Precedence::PRDOCUT,
            _ => Precedence::LOWEST,
        }
    }
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

    fn peek_precedence(&self) -> Precedence {
        Precedence::get_precedence(self.peek_token.type_kind)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::get_precedence(self.cur_token.type_kind)
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

    fn parse_expression(&mut self, precdence: Precedence) -> Option<Expression> {
        let left_expression_opt = match self.cur_token.type_kind {
            TokenType::Ident => Some(self.parse_identifier()),
            TokenType::Int => Some(self.parse_integer_literal()),
            TokenType::True | TokenType::False => Some(self.parse_boolean()),
            TokenType::Bang | TokenType::Minus => Some(self.parse_prefix_expression()),
            _ => {
                self.no_prefix_parse_error();
                None
            }
        };

        if left_expression_opt.is_none() {
            return None;
        }

        let mut left = left_expression_opt.unwrap();
        while (!self.peek_token_is(TokenType::Semicolon)) && (precdence < self.peek_precedence()) {
            let left_option = match self.peek_token.type_kind {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {
                    self.next_token();
                    Some(self.parse_infix_expression(&left))
                }
                _ => None,
            };

            if left_option.is_none() {
                return Some(left);
            }

            left = left_option.unwrap();
        }

        Some(left)
    }

    fn parse_boolean(&self) -> Expression {
        Expression::Boolean(self.cur_token.literal.parse::<bool>().unwrap())
    }

    fn parse_infix_expression(&mut self, left: &Expression) -> Expression {
        let operator = self.cur_token.literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence).unwrap();
        Expression::InfixExpression {
            left: Box::new(left.clone()),
            operator,
            right: Box::new(right),
        }
    }

    fn no_prefix_parse_error(&mut self) {
        let s = format!(
            "no prefix parse function for {} found",
            self.cur_token.literal
        );
        self.errors.push(s);
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let cur_token_literal = self.cur_token.literal.clone();
        self.next_token();
        Expression::PrefixExpression {
            operator: cur_token_literal,
            right: Box::new(self.parse_expression(Precedence::PREFIX).unwrap()),
        }
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier(Ident::new(self.cur_token.literal.clone()))
    }

    fn parse_integer_literal(&self) -> Expression {
        Expression::IntegerLiteral(self.cur_token.literal.parse::<i64>().unwrap())
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
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    enum TestValue {
        Int(i64),
        Str(String),
        Bool(bool),
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];
        for tt in tests.iter() {
            let lexer = Lexer::new(tt.0);
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
                    Expression::Boolean(b) => {
                        assert_eq!(tt.1, *b, "boolean value not {}, got={}", tt.1, b);
                    }
                    _ => assert!(false, "exp not bool got={}", expression),
                },
                _ => assert!(
                    false,
                    "statement[0] not exp got={}",
                    program.statements.get(0).unwrap()
                ),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for tt in tests.iter() {
            let lexer = Lexer::new(tt.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parser_program();
            check_parser_errors(&parser);

            let actual = program.to_string();
            assert_eq!(actual, tt.1, "expected={}, got={}", tt.1, actual);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", TestValue::Int(5), "+", TestValue::Int(5)),
            ("5 - 5;", TestValue::Int(5), "-", TestValue::Int(5)),
            ("5 * 5;", TestValue::Int(5), "*", TestValue::Int(5)),
            ("5 / 5;", TestValue::Int(5), "/", TestValue::Int(5)),
            ("5 > 5;", TestValue::Int(5), ">", TestValue::Int(5)),
            ("5 < 5;", TestValue::Int(5), "<", TestValue::Int(5)),
            ("5 == 5;", TestValue::Int(5), "==", TestValue::Int(5)),
            ("5 != 5;", TestValue::Int(5), "!=", TestValue::Int(5)),
            (
                "true == true",
                TestValue::Bool(true),
                "==",
                TestValue::Bool(true),
            ),
            (
                "true != false",
                TestValue::Bool(true),
                "!=",
                TestValue::Bool(false),
            ),
            (
                "false == false",
                TestValue::Bool(false),
                "==",
                TestValue::Bool(false),
            ),
        ];

        for tt in infix_tests.iter() {
            let lexer = Lexer::new(tt.0);
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
                Statement::Expression { expression } => {
                    test_infix_expression(tt.1.clone(), tt.2.to_string(), tt.3.clone(), expression)
                }
                _ => assert!(
                    false,
                    "statement[0] not exp got={}",
                    program.statements.get(0).unwrap()
                ),
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![
            ("!5;", "!", TestValue::Int(5)),
            ("-15;", "-", TestValue::Int(15)),
            ("!true;", "!", TestValue::Bool(true)),
            ("!false;", "!", TestValue::Bool(false)),
        ];
        for tt in prefix_tests.iter() {
            let lexer = Lexer::new(tt.0);
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
                    Expression::PrefixExpression { operator, right } => {
                        assert_eq!(tt.1, operator);
                        test_literal_expression(tt.2.clone(), right);
                    }
                    _ => assert!(false, "exp operator not prefix got={}", expression),
                },
                _ => assert!(
                    false,
                    "statement[0] not exp got={}",
                    program.statements.get(0).unwrap()
                ),
            }
        }
    }

    fn test_integral_literal(value: i64, expression: &Expression) {
        match expression {
            Expression::IntegerLiteral(i) => {
                assert_eq!(value, *i, "integral value not {}, got={}", value, i);
            }
            _ => {
                assert!(false, "exp not integral literal, got={}", expression);
            }
        }
    }

    fn test_identifier_literal(value: String, expression: &Expression) {
        match expression {
            Expression::Identifier(ident) => {
                assert_eq!(value, ident.0, "ident value not {}, got={}", value, ident.0);
            }
            _ => {
                assert!(false, "exp not ident, got={}", expression);
            }
        }
    }

    fn test_boolean_literal(value: bool, expression: &Expression) {
        match expression {
            Expression::Boolean(b) => {
                assert_eq!(value, *b, "bool value not {}, got={}", value, b);
            }
            _ => {
                assert!(false, "exp not bool, got={}", expression);
            }
        }
    }

    fn test_literal_expression(expected: TestValue, expression: &Expression) {
        match expected {
            TestValue::Int(i) => test_integral_literal(i, expression),
            TestValue::Str(s) => test_identifier_literal(s, expression),
            TestValue::Bool(b) => test_boolean_literal(b, expression),
        }
    }

    fn test_infix_expression(
        left_value: TestValue,
        operator_value: String,
        right_value: TestValue,
        expression: &Expression,
    ) {
        match expression {
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => {
                test_literal_expression(left_value, &left);
                assert_eq!(
                    &operator_value, operator,
                    "operator is not {}, got={}",
                    operator_value, operator
                );
                test_literal_expression(right_value, &right);
            }
            _ => assert!(false, "exp operator not infix got={}", expression),
        }
    }

    #[test]
    fn test_integer_literal() {
        let input = "5;";
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
                Expression::IntegerLiteral(i) => {
                    assert_eq!(5, *i, "literal value not {}, got={}", 5, i);
                }
                _ => assert!(false, "exp not integer got={}", expression),
            },
            _ => assert!(
                false,
                "statement[0] not exp got={}",
                program.statements.get(0).unwrap()
            ),
        }
    }

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
                "statement[0] not exp got={}",
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
