use itertools::Itertools;
use std::fmt;

pub enum Statement {
    Let {
        identifier: Ident,
        expression: Expression,
    },
    Return {
        return_value: Expression,
    },
    Expression {
        expression: Expression,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => write!(f, "let {} = {};", identifier, expression),
            Statement::Return { return_value } => write!(f, "return {};", return_value),
            Statement::Expression { expression } => {
                write!(f, "{}", expression)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Identifier(Ident),
    IntegerLiteral(i64),
    PrefixExpression {
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean(bool),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "{}", i),
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::PrefixExpression { operator, right } => {
                write!(f, "({}{})", operator, right)
            }
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub String);

impl Ident {
    pub fn new(s: impl Into<String>) -> Self {
        Ident(s.into())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements.iter().map(|s| s.to_string()).join("")
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program::new(vec![Statement::Let {
            identifier: Ident::new("myVar"),
            expression: Expression::Identifier(Ident::new("anotherVar")),
        }]);
        assert_eq!(
            format!("{}", program),
            "let myVar = anotherVar;".to_string(),
            "program.to_string() wrong. got = {}",
            program
        );
    }
}
