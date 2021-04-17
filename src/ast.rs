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
            } => write!(
                f,
                "Let({}, {})",
                identifier.to_string(),
                expression.to_string()
            ),
            Statement::Return { return_value } => write!(f, "Return({})", return_value.to_string()),
            Statement::Expression { expression } => {
                write!(f, "Expression({})", expression.to_string())
            }
        }
    }
}

impl Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => format!(
                "let {} = {};",
                identifier.to_string(),
                expression.to_string()
            ),
            Statement::Return { return_value } => format!("return {};", return_value),
            Statement::Expression { expression } => format!("{}", expression.to_string()),
        }
    }
}

pub enum Expression {
    Identifier(Ident),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "Identifier({})", i),
        }
    }
}

impl Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(ident) => format!("{}", &ident.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident(pub String);

impl Ident {
    pub fn new(s: impl Into<String>) -> Self {
        Ident(s.into())
    }

    pub fn to_string(&self) -> String {
        self.0.clone()
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

    pub fn to_string(&self) -> String {
        format!("{}", self.statements.iter().map(|s| s.to_string()).join(""))
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
            program.to_string(),
            "let myVar = anotherVar;".to_string(),
            "program.to_string() wrong. got = {}",
            program.to_string()
        );
    }
}
