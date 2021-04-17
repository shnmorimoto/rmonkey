use std::fmt;

pub enum Statement {
    Let {
        identifier: Ident,
        expression: Expression,
    },
    Return {
        return_value: Expression,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => write!(f, "Let({}, {})", identifier, expression),
            Statement::Return { return_value } => write!(f, "Return({})", return_value),
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

#[derive(Debug, Clone)]
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
