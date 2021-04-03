use crate::lexer::*;
use crate::token::*;
use std::io::{stdout, BufRead, Write};

const PROMPT: &str = ">> ";

pub struct Repl {}

impl Repl {
    pub fn new() -> Self {
        Repl {}
    }
    pub fn start<R: BufRead>(&self, reader: R) {
        self.display_prompt();
        for line in reader.lines() {
            let line = line.unwrap();
            let mut lexer = Lexer::new(line);
            let mut tok = lexer.next_token();
            while tok.type_kind != TokenType::Eof {
                println!("{}", tok);
                tok = lexer.next_token();
            }
            self.display_prompt();
        }
    }

    fn display_prompt(&self) {
        print!("{}", PROMPT);
        stdout().flush().unwrap();
    }
}
