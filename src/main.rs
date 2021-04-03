use repl::Repl;

mod lexer;
mod repl;
mod token;
use std::io::stdin;

fn main() {
    let repl = Repl::new();
    let stdin = stdin();
    let reader = stdin.lock();
    repl.start(reader);
}
