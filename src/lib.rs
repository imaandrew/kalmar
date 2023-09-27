pub mod compiler;
pub mod lexer;
pub mod optimizer;
pub mod parser;
pub mod sem_checker;

pub fn test() {
    let mut x = lexer::Lexer::new("");
    x.lex();
}
