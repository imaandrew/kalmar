pub mod lexer;
pub mod parser;

pub fn test() {
    let mut x = lexer::Lexer::new("");
    x.lex();
}
