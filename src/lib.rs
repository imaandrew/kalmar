pub mod lexer;

pub fn test() {
    let mut x = lexer::Lexer::new("");
    x.lex();
}
