use clap::Parser;
use kalmar::lexer;
use std::{error, fs, path::PathBuf};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Use this file as input
    #[clap(value_parser)]
    input_file: PathBuf,
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let cli = Cli::parse();
    let data: String = fs::read_to_string(cli.input_file)?.parse()?;
    let mut lexer = lexer::Lexer::new(&data);
    lexer.lex();

    println!("{:#?}", lexer.tokens);

    Ok(())
}
