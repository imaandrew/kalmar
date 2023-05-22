use clap::Parser;
use kalmar::parser;
use std::{error, fs, path::PathBuf};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Use this file as input
    #[clap(value_parser)]
    input_file: PathBuf,
    #[arg(short, long)]
    verbose: bool,
}

fn masin() -> Result<(), Box<dyn error::Error>> {
    let cli = Cli::parse();
    let data: String = fs::read_to_string(cli.input_file)?.parse()?;
    let mut parser = parser::Parser::new(&data);

    let stmts = parser.parse(cli.verbose);

    println!("{:#?}", stmts);

    Ok(())
}

fn main() {
    let cli = Cli::parse();
    let data: String = fs::read_to_string("test.scr").unwrap().parse().unwrap();
    let mut parser = parser::Parser::new(&data);

    let stmts = parser.parse(cli.verbose);

    println!("{:#?}", stmts);
}
