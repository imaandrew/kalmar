mod compiler;
mod lexer;
mod optimizer;
mod parser;
mod sem_checker;

use clap::Parser;
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

fn main() -> Result<(), Box<dyn error::Error>> {
    //let cli = Cli::parse();
    let data: String = fs::read_to_string("test.scr")?.parse()?;
    let mut parser = parser::Parser::new(&data);

    let mut stmts = parser.parse(false);

    for s in &stmts {
        println!("{}", s);
    }

    let mut sem = sem_checker::SemChecker::default();
    sem.check_scripts(&stmts);

    optimizer::optimize_stmts(&mut stmts);
    println!("{:#?}", stmts);

    let mut compiler = compiler::Compiler::new();
    let code = compiler.compile(&stmts);

    code.iter().for_each(|x| println!("{:08x?}", x));
    Ok(())
}
