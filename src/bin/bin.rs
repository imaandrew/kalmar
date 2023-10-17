use clap::Parser;
use kalmar::compiler;
use kalmar::optimizer;
use kalmar::parser;
use kalmar::sem_checker;
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
    //let cli = Cli::parse();
    let data: String = fs::read_to_string("test.scr").unwrap().parse().unwrap();
    let mut parser = parser::Parser::new(&data);

    let mut stmts = parser.parse(false);

    for s in &stmts {
        println!("{}", s);
    }

    let mut sem = sem_checker::SemChecker::default();
    sem.check_scripts(&stmts);

    optimizer::optimize_stmts(&mut stmts);
    println!("{:#?}", stmts);

    let compiler = compiler::Compiler::new();
    let code = compiler.compile(stmts);

    code.iter().for_each(|x| println!("{:08x?}", x));
}
