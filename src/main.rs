mod compiler;
//mod error;
mod lexer;
mod optimizer;
mod parser;
mod sem_checker;

use clap::Parser;
use std::{
    error as serror,
    fs::{self, File},
    io::{BufWriter, Write},
    path::PathBuf,
};

//use error::Error;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Use <input> as input
    #[clap(value_parser)]
    input: PathBuf,
    /// Write output to <output>
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Read symbols from <syms>
    #[arg(short, long)]
    syms: Option<PathBuf>,
    // Address that script will be loaded at
    #[arg(short, long)]
    base_addr: u32,
    /// Print AST
    #[arg(long)]
    print_ast: bool,
}

fn parse_syms(s: &str) -> Result<Vec<(&str, u32)>, ()> {
    let mut v = vec![];
    for line in s.lines() {
        let mut l = line.split('=');
        let sym = l.next().unwrap().trim();
        let num = l.next().unwrap().trim();
        let num = if let Some(n) = num.strip_prefix("0x") {
            u32::from_str_radix(n, 16)
        } else {
            num.parse()
        };

        match num {
            Ok(n) => v.push((sym, n)),
            Err(_) => eprintln!("Invalid symbol file line: {}", line),
        }
    }
    Ok(v)
}

fn main() -> Result<(), Box<dyn serror::Error>> {
    let cli = Cli::parse();
    let data: String = fs::read_to_string(cli.input)?.parse()?;
    let mut parser = parser::Parser::new(&data);
    //let printer = error::ContextPrinter::new(&data);

    let mut stmts = match parser.parse(false) {
        Ok(s) => s,
        Err(e) => {
            //printer.print(&e)?;
            std::process::exit(1);
        }
    };

    let mut sem = sem_checker::SemChecker::default();
    if let Err(e) = sem.check_ast(&stmts) {
        //printer.print(&e)?;
        std::process::exit(1);
    }

    optimizer::optimize_stmts(&mut stmts);

    if cli.print_ast {
        for s in &stmts {
            println!("{:#?}", s);
        }
    }

    let mut compiler = compiler::Compiler::new(cli.base_addr);
    let mut syms = String::new();
    if let Some(s) = cli.syms {
        syms = fs::read_to_string(s)?;
    }
    compiler.add_syms(parse_syms(&syms).unwrap());
    let code = compiler.compile(&stmts);

    if let Some(o) = cli.output {
        let o = File::create(o)?;
        let mut o = BufWriter::new(o);
        for i in &code {
            o.write_all(&i.to_be_bytes())?;
        }
    }

    Ok(())
}
