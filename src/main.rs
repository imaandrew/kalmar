use clap::Parser;
use std::{
    error,
    fs::{self, File},
    io::{BufWriter, Write},
    path::PathBuf,
    process::exit,
};

use kalmar::{CompilerBuilder, ErrorPrinter};

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

fn main() -> Result<(), Box<dyn error::Error>> {
    let cli = Cli::parse();
    let fname = cli.input.file_name().unwrap().to_str().unwrap();
    let data: String = fs::read_to_string(&cli.input)?.parse()?;
    let mut c = CompilerBuilder::default().input(&data).base(cli.base_addr);

    let s = if let Some(s) = cli.syms {
        fs::read_to_string(s)?
    } else {
        String::new()
    };
    c = c.syms(&s);

    let mut c = c.build();

    let tokens = match c.lex() {
        Ok(t) => t,
        Err(es) => {
            let ep = ErrorPrinter::new(fname, c.literals());
            for e in es {
                ep.print(&e)?;
            }
            exit(1)
        }
    };

    let mut stmts = match c.parse(&tokens) {
        Ok(s) => s,
        Err(e) => {
            let ep = ErrorPrinter::new(fname, c.literals());
            ep.print(&e)?;
            exit(1);
        }
    };

    match c.sem_check(&stmts) {
        Ok(_) => (),
        Err(e) => {
            let ep = ErrorPrinter::new(fname, c.literals());
            ep.print(&e)?;
            exit(1);
        }
    }

    c.optimize(&mut stmts);

    let code = match c.compile(&stmts) {
        Ok(c) => c,
        Err(e) => {
            let ep = ErrorPrinter::new(fname, c.literals());
            ep.print(&e)?;
            exit(1);
        }
    };

    if let Some(o) = &cli.output {
        let o = File::create(o)?;
        let mut o = BufWriter::new(o);
        for i in code {
            o.write_all(&i.to_be_bytes())?;
        }
    }

    /*
    let r: Vec<u8> = std::fs::read(cli.output.unwrap())?;
    let s = decompiler::decompile_script(&r).unwrap();

    println!("{}", s);
    */

    Ok(())
}
