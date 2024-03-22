use error::KalmarError;
use parser::Stmt;

mod compiler;
mod decompiler;
mod error;
mod lexer;
mod optimizer;
mod parser;
mod sem_checker;

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

#[derive(Default)]
pub struct CompilerBuilder<'a> {
    verbose: bool,
    input: &'a str,
    base_addr: u32,
    syms: &'a str,
}

impl<'a> CompilerBuilder<'a> {
    pub fn new() -> Self {
        Self {
            verbose: false,
            input: "",
            base_addr: 0,
            syms: "",
        }
    }

    pub fn input(mut self, input: &'a str) -> Self {
        self.input = input;
        self
    }

    pub fn base(mut self, base: u32) -> Self {
        self.base_addr = base;
        self
    }

    pub fn syms(mut self, syms: &'a str) -> Self {
        self.syms = syms;
        self
    }

    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    pub fn build(&mut self) -> Compiler<'a> {
        Compiler {
            verbose: self.verbose,
            input: self.input,
            stmts: vec![],
            base_addr: self.base_addr,
            code: vec![],
            syms: parse_syms(self.syms).unwrap(),
        }
    }
}

pub struct Compiler<'a> {
    verbose: bool,
    input: &'a str,
    stmts: Vec<Stmt>,
    base_addr: u32,
    code: Vec<u32>,
    syms: Vec<(&'a str, u32)>,
}

impl<'a> Compiler<'a> {
    pub fn parse(mut self) -> Result<Self, KalmarError> {
        let mut parser = parser::Parser::new(self.input);
        match parser.parse(self.verbose) {
            Ok(s) => self.stmts = s,
            Err(e) => return Err(e),
        }
        Ok(self)
    }

    pub fn sem_check(self) -> Result<Self, KalmarError> {
        let mut sem = sem_checker::SemChecker::default();
        match sem.check_ast(&self.stmts) {
            Ok(_) => Ok(self),
            Err(e) => Err(e),
        }
    }

    pub fn optimize(mut self) -> Self {
        optimizer::optimize_stmts(&mut self.stmts);
        self
    }

    pub fn compile(mut self) -> Self {
        let mut compiler = compiler::Compiler::new(self.base_addr);
        compiler.add_syms(std::mem::take(&mut self.syms));
        self.code = compiler.compile(&self.stmts).unwrap();
        self
    }

    pub fn stmts(&mut self) -> &[Stmt] {
        &self.stmts
    }

    pub fn code(&mut self) -> &[u32] {
        &self.code
    }
}

#[derive(Default)]
pub struct DecompilerBuilder<'a> {
    verbose: bool,
    input: &'a [u8],
    base_addr: u32,
    syms: &'a str,
}

impl<'a> DecompilerBuilder<'a> {
    pub fn new() -> Self {
        Self {
            verbose: false,
            input: &[],
            base_addr: 0,
            syms: "",
        }
    }

    pub fn input(mut self, input: &'a [u8]) -> Self {
        self.input = input;
        self
    }

    pub fn base(mut self, base: u32) -> Self {
        self.base_addr = base;
        self
    }

    pub fn syms(mut self, syms: &'a str) -> Self {
        self.syms = syms;
        self
    }

    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    pub fn build(&mut self) -> Decompiler<'a> {
        Decompiler {
            verbose: self.verbose,
            input: self.input,
            stmts: vec![],
            base_addr: self.base_addr,
            syms: parse_syms(self.syms).unwrap(),
        }
    }
}

pub struct Decompiler<'a> {
    verbose: bool,
    input: &'a [u8],
    stmts: Vec<Stmt>,
    base_addr: u32,
    syms: Vec<(&'a str, u32)>,
}

impl<'a> Decompiler<'a> {
    pub fn parse(mut self) -> Result<Self, KalmarError> {
        match decompiler::decompile_script(self.input) {
            Ok(s) => self.stmts.push(s),
            Err(e) => return Err(e),
        }
        Ok(self)
    }

    pub fn stmts(&mut self) -> &[Stmt] {
        &self.stmts
    }
}
