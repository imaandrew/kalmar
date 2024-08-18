use std::collections::HashMap;

pub use error::ErrorPrinter;
use error::{DecompilerError, KalmarError};
use indexmap::IndexSet;
use lexer::Token;
use parser::Stmt;

mod compiler;
mod decompiler;
mod error;
mod lexer;
mod optimizer;
mod parser;
mod sem_checker;

fn parse_syms(s: &str) -> Result<HashMap<&str, u32>, ()> {
    let mut v = HashMap::new();
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
            Ok(n) => {
                v.insert(sym, n);
            }
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
            literals: StringManager::new(self.input),
            base_addr: self.base_addr,
            syms: parse_syms(self.syms).unwrap(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SymbolIndex(u32);

impl SymbolIndex {
    fn new(idx: usize) -> Self {
        Self(idx as u32)
    }

    fn get_idx(&self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for SymbolIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

pub struct StringManager<'a> {
    strings: IndexSet<&'a str>,
    text: &'a str,
    lines: Vec<&'a str>,
}

impl<'a> StringManager<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            strings: IndexSet::new(),
            text,
            lines: text.lines().collect(),
        }
    }

    fn add(&mut self, lit: &'a str) -> SymbolIndex {
        SymbolIndex::new(self.strings.insert_full(lit).0)
    }

    fn get(&self, idx: SymbolIndex) -> Option<&'a str> {
        self.strings.get_index(idx.get_idx()).copied()
    }

    fn err_context(&self, line_num: usize) -> &str {
        self.lines.get(line_num).unwrap()
    }
}

pub struct Compiler<'a> {
    verbose: bool,
    literals: StringManager<'a>,
    base_addr: u32,
    syms: HashMap<&'a str, u32>,
}

impl<'a> Compiler<'a> {
    pub fn lex(&mut self) -> Result<Vec<Token>, Vec<KalmarError>> {
        let mut lexer = lexer::Lexer::new(&mut self.literals);
        lexer.lex()
    }

    pub fn parse(&mut self, tokens: &[Token]) -> Result<Vec<Stmt>, KalmarError> {
        let mut parser = parser::Parser::new(tokens, &mut self.literals);
        parser.parse(self.verbose)
    }

    pub fn sem_check(&mut self, stmts: &[Stmt]) -> Result<(), Vec<KalmarError>> {
        let mut sem = sem_checker::SemChecker::new(&mut self.literals, &self.syms);
        sem.check_ast(stmts)
    }

    pub fn optimize(&mut self, stmts: &mut [Stmt]) {
        optimizer::optimize_stmts(stmts);
    }

    pub fn compile(&mut self, stmts: &[Stmt]) -> Result<Vec<u32>, KalmarError> {
        let mut compiler: compiler::Compiler =
            compiler::Compiler::new(self.base_addr, &mut self.literals);
        compiler.add_syms(std::mem::take(&mut self.syms));
        compiler.compile(stmts)
    }

    pub fn literals(&self) -> &StringManager<'a> {
        &self.literals
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
            literals: StringManager::new(""),
            verbose: self.verbose,
            input: self.input,
            stmts: vec![],
            base_addr: self.base_addr,
            syms: parse_syms(self.syms).unwrap(),
        }
    }
}

pub struct Decompiler<'a> {
    literals: StringManager<'a>,
    verbose: bool,
    input: &'a [u8],
    stmts: Vec<Stmt>,
    base_addr: u32,
    syms: HashMap<&'a str, u32>,
}

impl<'a> Decompiler<'a> {
    pub fn parse(mut self) -> Result<Self, DecompilerError> {
        let mut de = decompiler::Decompiler::new(&mut self.literals);
        match de.decompile_script(self.input) {
            Ok(s) => self.stmts.push(s),
            Err(e) => return Err(e),
        }
        Ok(self)
    }

    pub fn stmts(&mut self) -> &[Stmt] {
        &self.stmts
    }
}
