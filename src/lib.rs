use error::KalmarError;
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
            tokens: vec![],
            literals: StringManager::new(self.input),
            stmts: vec![],
            base_addr: self.base_addr,
            code: vec![],
            syms: parse_syms(self.syms).unwrap(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SymbolIndex(u32);

impl SymbolIndex {
    pub fn new(idx: usize) -> Self {
        Self(idx as u32)
    }

    pub fn get_idx(&self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for SymbolIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

#[derive(Default)]
struct StringManager<'a> {
    strings: IndexSet<&'a str>,
    text: &'a str,
}

impl<'a> StringManager<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            strings: IndexSet::new(),
            text,
        }
    }

    fn add(&mut self, lit: &'a str) -> SymbolIndex {
        SymbolIndex::new(self.strings.insert_full(lit).0)
    }

    fn get(&self, idx: SymbolIndex) -> Option<&'a str> {
        self.strings.get_index(idx.get_idx()).copied()
    }
}

pub struct Compiler<'a> {
    verbose: bool,
    input: &'a str,
    tokens: Vec<Token>,
    literals: StringManager<'a>,
    stmts: Vec<Stmt>,
    base_addr: u32,
    code: Vec<u32>,
    syms: Vec<(&'a str, u32)>,
}

impl<'a> Compiler<'a> {
    pub fn lex(mut self) -> Result<Self, KalmarError> {
        let mut lexer = lexer::Lexer::new(self.input, &mut self.literals);
        match lexer.lex() {
            Ok(t) => {
                self.tokens = t;
            }
            Err(e) => e.iter().for_each(|e| println!("{}", e)),
        }
        Ok(self)
    }

    pub fn parse(mut self) -> Result<Self, KalmarError> {
        let mut parser = parser::Parser::new(&self.tokens, &mut self.literals);
        match parser.parse(self.verbose) {
            Ok(s) => self.stmts = s,
            Err(e) => return Err(e),
        }
        Ok(self)
    }

    pub fn sem_check(mut self) -> Result<Self, KalmarError> {
        let mut sem = sem_checker::SemChecker::new(&mut self.literals);
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
        let mut compiler = compiler::Compiler::new(self.base_addr, &mut self.literals);
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
    syms: Vec<(&'a str, u32)>,
}

impl<'a> Decompiler<'a> {
    pub fn parse(mut self) -> Result<Self, KalmarError> {
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
