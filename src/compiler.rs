use std::collections::HashMap;

use crate::lexer::Literal;
use crate::parser::{BinOp, Expr, ExprEnum, Stmt};

pub struct Compiler {
    syms: HashMap<String, u32>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            syms: HashMap::new(),
        }
    }

    pub fn symbols(&mut self, syms: HashMap<String, u32>) {
        self.syms = syms;
    }

    pub fn compile(&self, stmts: Vec<Stmt>) -> Vec<u32> {
        let mut code = vec![];
        for s in stmts {
            code.append(&mut self.compile_stmt(s));
        }

        code
    }

    fn compile_stmt(&self, stmt: Stmt) -> Vec<u32> {
        let mut bin = vec![];
        match stmt {
            Stmt::Script(_, s) => bin.append(&mut self.compile_stmt(*s)),
            Stmt::Block(stmts) => {
                for s in stmts {
                    bin.append(&mut self.compile_stmt(s))
                }
            }
            Stmt::Return => bin.push(2),
            Stmt::Label(Literal::Number(n)) => {
                bin.push(3);
                bin.push(n.as_u32())
            }
            Stmt::Goto(Literal::Number(n)) => {
                bin.push(4);
                bin.push(n.as_u32())
            }
            Stmt::Loop(e, s) => {
                bin.push(5);
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(*s));
                bin.push(6)
            }
            Stmt::BreakLoop => bin.push(7),
            _ => todo!(),
        }

        bin
    }

    fn compile_expr(&self, expr: Expr) -> Vec<u32> {
        let mut bin = vec![];

        match expr.expr {
            ExprEnum::Identifier(lit) => match lit {
                Literal::Number(n) => bin.push(n.as_u32()),
                _ => todo!(),
            },
            _ => todo!(),
        }

        bin
    }
}
