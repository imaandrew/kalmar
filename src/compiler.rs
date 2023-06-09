use std::collections::HashMap;

use crate::lexer::Literal;
use crate::parser::{BinOp, Expr, ExprEnum, Stmt, UnOp};

pub struct Compiler {
    syms: HashMap<String, u32>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
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
            Stmt::IfElse(i, e) => {
                bin.append(&mut self.compile_stmt(*i));
                for expr in e {
                    bin.append(&mut self.compile_stmt(expr));
                }
            }
            Stmt::If(
                Expr {
                    expr: ExprEnum::BinOp(op, l, r),
                    ty: _,
                },
                s,
            ) => {
                match op {
                    BinOp::EqEq => bin.push(10),
                    BinOp::BangEq => bin.push(11),
                    BinOp::Less => bin.push(12),
                    BinOp::Greater => bin.push(13),
                    BinOp::LessEq => bin.push(14),
                    BinOp::GreaterEq => bin.push(15),
                    _ => panic!(),
                }
                bin.append(&mut self.compile_expr(*l));
                bin.append(&mut self.compile_expr(*r));
                bin.append(&mut self.compile_stmt(*s));
                bin.push(0x13)
            }
            Stmt::Else(Some(i), None) => {
                bin.push(0x12);
                bin.append(&mut self.compile_stmt(*i))
            }
            Stmt::Else(None, Some(s)) => {
                bin.push(0x12);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(0x13)
            }
            Stmt::Switch(e, s) => {
                bin.push(0x14);
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(*s));
                bin.push(0x23);
            }
            Stmt::CaseStmt(e, s) => {
                match e.expr {
                    ExprEnum::UnOp(op, e) => {
                        match op {
                            UnOp::EqEq => bin.push(0x16),
                            UnOp::BangEq => bin.push(0x17),
                            UnOp::Less => bin.push(0x18),
                            UnOp::Greater => bin.push(0x19),
                            UnOp::LessEq => bin.push(0x1a),
                            UnOp::GreaterEq => bin.push(0x1b),
                            UnOp::Addr => bin.push(0x10),
                            e => panic!("{:?}", e),
                        }
                        bin.append(&mut self.compile_expr(*e));
                    }
                    ExprEnum::BinOp(op, e, b) => {
                        if matches!(op, BinOp::KwOr | BinOp::KwAnd) {
                            bin.push(0x16);
                        } else if op == BinOp::Range {
                            bin.push(0x21);
                        }
                        bin.append(&mut self.compile_expr(*e));
                        match op {
                            BinOp::KwOr => bin.push(0x1d),
                            BinOp::KwAnd => bin.push(0x1e),
                            _ => (),
                        }
                        bin.append(&mut self.compile_expr(*b));
                        if matches!(op, BinOp::KwOr | BinOp::KwAnd) {
                            bin.push(0x20);
                        }
                    }
                    ExprEnum::Default => bin.push(0x1c),
                    _ => bin.append(&mut self.compile_expr(e)),
                }
                bin.append(&mut self.compile_stmt(*s));
            }
            Stmt::BreakCase => bin.push(0x22),
            e => panic!("Not implemented: {:?}", e),
        }

        bin
    }

    fn compile_expr(&self, expr: Expr) -> Vec<u32> {
        let mut bin = vec![];

        match expr.expr {
            ExprEnum::Identifier(lit) => match lit {
                Literal::Number(n) => bin.push(n.as_u32()),
                Literal::Identifier(i) => bin.push(*self.syms.get(&i).unwrap()),
                _ => todo!(),
            },
            ExprEnum::UnOp(op, expr) => {
                let e = self.compile_expr(*expr);
                assert_eq!(e.len(), 1);
                match op {
                    UnOp::Minus => bin.push(i32_to_u32(*e.first().unwrap() as i32)),
                    _ => todo!(),
                }
            }
            ExprEnum::BinOp(op, l, r) => match op {
                BinOp::KwOr => {
                    bin.append(&mut self.compile_expr(*l));
                    bin.push(0x1d);
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::KwAnd => {
                    bin.append(&mut self.compile_expr(*l));
                    bin.push(0x1e);
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Range => {
                    bin.push(0x21);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                _ => panic!(),
            },
            _ => todo!(),
        }

        bin
    }
}

fn i32_to_u32(int: i32) -> u32 {
    u32::from_be_bytes(int.to_be_bytes())
}
