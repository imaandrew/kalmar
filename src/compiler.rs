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
            Stmt::Script(_, s) => {
                bin.append(&mut self.compile_stmt(*s));
                bin.push(1);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    bin.append(&mut self.compile_stmt(s))
                }
            }
            Stmt::Return => bin.push(2),
            Stmt::Label(n) => {
                bin.push(3);
                bin.push(n)
            }
            Stmt::Goto(n) => {
                bin.push(4);
                bin.push(n)
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
            Stmt::Expr(e) => bin.append(&mut self.compile_expr(e)),
            Stmt::Thread(s) => {
                bin.push(0x56);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(0x57);
            }
            Stmt::ChildThread(s) => {
                bin.push(0x58);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(0x59);
            }
            Stmt::Jump(i) => {
                bin.push(0x4a);
                bin.push(match i {
                    Literal::Number(n) => n.as_u32(),
                    _ => todo!(),
                });
            }
            Stmt::If(_, _) | Stmt::Else(_, _) => panic!(),
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
            ExprEnum::Array(ident, index) => {
                let ident = match ident.expr {
                    ExprEnum::Identifier(Literal::Identifier(i)) => i,
                    _ => panic!(),
                };

                let index = match index.expr {
                    ExprEnum::Identifier(Literal::Number(n)) => n.as_u32(),
                    _ => panic!(),
                };

                bin.push(get_var(&ident, index));
            }
            ExprEnum::FuncCall(func, args) => {
                let addr = match func.expr {
                    ExprEnum::Identifier(Literal::Identifier(i)) => self.get_func(&i).unwrap(),
                    _ => panic!(),
                };
                bin.push(addr);
                for arg in args {
                    bin.append(&mut self.compile_expr(arg));
                }
            }
            e => panic!("Not implemented {:?}", e),
        }

        bin
    }

    fn get_func(&self, func: &str) -> Option<u32> {
        match func {
            "exec" => Some(0x44),
            "exec_wait" => Some(0x46),
            "bind" => Some(0x47),
            "unbind" => Some(0x48),
            "kill" => Some(0x49),
            "set_priority" => Some(0x4b),
            "set_timescale" => Some(0x4c),
            "set_group" => Some(0x4d),
            "bind_lock" => Some(0x4e),
            "suspend_all" => Some(0x4f),
            "resume_all" => Some(0x50),
            "suspend_others" => Some(0x51),
            "resume_others" => Some(0x52),
            "suspend" => Some(0x53),
            "resume" => Some(0x54),
            "does_exist" => Some(0x55),
            e => self.syms.get(e).copied(),
        }
    }
}

fn get_var(ident: &str, index: u32) -> u32 {
    match ident {
        "var" => index + 30000000,
        "map_var" => index + 50000000,
        "flag" => index + 70000000,
        "map_flag" => index + 90000000,
        "area_flag" => index + 110000000,
        "game_flag" => index + 130000000,
        "area_byte" => index + 150000000,
        "game_byte" => index + 170000000,
        _ => todo!(),
    }
}

fn i32_to_u32(int: i32) -> u32 {
    u32::from_be_bytes(int.to_be_bytes())
}
