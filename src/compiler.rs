use std::collections::HashMap;

use crate::lexer::Literal;
use crate::parser::{BinOp, Expr, Stmt, UnOp};

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
                bin.push(n.as_u32())
            }
            Stmt::Goto(n) => {
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
            Stmt::If(e, s) => {
                match e {
                    Expr::BinOp(_, _, _) => bin.append(&mut self.compile_expr(e)),
                    Expr::UnOp(UnOp::Bang, e) => {
                        let mut e = self.compile_expr(*e);
                        e[0] = match e[0] {
                            0xa => 0xb,
                            0xb => 0xa,
                            0xc => 0xf,
                            0xd => 0xe,
                            0xe => 0xd,
                            0xf => 0xc,
                            0x10 => 0x11,
                            0x11 => 0x10,
                            _ => unreachable!(),
                        };
                        bin.append(&mut e);
                    }
                    _ => unreachable!(),
                }
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
                match e {
                    Expr::UnOp(op, e) => {
                        match op {
                            UnOp::Equal => bin.push(0x16),
                            UnOp::NotEqual => bin.push(0x17),
                            UnOp::Less => bin.push(0x18),
                            UnOp::Greater => bin.push(0x19),
                            UnOp::LessEq => bin.push(0x1a),
                            UnOp::GreaterEq => bin.push(0x1b),
                            UnOp::Ampersand => bin.push(0x10),
                            _ => unreachable!(),
                        }
                        bin.append(&mut self.compile_expr(*e));
                    }
                    Expr::BinOp(op, e, b) => {
                        if matches!(op, BinOp::Or | BinOp::And) {
                            bin.push(0x16);
                        } else if op == BinOp::Range {
                            bin.push(0x21);
                        }
                        bin.append(&mut self.compile_expr(*e));
                        match op {
                            BinOp::Or => bin.push(0x1d),
                            BinOp::And => bin.push(0x1e),
                            _ => (),
                        }
                        bin.append(&mut self.compile_expr(*b));
                        if matches!(op, BinOp::Or | BinOp::And) {
                            bin.push(0x20);
                        }
                    }
                    _ => unreachable!(),
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
            Stmt::Else(_, _) => unreachable!(),
        }

        bin
    }

    fn compile_expr(&self, expr: Expr) -> Vec<u32> {
        let mut bin = vec![];

        match expr {
            Expr::Identifier(lit) => match lit {
                Literal::Number(n) => bin.push(n.as_u32()),
                Literal::Identifier(i) => bin.push(*self.syms.get(&i).unwrap()),
                _ => todo!(),
            },
            Expr::UnOp(op, expr) => {
                let e = self.compile_expr(*expr);
                assert_eq!(e.len(), 1);
                match op {
                    UnOp::Minus => bin.push(i32_to_u32(*e.first().unwrap() as i32)),
                    _ => todo!(),
                }
            }
            Expr::BinOp(op, l, r) => match op {
                BinOp::BitOr => {
                    bin.append(&mut self.compile_expr(*l));
                    bin.push(0x1d);
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::BitAnd => {
                    bin.append(&mut self.compile_expr(*l));
                    bin.push(0x1e);
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Range => {
                    bin.push(0x21);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::PlusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() { 0x2c } else { 0x27 });
                    } else {
                        bin.push(0x27);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::MinusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() { 0x2d } else { 0x28 });
                    } else {
                        bin.push(0x28);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::StarEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() { 0x2e } else { 0x29 });
                    } else {
                        bin.push(0x29);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::DivEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() { 0x2f } else { 0x2a });
                    } else {
                        bin.push(0x2a);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::ModEq => {
                    bin.push(0x2b);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::AndEq => {
                    match *r {
                        Expr::Array(_, _) => bin.push(0x3f),
                        Expr::Identifier(Literal::Number(_)) => bin.push(0x40),
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::OrEq => {
                    match *r {
                        Expr::Array(_, _) => bin.push(0x41),
                        Expr::Identifier(Literal::Number(_)) => bin.push(0x42),
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Assign => match *r {
                    Expr::Identifier(Literal::Number(n)) => {
                        if n.is_float() {
                            bin.push(0x26);
                        } else {
                            bin.push(0x24);
                        }
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    Expr::Array(_, _) => {
                        bin.push(0x24);
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    Expr::UnOp(UnOp::Ampersand, r) => {
                        bin.push(0x25);
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    _ => unreachable!(),
                },
                BinOp::Equal => {
                    bin.push(0xa);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::NotEqual => {
                    bin.push(0xb);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Less => {
                    bin.push(0xc);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Greater => {
                    bin.push(0xd);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::LessEq => {
                    bin.push(0xe);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::GreaterEq => {
                    bin.push(0xf);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::And => {
                    bin.push(0x10);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Or => {
                    bin.push(0x11);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Div | BinOp::Mod => {
                    unreachable!()
                }
            },
            Expr::Array(ident, index) => {
                let ident = match ident {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };

                let index = match *index {
                    Expr::Identifier(Literal::Number(n)) => n.as_u32(),
                    _ => unreachable!(),
                };

                bin.push(get_var(&ident, index));
            }
            Expr::FuncCall(func, args) => {
                let addr = match func {
                    Literal::Identifier(i) => self.get_func(&i).unwrap(),
                    _ => unreachable!(),
                };
                bin.push(addr);
                for arg in args {
                    bin.append(&mut self.compile_expr(arg));
                }
            }
            Expr::Default => bin.push(0x1c),
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
