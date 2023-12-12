//use num_enum::TryFromPrimitive;
use std::collections::HashMap;

use crate::lexer::Literal;
use crate::parser::{BinOp, Expr, Stmt, UnOp};

//#[derive(TryFromPrimitive)]
//#[repr(u8)]
enum Op {
    InternalFetch,
    End,
    Return,
    Label,
    Goto,
    Loop,
    EndLoop,
    BreakLoop,
    WaitFrames,
    WaitSecs,
    IfEq,
    IfNe,
    IfLt,
    IfGt,
    IfLe,
    IfGe,
    IfFlag,
    IfNotFlag,
    Else,
    EndIf,
    Switch,
    SwitchConst,
    CaseEq,
    CaseNe,
    CaseLt,
    CaseGt,
    CaseLe,
    CaseGe,
    CaseDefault,
    CaseOrEq,
    CaseAndEq,
    CaseFlag,
    EndCaseGroup,
    CaseRange,
    BreakSwitch,
    EndSwitch,
    Set,
    SetConst,
    SetF,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    AddF,
    SubF,
    MulF,
    DivF,
    UseBuf,
    BufRead1,
    BufRead2,
    BufRead3,
    BufRead4,
    BufPeek,
    UseFBuf,
    FBufRead1,
    FBufRead2,
    FBufRead3,
    FBufRead4,
    FBufPeek,
    UseArray,
    UseFlags,
    MallocArray,
    BitwiseAnd,
    BitwiseAndConst,
    BitwiseOr,
    BitwiseOrConst,
    Call,
    Exec,
    ExecGetTid,
    ExecWait,
    BindTrigger,
    Unbind,
    KillThread,
    Jump,
    SetPriority,
    SetTimescale,
    SetGroup,
    BindPadlock,
    SuspendGroup,
    ResumeGroup,
    SuspendOthers,
    ResumeOthers,
    SuspendThread,
    ResumeThread,
    IsThreadRunning,
    Thread,
    EndThread,
    ChildThread,
    EndChildThread,
    DebugLog,
    DebugPrintVar,
    Op92,
    Op93,
    Op94,
}

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
                bin.push(Op::End as u32);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    bin.append(&mut self.compile_stmt(s));
                }
            }
            Stmt::Return => bin.push(Op::Return as u32),
            Stmt::Label(n) => {
                bin.push(Op::Label as u32);
                bin.push(n.as_u32());
            }
            Stmt::Goto(n) => {
                bin.push(Op::Goto as u32);
                bin.push(n.as_u32());
            }
            Stmt::Loop(e, s) => {
                bin.push(Op::Loop as u32);
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndLoop as u32)
            }
            Stmt::BreakLoop => bin.push(Op::BreakLoop as u32),
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
                        /*
                        e[0] = match Op::try_from(e[0] as u8).unwrap() {
                            Op::IfEq => Op::IfNe,
                            Op::IfNe => Op::IfEq,
                            Op::IfLt => Op::IfGe,
                            Op::IfGt => Op::IfLe,
                            Op::IfLe => Op::IfGt,
                            Op::IfGe => Op::IfLt,
                            Op::IfFlag => Op::IfNotFlag,
                            Op::IfNotFlag => Op::IfFlag,
                            _ => unreachable!(),
                        } as u32;
                        */
                        e[0] = match e[0] {
                            0xa => 0xb,
                            0xb => 0xa,
                            0xc => 0xf,
                            0xd => 0xe,
                            0xe => 0xd,
                            0xf => 0xc,
                            0x10 => 0x11,
                            0x11 => 0x10,
                            e => panic!("{:?}", e),
                        };
                        bin.append(&mut e);
                    }
                    p => panic!("{:?}", p),
                }
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndIf as u32)
            }
            Stmt::Else(Some(i), None) => {
                bin.push(Op::Else as u32);
                bin.append(&mut self.compile_stmt(*i))
            }
            Stmt::Else(None, Some(s)) => {
                bin.push(Op::Else as u32);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndIf as u32)
            }
            Stmt::Switch(e, s) => {
                match e {
                    Expr::Array(_, _) => bin.push(Op::Switch as u32),
                    Expr::Identifier(_) => bin.push(Op::SwitchConst as u32),
                    _ => panic!(),
                }
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndSwitch as u32);
            }
            Stmt::CaseStmt(e, s) => {
                match e {
                    Expr::UnOp(op, e) => {
                        match op {
                            UnOp::Equal => bin.push(Op::CaseEq as u32),
                            UnOp::NotEqual => bin.push(Op::CaseNe as u32),
                            UnOp::Less => bin.push(Op::CaseLt as u32),
                            UnOp::Greater => bin.push(Op::CaseGt as u32),
                            UnOp::LessEq => bin.push(Op::CaseLe as u32),
                            UnOp::GreaterEq => bin.push(Op::CaseGe as u32),
                            UnOp::Ampersand => bin.push(Op::CaseFlag as u32),
                            _ => unreachable!(),
                        }
                        bin.append(&mut self.compile_expr(*e));
                    }
                    Expr::BinOp(op, e, b) => {
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            bin.push(Op::CaseEq as u32);
                        } else if op == BinOp::Range {
                            bin.push(Op::CaseRange as u32);
                        }
                        bin.append(&mut self.compile_expr(*e));
                        match op {
                            BinOp::BitOr => bin.push(Op::CaseOrEq as u32),
                            BinOp::BitAnd => bin.push(Op::CaseAndEq as u32),
                            _ => (),
                        }
                        bin.append(&mut self.compile_expr(*b));
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            bin.push(Op::EndCaseGroup as u32);
                        }
                    }
                    Expr::Default => bin.push(Op::CaseDefault as u32),
                    e => panic!("ERROR: {:?}", e),
                }
                bin.append(&mut self.compile_stmt(*s));
            }
            Stmt::BreakCase => bin.push(Op::BreakSwitch as u32),
            Stmt::Expr(e) => bin.append(&mut self.compile_expr(e)),
            Stmt::Thread(s) => {
                bin.push(Op::Thread as u32);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndThread as u32);
            }
            Stmt::ChildThread(s) => {
                bin.push(Op::ChildThread as u32);
                bin.append(&mut self.compile_stmt(*s));
                bin.push(Op::EndChildThread as u32);
            }
            Stmt::Jump(i) => {
                bin.push(Op::Jump as u32);
                bin.push(match i {
                    Literal::Number(n) => n.as_u32(),
                    _ => todo!(),
                });
            }
            Stmt::Empty => (),
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
                    UnOp::Minus => bin.push(*e.first().unwrap() as i32 as u32),
                    _ => todo!(),
                }
            }
            Expr::BinOp(op, l, r) => match op {
                BinOp::BitAnd => {
                    bin.append(&mut self.compile_expr(*l));
                    bin.push(Op::IfFlag as u32);
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Range => {
                    bin.push(Op::CaseRange as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::PlusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() {
                            Op::AddF as u32
                        } else {
                            Op::Add as u32
                        });
                    } else {
                        bin.push(Op::Add as u32);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::MinusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() {
                            Op::SubF as u32
                        } else {
                            Op::Sub as u32
                        });
                    } else {
                        bin.push(Op::Sub as u32);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::StarEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() {
                            Op::MulF as u32
                        } else {
                            Op::Mul as u32
                        });
                    } else {
                        bin.push(Op::Mul as u32);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::DivEq => {
                    if let Expr::Identifier(Literal::Number(n)) = *r {
                        bin.push(if n.is_float() {
                            Op::DivF as u32
                        } else {
                            Op::Div as u32
                        });
                    } else {
                        bin.push(Op::Div as u32);
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::ModEq => {
                    bin.push(Op::Mod as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::AndEq => {
                    match *r {
                        Expr::Array(_, _) => bin.push(Op::BitwiseAnd as u32),
                        Expr::Identifier(Literal::Number(_)) => {
                            bin.push(Op::BitwiseAndConst as u32)
                        }
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::OrEq => {
                    match *r {
                        Expr::Array(_, _) => bin.push(Op::BitwiseOr as u32),
                        Expr::Identifier(Literal::Number(_)) => bin.push(Op::BitwiseOrConst as u32),
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Assign => match *r {
                    Expr::Identifier(Literal::Number(n)) => {
                        if n.is_float() {
                            bin.push(Op::SetF as u32);
                        } else {
                            bin.push(Op::Set as u32);
                        }
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    Expr::Array(_, _) => {
                        bin.push(Op::Set as u32);
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    Expr::UnOp(UnOp::Ampersand, r) => {
                        bin.push(Op::SetConst as u32);
                        bin.append(&mut self.compile_expr(*l));
                        bin.append(&mut self.compile_expr(*r));
                    }
                    _ => unreachable!(),
                },
                BinOp::Equal => {
                    bin.push(Op::IfEq as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::NotEqual => {
                    bin.push(Op::IfNe as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Less => {
                    bin.push(Op::IfLt as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Greater => {
                    bin.push(Op::IfGt as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::LessEq => {
                    bin.push(Op::IfLe as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::GreaterEq => {
                    bin.push(Op::IfGe as u32);
                    bin.append(&mut self.compile_expr(*l));
                    bin.append(&mut self.compile_expr(*r));
                }
                BinOp::Plus
                | BinOp::Minus
                | BinOp::Star
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BitOr => {
                    unreachable!()
                }
                _ => todo!(),
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
                bin.push(Op::Call as u32);
                let addr = match func {
                    Literal::Identifier(i) => self.get_func(&i).unwrap(),
                    _ => unreachable!(),
                };
                bin.push(addr);
                bin.push(args.len().try_into().unwrap());
                for arg in args {
                    bin.append(&mut self.compile_expr(arg));
                }
            }
            Expr::Default => bin.push(Op::CaseDefault as u32),
        }

        bin
    }

    fn get_func(&self, func: &str) -> Option<u32> {
        match func {
            "wait" => Some(Op::WaitFrames as u32),
            "wait_sec" => Some(Op::WaitSecs as u32),
            "alloc" => Some(Op::MallocArray as u32),
            "exec" => Some(Op::Exec as u32),
            "exec_wait" => Some(Op::ExecWait as u32),
            "bind" => Some(Op::BindTrigger as u32),
            "unbind" => Some(Op::Unbind as u32),
            "kill" => Some(Op::KillThread as u32),
            "set_priority" => Some(Op::SetPriority as u32),
            "set_timescale" => Some(Op::SetTimescale as u32),
            "set_group" => Some(Op::SetGroup as u32),
            "bind_lock" => Some(Op::BindPadlock as u32),
            "suspend_all" => Some(Op::SuspendGroup as u32),
            "resume_all" => Some(Op::ResumeGroup as u32),
            "suspend_others" => Some(Op::SuspendOthers as u32),
            "resume_others" => Some(Op::ResumeOthers as u32),
            "suspend" => Some(Op::SuspendThread as u32),
            "resume" => Some(Op::ResumeThread as u32),
            "does_exist" => Some(Op::IsThreadRunning as u32),
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
