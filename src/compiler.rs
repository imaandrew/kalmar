use std::collections::HashMap;

use num_enum::TryFromPrimitive;

use crate::lexer::Literal;
use crate::parser::{BinOp, Expr, Stmt, UnOp};

#[derive(TryFromPrimitive)]
#[repr(u8)]
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
    BufRead2, // (F)BufRead2-4 are unused as we manually calculate their values
    BufRead3, // as offsets from BufRead1 in compile_expr
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
    labels: HashMap<String, u32>,
    num_labels: u32,
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
            labels: HashMap::new(),
            num_labels: 0,
        }
    }

    pub fn symbols(&mut self, syms: HashMap<String, u32>) {
        self.syms = syms;
    }

    pub fn compile(&mut self, stmts: &Vec<Stmt>) -> Vec<u32> {
        let mut code = vec![];
        for s in stmts {
            code.append(&mut self.compile_stmt(s));
        }

        code
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Vec<u32> {
        let mut bin = vec![];

        macro_rules! add_op {
            ($op:ident) => {
                bin.push(Op::$op as u32)
            };
        }

        match stmt {
            Stmt::Script(_, s) => {
                bin.append(&mut self.compile_stmt(s));
                add_op!(End);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    bin.append(&mut self.compile_stmt(s));
                }
            }
            Stmt::Return => add_op!(Return),
            Stmt::Label(n) => {
                let lbl = match n {
                    Literal::Identifier(i) => i,
                    _ => panic!(),
                };
                self.labels.insert(lbl.to_string(), self.num_labels);
                add_op!(Label);
                bin.push(self.num_labels);
                self.num_labels += 1;
            }
            Stmt::Goto(n) => {
                let lbl = match n {
                    Literal::Identifier(i) => i,
                    _ => panic!(),
                };
                add_op!(Goto);
                bin.push(*self.labels.get(lbl).unwrap());
            }
            Stmt::Loop(e, s) => {
                add_op!(Loop);
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndLoop)
            }
            Stmt::BreakLoop => add_op!(BreakLoop),
            Stmt::IfElse(i, e) => {
                bin.append(&mut self.compile_stmt(i));
                for expr in e {
                    bin.append(&mut self.compile_stmt(expr));
                }
            }
            Stmt::If(e, s) => {
                match e {
                    Expr::BinOp(_, _, _) => bin.append(&mut self.compile_expr(e)),
                    Expr::UnOp(UnOp::Bang, e) => {
                        let mut e = self.compile_expr(e);
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
                        bin.append(&mut e);
                    }
                    p => panic!("{:?}", p),
                }
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndIf)
            }
            Stmt::Else(Some(i), None) => {
                add_op!(Else);
                bin.append(&mut self.compile_stmt(i))
            }
            Stmt::Else(None, Some(s)) => {
                add_op!(Else);
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndIf)
            }
            Stmt::Switch(e, s) => {
                match e {
                    Expr::Array(_, _) => add_op!(Switch),
                    Expr::Identifier(_) => add_op!(SwitchConst),
                    _ => panic!(),
                }
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndSwitch);
            }
            Stmt::CaseStmt(e, s) => {
                match e {
                    Expr::UnOp(op, e) => {
                        match op {
                            UnOp::Equal => add_op!(CaseEq),
                            UnOp::NotEqual => add_op!(CaseNe),
                            UnOp::Less => add_op!(CaseLt),
                            UnOp::Greater => add_op!(CaseGt),
                            UnOp::LessEq => add_op!(CaseLe),
                            UnOp::GreaterEq => add_op!(CaseGe),
                            UnOp::Ampersand => add_op!(CaseFlag),
                            _ => unreachable!(),
                        }
                        bin.append(&mut self.compile_expr(e));
                    }
                    Expr::BinOp(op, e, b) => {
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            add_op!(CaseEq);
                        } else if *op == BinOp::Range {
                            add_op!(CaseRange);
                        }
                        bin.append(&mut self.compile_expr(e));
                        match op {
                            BinOp::BitOr => add_op!(CaseOrEq),
                            BinOp::BitAnd => add_op!(CaseAndEq),
                            _ => (),
                        }
                        bin.append(&mut self.compile_expr(b));
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            add_op!(EndCaseGroup);
                        }
                    }
                    Expr::Default => add_op!(CaseDefault),
                    Expr::Identifier(_) => {
                        add_op!(CaseEq);
                        bin.append(&mut self.compile_expr(e));
                    }
                    e => panic!("ERROR: {:?}", e),
                }
                bin.append(&mut self.compile_stmt(s));
            }
            Stmt::BreakCase => add_op!(BreakSwitch),
            Stmt::Expr(e) => bin.append(&mut self.compile_expr(e)),
            Stmt::Thread(s) => {
                add_op!(Thread);
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndThread);
            }
            Stmt::ChildThread(s) => {
                add_op!(ChildThread);
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndChildThread);
            }
            Stmt::Jump(i) => {
                add_op!(Jump);
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

    fn compile_expr(&self, expr: &Expr) -> Vec<u32> {
        let mut bin = vec![];

        macro_rules! add_op {
            ($op:ident) => {
                bin.push(Op::$op as u32)
            };
        }

        match expr {
            Expr::Identifier(lit) => match lit {
                Literal::Number(n) => bin.push(n.as_u32()),
                Literal::Identifier(i) => bin.push(*self.syms.get(i).unwrap()),
                _ => todo!(),
            },
            Expr::UnOp(op, expr) => {
                let e = self.compile_expr(expr);
                assert_eq!(e.len(), 1);
                match op {
                    UnOp::Minus => bin.push(*e.first().unwrap() as i32 as u32),
                    _ => todo!(),
                }
            }
            Expr::BinOp(op, l, r) => match op {
                BinOp::BitAnd => {
                    add_op!(IfFlag);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Range => {
                    add_op!(CaseRange);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::PlusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = **r {
                        bin.push(if n.is_float() { Op::AddF } else { Op::Add } as u32);
                    } else {
                        add_op!(Add);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::MinusEq => {
                    if let Expr::Identifier(Literal::Number(n)) = **r {
                        bin.push(if n.is_float() { Op::SubF } else { Op::Sub } as u32);
                    } else {
                        add_op!(Sub);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::StarEq => {
                    if let Expr::Identifier(Literal::Number(n)) = **r {
                        bin.push(if n.is_float() { Op::MulF } else { Op::Mul } as u32);
                    } else {
                        add_op!(Mul);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::DivEq => {
                    if let Expr::Identifier(Literal::Number(n)) = **r {
                        bin.push(if n.is_float() { Op::DivF } else { Op::Div } as u32);
                    } else {
                        add_op!(Div);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::ModEq => {
                    add_op!(Mod);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::AndEq => {
                    match **r {
                        Expr::Array(_, _) => add_op!(BitwiseAnd),
                        Expr::Identifier(Literal::Number(_)) => {
                            add_op!(BitwiseAndConst)
                        }
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::OrEq => {
                    match **r {
                        Expr::Array(_, _) => add_op!(BitwiseOr),
                        Expr::Identifier(Literal::Number(_)) => add_op!(BitwiseOrConst),
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Assign => match r.as_ref() {
                    Expr::Identifier(Literal::Number(n)) => {
                        if n.is_float() {
                            add_op!(SetF);
                        } else {
                            add_op!(Set);
                        }
                        bin.append(&mut self.compile_expr(l));
                        bin.append(&mut self.compile_expr(r));
                    }
                    Expr::Identifier(Literal::Identifier(i)) => {
                        match i.as_str() {
                            "buffer" => add_op!(BufPeek),
                            "fbuffer" => add_op!(FBufPeek),
                            _ => panic!(),
                        }
                        bin.append(&mut self.compile_expr(l));
                    }
                    Expr::Array(_, _) => {
                        add_op!(Set);
                        bin.append(&mut self.compile_expr(l));
                        bin.append(&mut self.compile_expr(r));
                    }
                    Expr::UnOp(UnOp::Ampersand, r) => {
                        add_op!(SetConst);
                        bin.append(&mut self.compile_expr(l));
                        bin.append(&mut self.compile_expr(r));
                    }
                    Expr::FuncCall(Literal::Identifier(s), a) => {
                        bin.push(self.get_func(s, true).unwrap());
                        for arg in a {
                            bin.append(&mut self.compile_expr(arg));
                        }
                    }
                    e => panic!("{:?}", e),
                },
                BinOp::Equal => {
                    add_op!(IfEq);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::NotEqual => {
                    add_op!(IfNe);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Less => {
                    add_op!(IfLt);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Greater => {
                    add_op!(IfGt);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::LessEq => {
                    add_op!(IfLe);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::GreaterEq => {
                    add_op!(IfGe);
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Plus
                | BinOp::Minus
                | BinOp::Star
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BitOr => {
                    unreachable!()
                }
                BinOp::Arrow => {
                    let vars = self.compile_expr(l);
                    let mut num_vars = vars.len();
                    let mut vars = vars.iter();
                    for i in (1..5usize).rev() {
                        if num_vars == 0 {
                            break;
                        };
                        let op = match r.as_ref() {
                            Expr::Identifier(Literal::Identifier(s)) if s == "buffer" => {
                                Op::BufRead1 as usize
                            }
                            Expr::Identifier(Literal::Identifier(s)) if s == "fbuffer" => {
                                Op::FBufRead1 as usize
                            }
                            _ => panic!(),
                        } - 1
                            + i;
                        let num_times = num_vars / i;
                        num_vars %= i;
                        bin.push(op as u32);
                        for _ in 0..num_times {
                            bin.push(*vars.next().unwrap());
                        }
                    }
                }
                BinOp::Comma => {
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
            },
            Expr::Array(ident, index) => {
                let ident = match ident {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };

                let index = match **index {
                    Expr::Identifier(Literal::Number(n)) => n.as_u32(),
                    _ => unreachable!(),
                };

                bin.push(get_var(ident, index));
            }
            Expr::FuncCall(func, args) => {
                add_op!(Call);
                let addr = match func {
                    Literal::Identifier(i) => self.get_func(i, false).unwrap(),
                    _ => unreachable!(),
                };
                bin.push(addr);
                bin.push(args.len().try_into().unwrap());
                for arg in args {
                    bin.append(&mut self.compile_expr(arg));
                }
            }
            Expr::ArrayAssign(ident, expr) => {
                let ident = match ident {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };

                match ident.as_str() {
                    "buffer" => add_op!(UseBuf),
                    "fbuffer" => add_op!(UseFBuf),
                    "array" => add_op!(UseArray),
                    "flag_array" => add_op!(UseFlags),
                    _ => panic!(),
                }
                bin.append(&mut self.compile_expr(expr));
            }
            Expr::Default => add_op!(CaseDefault),
        }

        bin
    }

    fn get_func(&self, func: &str, returns_val: bool) -> Option<u32> {
        match func {
            "wait" => Some(Op::WaitFrames as u32),
            "wait_sec" => Some(Op::WaitSecs as u32),
            "alloc" => Some(Op::MallocArray as u32),
            "exec" => {
                if returns_val {
                    Some(Op::ExecGetTid as u32)
                } else {
                    Some(Op::Exec as u32)
                }
            }
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
