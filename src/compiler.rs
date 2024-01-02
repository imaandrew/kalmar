use std::borrow::Borrow;
use std::collections::HashMap;

use num_enum::TryFromPrimitive;

use crate::lexer::Literal;
use crate::parser::{ASTNode, BinOp, Expr, Stmt, UnOp};

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

impl Op {
    fn get_arg_count(&self) -> u32 {
        match self {
            Self::End
            | Self::Return
            | Self::EndLoop
            | Self::BreakLoop
            | Self::EndIf
            | Self::Else
            | Self::Unbind
            | Self::EndCaseGroup
            | Self::BreakSwitch
            | Self::EndThread
            | Self::EndChildThread
            | Self::Thread
            | Self::ChildThread
            | Self::EndSwitch => 0,
            Self::Label
            | Self::Goto
            | Self::Loop
            | Self::Switch
            | Self::SwitchConst
            | Self::CaseEq
            | Self::CaseNe
            | Self::CaseLt
            | Self::CaseGt
            | Self::CaseLe
            | Self::CaseGe
            | Self::CaseFlag
            | Self::CaseOrEq
            | Self::CaseAndEq
            | Self::UseBuf
            | Self::UseFBuf
            | Self::UseArray
            | Self::UseFlags
            | Self::CaseDefault
            | Self::WaitFrames
            | Self::WaitSecs
            | Self::Exec
            | Self::ExecWait
            | Self::KillThread
            | Self::SetPriority
            | Self::SetTimescale
            | Self::SetGroup
            | Self::SuspendGroup
            | Self::ResumeGroup
            | Self::SuspendOthers
            | Self::ResumeOthers
            | Self::SuspendThread
            | Self::ResumeThread
            | Self::BufPeek
            | Self::FBufPeek
            | Self::BufRead1
            | Self::FBufRead1
            | Self::Jump => 1,
            Self::IfEq
            | Self::IfNe
            | Self::IfLt
            | Self::IfGt
            | Self::IfLe
            | Self::IfGe
            | Self::IfFlag
            | Self::IfNotFlag
            | Self::CaseRange
            | Self::MallocArray
            | Self::ExecGetTid
            | Self::IsThreadRunning
            | Self::Set
            | Self::SetF
            | Self::SetConst
            | Self::Add
            | Self::AddF
            | Self::Sub
            | Self::SubF
            | Self::Mul
            | Self::MulF
            | Self::Div
            | Self::DivF
            | Self::Mod
            | Self::BufRead2
            | Self::FBufRead2
            | Self::BitwiseAnd
            | Self::BitwiseAndConst
            | Self::BitwiseOr
            | Self::BitwiseOrConst => 2,
            Self::BufRead3 | Self::FBufRead3 => 3,
            Self::BufRead4 | Self::FBufRead4 => 4,
            Self::BindTrigger => 5,
            Self::BindPadlock => 6,
            Self::InternalFetch
            | Self::DebugLog
            | Self::DebugPrintVar
            | Self::Op92
            | Self::Op93
            | Self::Op94
            | Self::Call => unreachable!(),
        }
    }
}

pub struct Compiler<'a> {
    syms: HashMap<&'a str, u32>,
    labels: HashMap<String, u32>,
    num_labels: u32,
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Compiler {
            syms: HashMap::new(),
            labels: HashMap::new(),
            num_labels: 0,
        }
    }

    pub fn add_syms(&mut self, syms: Vec<(&'a str, u32)>) {
        self.syms.extend(syms)
    }

    pub fn compile(&mut self, stmts: &Vec<ASTNode>) -> Vec<u32> {
        let mut code = vec![];
        for s in stmts {
            code.append(&mut self.compile_stmt(s));
        }

        code
    }

    fn compile_stmt(&mut self, stmt: &ASTNode) -> Vec<u32> {
        let mut bin = vec![];

        macro_rules! add_op {
            ($op:ident) => {{
                bin.push(Op::$op as u32);
                bin.push(Op::$op.get_arg_count());
            }};
        }

        match stmt.get_stmt() {
            Stmt::Script(_, s) => {
                bin.append(&mut self.compile_stmt(s));
                add_op!(End);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    bin.append(&mut self.compile_stmt(s));
                }
            }
            Stmt::Return => {
                add_op!(Return);
            }
            Stmt::Label(n) => {
                let lbl = match n.as_ref() {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };
                self.labels.insert(lbl.to_string(), self.num_labels);
                add_op!(Label);
                bin.push(self.num_labels);
                self.num_labels += 1;
            }
            Stmt::Goto(n) => {
                let lbl = match n.as_ref() {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };
                add_op!(Goto);
                bin.push(*self.labels.get(lbl).unwrap());
            }
            Stmt::Loop(e, s) => {
                add_op!(Loop);
                if let Some(e) = e {
                    bin.append(&mut self.compile_expr(e));
                } else {
                    bin.push(0);
                }
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndLoop);
            }
            Stmt::BreakLoop => {
                add_op!(BreakLoop);
            }
            Stmt::IfElse(i, e) => {
                bin.append(&mut self.compile_stmt(i));
                for expr in e {
                    bin.append(&mut self.compile_stmt(expr));
                }
                add_op!(EndIf);
            }
            Stmt::If(e, s) => {
                match e.get_expr() {
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
                        bin.push(2);
                    }
                    p => panic!("{:?}", p),
                }
                bin.append(&mut self.compile_stmt(s));
            }
            Stmt::Else(Some(i), None) => {
                add_op!(Else);
                bin.append(&mut self.compile_stmt(i))
            }
            Stmt::Else(None, Some(s)) => {
                add_op!(Else);
                bin.append(&mut self.compile_stmt(s));
            }
            Stmt::Switch(e, s) => {
                match e.get_expr() {
                    Expr::Array(_, _) => add_op!(Switch),
                    Expr::Identifier(_) => add_op!(SwitchConst),
                    _ => panic!(),
                }
                bin.append(&mut self.compile_expr(e));
                bin.append(&mut self.compile_stmt(s));
                add_op!(EndSwitch);
            }
            Stmt::Case(e, s) => {
                match e.get_expr() {
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
                    Expr::Default => {
                        add_op!(CaseDefault);
                    }
                    Expr::Identifier(_) => {
                        add_op!(CaseEq);
                        bin.append(&mut self.compile_expr(e));
                    }
                    e => panic!("ERROR: {:?}", e),
                }
                bin.append(&mut self.compile_stmt(s));
            }
            Stmt::BreakCase => {
                add_op!(BreakSwitch);
            }
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
                bin.push(match i.borrow() {
                    Literal::Number(n) => n.as_u32(),
                    _ => todo!(),
                });
            }
            Stmt::Empty => (),
            Stmt::Else(_, _) => unreachable!(),
        }

        bin
    }

    fn compile_expr(&self, expr: &ASTNode) -> Vec<u32> {
        let mut bin = vec![];

        macro_rules! add_op {
            ($op:ident) => {{
                bin.push(Op::$op as u32);
                bin.push(Op::$op.get_arg_count());
            }};
        }

        match expr.get_expr() {
            Expr::Identifier(lit) => match lit.borrow() {
                Literal::Number(n) => bin.push(n.as_u32()),
                Literal::Identifier(i) => bin.push(*self.syms.get(i.as_str()).unwrap()),
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
                    if let Some(Literal::Number(n)) = r.get_expr().get_literal() {
                        if n.is_float() {
                            add_op!(AddF)
                        } else {
                            add_op!(Add)
                        }
                    } else {
                        add_op!(Add);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::MinusEq => {
                    if let Some(Literal::Number(n)) = r.get_expr().get_literal() {
                        if n.is_float() {
                            add_op!(SubF)
                        } else {
                            add_op!(Sub)
                        }
                    } else {
                        add_op!(Sub);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::StarEq => {
                    if let Some(Literal::Number(n)) = r.get_expr().get_literal() {
                        if n.is_float() {
                            add_op!(MulF)
                        } else {
                            add_op!(Mul)
                        }
                    } else {
                        add_op!(Mul);
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::DivEq => {
                    if let Some(Literal::Number(n)) = r.get_expr().get_literal() {
                        if n.is_float() {
                            add_op!(DivF)
                        } else {
                            add_op!(Div)
                        }
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
                    match r.get_expr() {
                        Expr::Array(_, _) => add_op!(BitwiseAnd),
                        Expr::Identifier(l) if matches!(l.borrow(), Literal::Number(_)) => {
                            add_op!(BitwiseAndConst)
                        }
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::OrEq => {
                    match r.get_expr() {
                        Expr::Array(_, _) => add_op!(BitwiseOr),
                        Expr::Identifier(l) if matches!(l.borrow(), Literal::Number(_)) => {
                            add_op!(BitwiseOrConst)
                        }
                        _ => unreachable!(),
                    }
                    bin.append(&mut self.compile_expr(l));
                    bin.append(&mut self.compile_expr(r));
                }
                BinOp::Assign => match r.get_expr() {
                    Expr::Identifier(lit) if matches!(lit.borrow(), Literal::Number(_)) => {
                        if let Literal::Number(n) = lit.borrow() {
                            if n.is_float() {
                                add_op!(SetF);
                            } else {
                                add_op!(Set);
                            }
                            bin.append(&mut self.compile_expr(l));
                            bin.append(&mut self.compile_expr(r));
                        }
                    }
                    Expr::Identifier(i) if matches!(i.borrow(), Literal::Identifier(_)) => {
                        if let Literal::Identifier(i) = i.borrow() {
                            match i.as_str() {
                                "buffer" => add_op!(BufPeek),
                                "fbuffer" => add_op!(FBufPeek),
                                _ => panic!(),
                            }
                            bin.append(&mut self.compile_expr(l));
                        }
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
                    Expr::FuncCall(i, a) if matches!(i.borrow(), Literal::Identifier(_)) => {
                        if let Literal::Identifier(s) = i.borrow() {
                            let f = self.get_func(s, true).unwrap();
                            bin.push(f.0);
                            bin.push(f.1 as u32);
                            for arg in a {
                                bin.append(&mut self.compile_expr(arg));
                            }
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
                        let op = match r.get_expr().get_literal() {
                            Some(Literal::Identifier(s)) if s == "buffer" => Op::BufRead1 as usize,
                            Some(Literal::Identifier(s)) if s == "fbuffer" => {
                                Op::FBufRead1 as usize
                            }
                            _ => panic!(),
                        } - 1
                            + i;
                        let num_times = num_vars / i;
                        num_vars %= i;
                        bin.push(op as u32);
                        bin.push(num_times as u32);
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
                let ident = match ident.borrow() {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };

                let index = match index.get_expr().get_literal() {
                    Some(Literal::Number(n)) => n.as_u32(),
                    _ => unreachable!(),
                };

                bin.push(get_var(ident, index));
            }
            Expr::FuncCall(func, args) => {
                let addr = match func.borrow() {
                    Literal::Identifier(i) => self
                        .get_func(i, false)
                        .unwrap_or_else(|| panic!("Missing function: {}", i)),
                    _ => unreachable!(),
                };
                if addr.1 == -1 {
                    bin.push(Op::Call as u32);
                    bin.push(args.len() as u32 + 1);
                    bin.push(addr.0);
                    for arg in args {
                        bin.append(&mut self.compile_expr(arg));
                    }
                } else {
                    bin.push(addr.0);
                    bin.push(addr.1 as u32);
                    for arg in args {
                        bin.append(&mut self.compile_expr(arg));
                    }
                }
            }
            Expr::ArrayAssign(ident, expr) => {
                let ident = match ident.borrow() {
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
            Expr::Default => {
                add_op!(CaseDefault);
            }
        }

        bin
    }

    fn get_func(&self, func: &str, returns_val: bool) -> Option<(u32, i32)> {
        let op = match func {
            "wait" => Some(Op::WaitFrames),
            "wait_sec" => Some(Op::WaitSecs),
            "alloc" => Some(Op::MallocArray),
            "exec" => {
                if returns_val {
                    Some(Op::ExecGetTid)
                } else {
                    Some(Op::Exec)
                }
            }
            "exec_wait" => Some(Op::ExecWait),
            "bind" => Some(Op::BindTrigger),
            "unbind" => Some(Op::Unbind),
            "kill" => Some(Op::KillThread),
            "set_priority" => Some(Op::SetPriority),
            "set_timescale" => Some(Op::SetTimescale),
            "set_group" => Some(Op::SetGroup),
            "bind_lock" => Some(Op::BindPadlock),
            "suspend_all" => Some(Op::SuspendGroup),
            "resume_all" => Some(Op::ResumeGroup),
            "suspend_others" => Some(Op::SuspendOthers),
            "resume_others" => Some(Op::ResumeOthers),
            "suspend" => Some(Op::SuspendThread),
            "resume" => Some(Op::ResumeThread),
            "does_exist" => Some(Op::IsThreadRunning),
            _ => None,
        };

        match op {
            Some(op) => {
                let a = op.get_arg_count();
                Some((op as u32, a as i32))
            }
            None => self.syms.get(func).copied().map(|x| (x, -1)),
        }
    }
}

fn get_var(ident: &str, index: u32) -> u32 {
    let index = index as i32;
    (match ident {
        "var" => index - 30000000,
        "MapVar" => index - 50000000,
        "flag" => index - 70000000,
        "MapFlag" => index - 90000000,
        "AreaFlag" => index - 110000000,
        "GameFlag" => index - 130000000,
        "AreaByte" => index - 150000000,
        "GameByte" => index - 170000000,
        "array" => index - 190000000,
        "flag_array" => index - 210000000,
        _ => panic!(),
    }) as u32
}
