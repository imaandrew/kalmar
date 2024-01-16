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
    base_addr: u32,
    unresolved_syms: Vec<(&'a str, u32)>,
    code: Vec<u32>,
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new(0)
    }
}

impl<'a> Compiler<'a> {
    pub fn new(base_addr: u32) -> Self {
        Compiler {
            syms: HashMap::new(),
            labels: HashMap::new(),
            num_labels: 0,
            base_addr,
            unresolved_syms: vec![],
            code: vec![],
        }
    }

    pub fn add_syms(&mut self, syms: Vec<(&'a str, u32)>) {
        self.syms.extend(syms)
    }

    pub fn compile(&mut self, stmts: &'a Vec<ASTNode>) -> Vec<u32> {
        for s in stmts {
            self.compile_stmt(s);
        }

        for (s, a) in &self.unresolved_syms {
            let s = self.syms.get(s).unwrap();
            self.code[*a as usize] = *s;
        }

        std::mem::take(&mut self.code)
    }

    fn compile_stmt(&mut self, stmt: &'a ASTNode) {
        macro_rules! add_op {
            ($op:ident) => {{
                self.code.push(Op::$op as u32);
                self.code.push(Op::$op.get_arg_count());
            }};
        }

        match stmt.get_stmt() {
            Stmt::Script(l, s) => {
                let l = match l.as_ref() {
                    Literal::Identifier(i) => i,
                    _ => panic!(),
                };
                self.syms
                    .insert(l, 4 * self.code.len() as u32 + self.base_addr);
                self.compile_stmt(s);
                add_op!(End);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.compile_stmt(s);
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
                self.code.push(self.num_labels);
                self.num_labels += 1;
            }
            Stmt::Goto(n) => {
                let lbl = match n.as_ref() {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };
                add_op!(Goto);
                self.code.push(*self.labels.get(lbl).unwrap());
            }
            Stmt::Loop(e, s) => {
                add_op!(Loop);
                if let Some(e) = e {
                    self.compile_expr(e);
                } else {
                    self.code.push(0);
                }
                self.compile_stmt(s);
                add_op!(EndLoop);
            }
            Stmt::BreakLoop => {
                add_op!(BreakLoop);
            }
            Stmt::IfElse(i, e) => {
                self.compile_stmt(i);
                for expr in e {
                    self.compile_stmt(expr);
                }
                add_op!(EndIf);
            }
            Stmt::If(e, s) => {
                match e.get_expr() {
                    Expr::BinOp(_, _, _) => self.compile_expr(e),
                    Expr::UnOp(UnOp::Bang, e) => {
                        let start = self.code.len();
                        self.compile_expr(e);
                        self.code[start] = match Op::try_from(self.code[start] as u8).unwrap() {
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
                        self.code.push(2);
                    }
                    p => panic!("{:?}", p),
                }
                self.compile_stmt(s);
            }
            Stmt::Else(Some(i), None) => {
                add_op!(Else);
                self.compile_stmt(i)
            }
            Stmt::Else(None, Some(s)) => {
                add_op!(Else);
                self.compile_stmt(s);
            }
            Stmt::Switch(e, s) => {
                match e.get_expr() {
                    Expr::Array(_, _) => add_op!(Switch),
                    Expr::Identifier(_) => add_op!(SwitchConst),
                    _ => panic!(),
                }
                self.compile_expr(e);
                self.compile_stmt(s);
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
                        self.compile_expr(e);
                    }
                    Expr::BinOp(op, e, b) => {
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            add_op!(CaseEq);
                        } else if *op == BinOp::Range {
                            add_op!(CaseRange);
                        }
                        self.compile_expr(e);
                        match op {
                            BinOp::BitOr => add_op!(CaseOrEq),
                            BinOp::BitAnd => add_op!(CaseAndEq),
                            _ => (),
                        }
                        self.compile_expr(b);
                        if matches!(op, BinOp::BitOr | BinOp::BitAnd) {
                            add_op!(EndCaseGroup);
                        }
                    }
                    Expr::Default => {
                        add_op!(CaseDefault);
                    }
                    Expr::Identifier(_) => {
                        add_op!(CaseEq);
                        self.compile_expr(e);
                    }
                    e => panic!("ERROR: {:?}", e),
                }
                self.compile_stmt(s);
            }
            Stmt::BreakCase => {
                add_op!(BreakSwitch);
            }
            Stmt::Expr(e) => self.compile_expr(e),
            Stmt::Thread(s) => {
                add_op!(Thread);
                self.compile_stmt(s);
                add_op!(EndThread);
            }
            Stmt::ChildThread(s) => {
                add_op!(ChildThread);
                self.compile_stmt(s);
                add_op!(EndChildThread);
            }
            Stmt::Jump(i) => {
                add_op!(Jump);
                self.code.push(match i.borrow() {
                    Literal::Number(n) => n.as_u32(),
                    _ => todo!(),
                });
            }
            Stmt::Empty => (),
            Stmt::Else(_, _) => unreachable!(),
        }
    }

    fn compile_expr(&mut self, expr: &'a ASTNode) {
        macro_rules! add_op {
            ($op:ident) => {{
                self.code.push(Op::$op as u32);
                self.code.push(Op::$op.get_arg_count());
            }};
        }

        match expr.get_expr() {
            Expr::Identifier(lit) => match lit.borrow() {
                Literal::Number(n) => self.code.push(n.as_u32()),
                Literal::Identifier(i) => {
                    self.code
                        .push(*self.syms.get(i.as_str()).unwrap_or_else(|| {
                            self.unresolved_syms.push((i, self.code.len() as u32));
                            &0
                        }))
                }
                Literal::Boolean(b) => self.code.push(if *b { 1 } else { 0 }),
            },
            Expr::UnOp(op, expr) => {
                /*
                let e = self.compile_expr(expr);
                assert_eq!(e.len(), 1);
                match op {
                    UnOp::Minus => self.code.push(*e.first().unwrap() as i32 as u32),
                    _ => todo!(),
                }
                */ // TODO: what is this doing
            }
            Expr::BinOp(op, l, r) => match op {
                BinOp::BitAnd => {
                    add_op!(IfFlag);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::Range => {
                    add_op!(CaseRange);
                    self.compile_expr(l);
                    self.compile_expr(r);
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
                    self.compile_expr(l);
                    self.compile_expr(r);
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
                    self.compile_expr(l);
                    self.compile_expr(r);
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
                    self.compile_expr(l);
                    self.compile_expr(r);
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
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::ModEq => {
                    add_op!(Mod);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::AndEq => {
                    match r.get_expr() {
                        Expr::Array(_, _) => add_op!(BitwiseAnd),
                        Expr::Identifier(l) if matches!(l.borrow(), Literal::Number(_)) => {
                            add_op!(BitwiseAndConst)
                        }
                        _ => unreachable!(),
                    }
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::OrEq => {
                    match r.get_expr() {
                        Expr::Array(_, _) => add_op!(BitwiseOr),
                        Expr::Identifier(l) if matches!(l.borrow(), Literal::Number(_)) => {
                            add_op!(BitwiseOrConst)
                        }
                        _ => unreachable!(),
                    }
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::Assign => match r.get_expr() {
                    Expr::Identifier(lit) if matches!(lit.borrow(), Literal::Number(_)) => {
                        if let Literal::Number(n) = lit.borrow() {
                            if n.is_float() {
                                add_op!(SetF);
                            } else {
                                add_op!(Set);
                            }
                            self.compile_expr(l);
                            self.compile_expr(r);
                        }
                    }
                    Expr::Identifier(i) if matches!(i.borrow(), Literal::Identifier(_)) => {
                        if let Literal::Identifier(i) = i.borrow() {
                            match i.as_str() {
                                "Buffer" => add_op!(BufPeek),
                                "FBuffer" => add_op!(FBufPeek),
                                _ => panic!(),
                            }
                            self.compile_expr(l);
                        }
                    }
                    Expr::Array(_, _) => {
                        add_op!(Set);
                        self.compile_expr(l);
                        self.compile_expr(r);
                    }
                    Expr::UnOp(UnOp::Ampersand, r) => {
                        add_op!(SetConst);
                        self.compile_expr(l);
                        self.compile_expr(r);
                    }
                    Expr::FuncCall(i, a) if matches!(i.borrow(), Literal::Identifier(_)) => {
                        if let Literal::Identifier(s) = i.borrow() {
                            let f = self.get_func(s, true).unwrap();
                            self.code.push(f.0);
                            self.code.push(f.1 as u32);
                            for arg in a {
                                self.compile_expr(arg);
                            }
                            self.compile_expr(l);
                        }
                    }
                    e => panic!("{:?}", e),
                },
                BinOp::Equal => {
                    add_op!(IfEq);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::NotEqual => {
                    add_op!(IfNe);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::Less => {
                    add_op!(IfLt);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::Greater => {
                    add_op!(IfGt);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::LessEq => {
                    add_op!(IfLe);
                    self.compile_expr(l);
                    self.compile_expr(r);
                }
                BinOp::GreaterEq => {
                    add_op!(IfGe);
                    self.compile_expr(l);
                    self.compile_expr(r);
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
                    let start = self.code.len();
                    self.compile_expr(l);
                    let mut num_vars = self.code.len() - start;
                    let mut vars = self.code.drain(start..);
                    let mut v = vec![];
                    for i in (1..5usize).rev() {
                        if num_vars == 0 {
                            break;
                        };
                        let op = match r.get_expr().get_literal() {
                            Some(Literal::Identifier(s)) if s == "Buffer" => Op::BufRead1 as usize,
                            Some(Literal::Identifier(s)) if s == "FBuffer" => {
                                Op::FBufRead1 as usize
                            }
                            _ => panic!(),
                        } - 1
                            + i;
                        let num_times = num_vars / i;
                        num_vars %= i;
                        v.push(op as u32);
                        v.push(num_times as u32);
                        for _ in 0..num_times {
                            v.push(vars.next().unwrap());
                        }
                    }
                    drop(vars);
                    self.code.append(&mut v);
                }
                BinOp::Comma => {
                    self.compile_expr(l);
                    self.compile_expr(r);
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

                self.code.push(get_var(ident, index));
            }
            Expr::FuncCall(func, args) => {
                let addr = match func.borrow() {
                    Literal::Identifier(i) => self
                        .get_func(i, false)
                        .unwrap_or_else(|| panic!("Missing function: {}", i)),
                    _ => unreachable!(),
                };
                if addr.1 == -1 {
                    self.code.push(Op::Call as u32);
                    self.code.push(args.len() as u32 + 1);
                    self.code.push(addr.0);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                } else {
                    self.code.push(addr.0);
                    self.code.push(addr.1 as u32);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                }
            }
            Expr::ArrayAssign(ident, expr) => {
                let ident = match ident.borrow() {
                    Literal::Identifier(i) => i,
                    _ => unreachable!(),
                };

                match ident.as_str() {
                    "Buffer" => add_op!(UseBuf),
                    "FBuffer" => add_op!(UseFBuf),
                    "Array" => add_op!(UseArray),
                    "FlagArray" => add_op!(UseFlags),
                    _ => panic!(),
                }
                self.compile_expr(expr);
            }
            Expr::Default => {
                add_op!(CaseDefault);
            }
        }
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
        "Var" => index - 30000000,
        "MapVar" => index - 50000000,
        "Flag" => index - 70000000,
        "MapFlag" => index - 90000000,
        "AreaFlag" => index - 110000000,
        "GameFlag" => index - 130000000,
        "AreaByte" => index - 150000000,
        "GameByte" => index - 170000000,
        "Array" => index - 190000000,
        "FlagArray" => index - 210000000,
        _ => panic!(),
    }) as u32
}
