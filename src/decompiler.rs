use std::{
    fmt,
    io::{Cursor, Write},
};

use num_enum::TryFromPrimitive;

use crate::{
    compiler::Op,
    lexer::{Literal, Number},
    parser::{BinOp, Expr, Stmt, UnOp},
};

struct Cur<T> {
    inner: T,
    pos: usize,
}

impl<T> Cur<T> {
    fn new(inner: T) -> Self {
        Self { inner, pos: 0 }
    }
}

impl<T> Cur<T>
where
    T: AsRef<[u8]>,
{
    fn read_u32(&mut self) -> Option<u32> {
        self.pos += 4;
        self.inner
            .as_ref()
            .get(self.pos - 4..self.pos)
            .and_then(|x| x.try_into().ok())
            .map(u32::from_be_bytes)
    }

    fn peek(&mut self) -> Option<u32> {
        self.inner
            .as_ref()
            .get(self.pos..self.pos + 4)
            .and_then(|x| x.try_into().ok())
            .map(u32::from_be_bytes)
    }

    fn seek(&mut self, amt: usize) {
        self.pos += amt;
    }
}

pub fn decompile_script(code: &[u8]) -> Result<Stmt, ()> {
    let mut c = Cur::new(code);
    let mut block = vec![];

    while Op::try_from_primitive(c.peek().unwrap()).unwrap() != Op::End {
        let s = match decompile_inst(&mut c) {
            Ok(e) => e,
            Err(_) => break,
        };
        println!("{:?}", s);
        block.push(s)
    }

    Ok(Stmt::Script(
        Literal::Identifier(String::from("yoooo")),
        Box::new(Stmt::Block(block)),
    ))
}

fn decompile_inst(c: &mut Cur<&[u8]>) -> Result<Stmt, ()> {
    let op = Op::try_from_primitive(c.read_u32().unwrap()).unwrap();
    let num_args = c.read_u32().unwrap();

    if let Some(x) = op.get_arg_count() {
        if num_args != x {
            panic!(
                "Op: {:?} has {} args instead of expected {}",
                op, num_args, x
            );
        };
    }

    Ok(match op {
        Op::Return => Stmt::Return,
        Op::Label => {
            let l = Literal::Number(Number::Int(c.read_u32().unwrap()));
            Stmt::Label(l)
        }
        Op::Goto => {
            let l = Literal::Number(Number::Int(c.read_u32().unwrap()));
            Stmt::Goto(l)
        }
        Op::Loop => {
            let expr = decompile_expr(c)?;
            let expr = if let Expr::Identifier(Literal::Number(Number::Int(0))) = expr {
                None
            } else {
                Some(expr)
            };
            let mut body = vec![];
            loop {
                if Op::try_from_primitive(c.peek().unwrap()).unwrap() == Op::EndLoop {
                    c.seek(8);
                    break;
                }
                body.push(decompile_inst(c).unwrap());
            }

            Stmt::Loop(expr, Box::new(Stmt::Block(body)))
        }
        Op::BreakLoop => Stmt::BreakLoop,
        Op::IfEq => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::Equal, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfNe => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::NotEqual, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfLt => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::Less, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfGt => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::Greater, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfLe => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::LessEq, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfGe => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::GreaterEq, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfFlag => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::BinOp(BinOp::BitAnd, Box::new(lhs), Box::new(rhs));

            decompile_if_stmt(c, expr)?
        }
        Op::IfNotFlag => {
            let lhs = decompile_expr(c)?;
            let rhs = decompile_expr(c)?;
            let expr = Expr::UnOp(
                UnOp::Bang,
                Box::new(Expr::BinOp(BinOp::BitAnd, Box::new(lhs), Box::new(rhs))),
            );

            decompile_if_stmt(c, expr)?
        }
        Op::Switch | Op::SwitchConst => {
            let val = decompile_expr(c)?;
            let mut case = vec![];

            'a: loop {
                let op = Op::try_from_primitive(c.read_u32().unwrap()).unwrap();
                assert_eq!(op.get_arg_count().unwrap(), c.read_u32().unwrap());
                let expr = if op == Op::CaseDefault {
                    Expr::Default
                } else if op.get_arg_count().unwrap() == 1 {
                    let op = match op {
                        Op::CaseEq => UnOp::Equal,
                        Op::CaseNe => UnOp::NotEqual,
                        Op::CaseGt => UnOp::Greater,
                        Op::CaseGe => UnOp::GreaterEq,
                        Op::CaseLt => UnOp::Less,
                        Op::CaseLe => UnOp::LessEq,
                        Op::CaseFlag => UnOp::Ampersand,
                        Op::CaseOrEq => {
                            let mut val = decompile_expr(c)?;
                            let mut block = vec![];
                            loop {
                                let op = Op::try_from_primitive(c.peek().unwrap()).unwrap();
                                if op == Op::CaseOrEq {
                                    c.seek(4);
                                    assert_eq!(op.get_arg_count().unwrap(), c.read_u32().unwrap());
                                    let v = decompile_expr(c)?;
                                    val = Expr::BinOp(BinOp::BitOr, Box::new(val), Box::new(v));
                                } else {
                                    while !matches!(
                                        Op::try_from_primitive(c.peek().unwrap()).unwrap(),
                                        Op::CaseEq
                                            | Op::CaseNe
                                            | Op::CaseGt
                                            | Op::CaseGe
                                            | Op::CaseLt
                                            | Op::CaseLe
                                            | Op::CaseFlag
                                            | Op::CaseRange
                                            | Op::CaseAndEq
                                            | Op::EndCaseGroup
                                            | Op::CaseDefault
                                    ) {
                                        block.push(decompile_inst(c)?);
                                    }
                                    break;
                                }
                            }
                            if Op::try_from_primitive(c.peek().unwrap()).unwrap()
                                == Op::EndCaseGroup
                            {
                                c.seek(8);
                            }
                            case.push(Stmt::Case(val, Box::new(Stmt::Block(block))));
                            continue;
                        }
                        Op::CaseAndEq => {
                            let mut val = decompile_expr(c)?;
                            let mut block = vec![];
                            loop {
                                let op = Op::try_from_primitive(c.peek().unwrap()).unwrap();
                                if op == Op::CaseAndEq {
                                    c.seek(4);
                                    assert_eq!(op.get_arg_count().unwrap(), c.read_u32().unwrap());
                                    let v = decompile_expr(c)?;
                                    val = Expr::BinOp(BinOp::BitAnd, Box::new(val), Box::new(v));
                                } else {
                                    while !matches!(
                                        Op::try_from_primitive(c.peek().unwrap()).unwrap(),
                                        Op::CaseEq
                                            | Op::CaseNe
                                            | Op::CaseGt
                                            | Op::CaseGe
                                            | Op::CaseLt
                                            | Op::CaseLe
                                            | Op::CaseFlag
                                            | Op::CaseRange
                                            | Op::CaseOrEq
                                            | Op::EndCaseGroup
                                            | Op::CaseDefault
                                    ) {
                                        block.push(decompile_inst(c)?);
                                    }
                                    break;
                                }
                            }
                            if Op::try_from_primitive(c.peek().unwrap()).unwrap()
                                == Op::EndCaseGroup
                            {
                                c.seek(8);
                            }
                            case.push(Stmt::Case(val, Box::new(Stmt::Block(block))));
                            continue;
                        }
                        e => panic!("{:?}", e),
                    };
                    let val = decompile_expr(c)?;
                    Expr::UnOp(op, Box::new(val))
                } else if op == Op::CaseRange {
                    let val1 = decompile_expr(c)?;
                    let val2 = decompile_expr(c)?;
                    Expr::BinOp(BinOp::Range, Box::new(val1), Box::new(val2))
                } else if op == Op::EndSwitch {
                    break 'a;
                } else {
                    panic!("{:?}", op)
                };
                let mut block = vec![];
                while !matches!(
                    Op::try_from_primitive(c.peek().unwrap()).unwrap(),
                    Op::CaseEq
                        | Op::CaseNe
                        | Op::CaseGt
                        | Op::CaseGe
                        | Op::CaseLt
                        | Op::CaseLe
                        | Op::CaseFlag
                        | Op::CaseRange
                        | Op::CaseAndEq
                        | Op::CaseOrEq
                        | Op::EndCaseGroup
                        | Op::CaseDefault
                        | Op::EndSwitch
                ) {
                    block.push(decompile_inst(c)?);
                }

                case.push(Stmt::Case(expr, Box::new(Stmt::Block(block))));
            }

            Stmt::Switch(val, Box::new(Stmt::Block(case)))
        }
        Op::BreakSwitch => Stmt::BreakCase,
        Op::Set | Op::SetF => Stmt::Expr(Expr::BinOp(
            BinOp::Assign,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::SetConst => Stmt::Expr(Expr::BinOp(
            BinOp::Assign,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::UnOp(UnOp::Ampersand, Box::new(decompile_expr(c)?))),
        )),
        Op::Add | Op::AddF => Stmt::Expr(Expr::BinOp(
            BinOp::PlusEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::Sub | Op::SubF => Stmt::Expr(Expr::BinOp(
            BinOp::MinusEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::Mul | Op::MulF => Stmt::Expr(Expr::BinOp(
            BinOp::StarEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::Div | Op::DivF => Stmt::Expr(Expr::BinOp(
            BinOp::DivEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::Mod => Stmt::Expr(Expr::BinOp(
            BinOp::ModEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::UseBuf => Stmt::Expr(Expr::ArrayAssign(
            Literal::Identifier("Buffer".to_string()),
            Box::new(decompile_expr(c)?),
        )),
        Op::BufRead1 => Stmt::Expr(Expr::BinOp(
            BinOp::Arrow,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::Identifier(Literal::Identifier("Buffer".to_string()))),
        )),
        Op::BufRead2 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("Buffer".to_string()))),
            )),
        )),
        Op::BufRead3 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(Expr::BinOp(
                BinOp::Comma,
                Box::new(decompile_expr(c)?),
                Box::new(decompile_expr(c)?),
            )),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("Buffer".to_string()))),
            )),
        )),
        Op::BufRead4 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(Expr::BinOp(
                BinOp::Comma,
                Box::new(Expr::BinOp(
                    BinOp::Comma,
                    Box::new(decompile_expr(c)?),
                    Box::new(decompile_expr(c)?),
                )),
                Box::new(decompile_expr(c)?),
            )),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("Buffer".to_string()))),
            )),
        )),
        Op::BufPeek => Stmt::Expr(Expr::BinOp(
            BinOp::Assign,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::Identifier(Literal::Identifier("Buffer".to_string()))),
        )),
        Op::UseFBuf => Stmt::Expr(Expr::ArrayAssign(
            Literal::Identifier("FBuffer".to_string()),
            Box::new(decompile_expr(c)?),
        )),
        Op::FBufRead1 => Stmt::Expr(Expr::BinOp(
            BinOp::Arrow,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::Identifier(Literal::Identifier("FBuffer".to_string()))),
        )),
        Op::FBufRead2 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("FBuffer".to_string()))),
            )),
        )),
        Op::FBufRead3 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(Expr::BinOp(
                BinOp::Comma,
                Box::new(decompile_expr(c)?),
                Box::new(decompile_expr(c)?),
            )),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("FBuffer".to_string()))),
            )),
        )),
        Op::FBufRead4 => Stmt::Expr(Expr::BinOp(
            BinOp::Comma,
            Box::new(Expr::BinOp(
                BinOp::Comma,
                Box::new(Expr::BinOp(
                    BinOp::Comma,
                    Box::new(decompile_expr(c)?),
                    Box::new(decompile_expr(c)?),
                )),
                Box::new(decompile_expr(c)?),
            )),
            Box::new(Expr::BinOp(
                BinOp::Arrow,
                Box::new(decompile_expr(c)?),
                Box::new(Expr::Identifier(Literal::Identifier("FBuffer".to_string()))),
            )),
        )),
        Op::FBufPeek => Stmt::Expr(Expr::BinOp(
            BinOp::Assign,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::Identifier(Literal::Identifier("FBuffer".to_string()))),
        )),
        Op::UseArray => Stmt::Expr(Expr::ArrayAssign(
            Literal::Identifier("Array".to_string()),
            Box::new(decompile_expr(c)?),
        )),
        Op::UseFlags => Stmt::Expr(Expr::ArrayAssign(
            Literal::Identifier("Flags".to_string()),
            Box::new(decompile_expr(c)?),
        )),
        Op::BitwiseAnd => Stmt::Expr(Expr::BinOp(
            BinOp::AndEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::BitwiseAndConst => Stmt::Expr(Expr::BinOp(
            BinOp::AndEq,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::UnOp(UnOp::Ampersand, Box::new(decompile_expr(c)?))),
        )),
        Op::BitwiseOr => Stmt::Expr(Expr::BinOp(
            BinOp::OrEq,
            Box::new(decompile_expr(c)?),
            Box::new(decompile_expr(c)?),
        )),
        Op::BitwiseOrConst => Stmt::Expr(Expr::BinOp(
            BinOp::OrEq,
            Box::new(decompile_expr(c)?),
            Box::new(Expr::UnOp(UnOp::Ampersand, Box::new(decompile_expr(c)?))),
        )),
        Op::Call => {
            let addr = c.read_u32().unwrap();
            let mut args = vec![];
            for _ in 0..num_args {
                args.push(decompile_expr(c).unwrap());
            }
            Stmt::Expr(Expr::FuncCall(Literal::Number(Number::Int(addr)), args))
        }
        Op::Jump => Stmt::Jump(Literal::Number(Number::Int(c.read_u32().unwrap()))),
        Op::Thread => {
            let mut block = vec![];
            while Op::try_from_primitive(c.peek().unwrap()).unwrap() != Op::EndThread {
                block.push(decompile_inst(c)?);
            }
            Stmt::Thread(Box::new(Stmt::Block(block)))
        }
        Op::ChildThread => {
            let mut block = vec![];
            while Op::try_from_primitive(c.peek().unwrap()).unwrap() != Op::EndThread {
                block.push(decompile_inst(c)?);
            }
            Stmt::ChildThread(Box::new(Stmt::Block(block)))
        }
        Op::WaitFrames
        | Op::WaitSecs
        | Op::MallocArray
        | Op::Exec
        | Op::ExecGetTid
        | Op::ExecWait
        | Op::BindTrigger
        | Op::Unbind
        | Op::KillThread
        | Op::SetPriority
        | Op::SetTimescale
        | Op::SetGroup
        | Op::BindPadlock
        | Op::SuspendGroup
        | Op::ResumeGroup
        | Op::SuspendOthers
        | Op::ResumeOthers
        | Op::SuspendThread
        | Op::ResumeThread
        | Op::IsThreadRunning => {
            let func = match op {
                Op::WaitFrames => "wait",
                Op::WaitSecs => "wait_sec",
                Op::MallocArray => "alloc",
                Op::Exec | Op::ExecGetTid => "exec",
                Op::ExecWait => "exec_wait",
                Op::BindTrigger => "bind",
                Op::Unbind => "unbind",
                Op::KillThread => "kill",
                Op::SetPriority => "set_priority",
                Op::SetTimescale => "set_timescale",
                Op::SetGroup => "set_group",
                Op::BindPadlock => "bind_lock",
                Op::SuspendGroup => "suspend_all",
                Op::ResumeGroup => "resume_all",
                Op::SuspendOthers => "suspend_others",
                Op::ResumeOthers => "resume_others",
                Op::SuspendThread => "suspend",
                Op::ResumeThread => "resume",
                Op::IsThreadRunning => "does_exist",
                _ => unreachable!(),
            }
            .to_string();
            let mut args = vec![];
            for _ in 0..num_args {
                args.push(decompile_expr(c).unwrap());
            }
            Stmt::Expr(Expr::FuncCall(Literal::Identifier(func), args))
        }
        Op::End => return Err(()),
        e => panic!("{:?}", e),
    })
}

fn decompile_if_stmt(c: &mut Cur<&[u8]>, expr: Expr) -> Result<Stmt, ()> {
    let mut if_block = vec![];
    let mut else_stmt = None;

    loop {
        let next_op = Op::try_from_primitive(c.peek().unwrap()).unwrap();
        match next_op {
            Op::EndIf => {
                c.seek(8);
                break;
            }
            Op::Else => {
                c.seek(8);

                if matches!(
                    Op::try_from_primitive(c.peek().unwrap()).unwrap(),
                    Op::IfEq
                        | Op::IfNe
                        | Op::IfGt
                        | Op::IfGe
                        | Op::IfLt
                        | Op::IfLe
                        | Op::IfFlag
                        | Op::IfNotFlag
                ) {
                    else_stmt = Some(Box::new(Stmt::Else(
                        Some(Box::new(decompile_inst(c)?)),
                        None,
                    )));
                    continue;
                }

                let mut block = vec![];
                while Op::try_from_primitive(c.peek().unwrap()).unwrap() != Op::EndIf {
                    block.push(decompile_inst(c)?);
                }

                else_stmt = Some(Box::new(Stmt::Else(
                    None,
                    Some(Box::new(Stmt::Block(block))),
                )));
                c.seek(8);
                break;
            }
            _ => {
                while !matches!(
                    Op::try_from_primitive(c.peek().unwrap()).unwrap(),
                    Op::EndIf | Op::Else
                ) {
                    if_block.push(decompile_inst(c)?);
                }
            }
        }
    }

    let if_stmt = Stmt::If(expr, Box::new(Stmt::Block(if_block)));

    Ok(Stmt::IfElse(Box::new(if_stmt), else_stmt))
}

fn decompile_expr(c: &mut Cur<&[u8]>) -> Result<Expr, ()> {
    let var = c.read_u32().unwrap() as i32;
    Ok(if var <= -220000000 {
        Expr::Identifier(Literal::Number(Number::Float(
            (var + 230000000) as f32 / 1024.0,
        )))
    } else if var <= -200000000 {
        Expr::Array(
            Literal::Identifier("FlagArray".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 210000000,
            )))),
        )
    } else if var <= -180000000 {
        Expr::Array(
            Literal::Identifier("Array".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 190000000,
            )))),
        )
    } else if var <= -160000000 {
        Expr::Array(
            Literal::Identifier("GlobalBytes".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 170000000,
            )))),
        )
    } else if var <= -140000000 {
        Expr::Array(
            Literal::Identifier("AreaBytes".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 150000000,
            )))),
        )
    } else if var <= -120000000 {
        Expr::Array(
            Literal::Identifier("GameFlag".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 130000000,
            )))),
        )
    } else if var <= -100000000 {
        Expr::Array(
            Literal::Identifier("AreaFlag".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 110000000,
            )))),
        )
    } else if var <= -80000000 {
        Expr::Array(
            Literal::Identifier("MapFlag".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 90000000,
            )))),
        )
    } else if var <= -60000000 {
        Expr::Array(
            Literal::Identifier("LocalFlag".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                var as u32 + 70000000,
            )))),
        )
    } else if var <= -40000000 {
        Expr::Array(
            Literal::Identifier("MapVar".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                (var + 50000000) as u32,
            )))),
        )
    } else if var <= -20000000 {
        Expr::Array(
            Literal::Identifier("Var".to_string()),
            Box::new(Expr::Identifier(Literal::Number(Number::Int(
                (var + 30000000) as u32,
            )))),
        )
    } else {
        Expr::Identifier(Literal::Number(Number::Int(var as u32)))
    })
}

pub fn write_a<W: Write>(out: W, code: &[u8]) {
    let mut c = Cursor::new(code);
}

fn write_indent<W: Write>(out: &mut W, s: &str, indent: usize) -> Result<(), std::io::Error> {
    write!(out, "{:1$}{2}", " ", indent * 4, s)
}

fn writeln_indent<W: Write>(out: &mut W, s: &str, indent: usize) -> Result<(), std::io::Error> {
    writeln!(out, "{:1$}{2}", " ", indent * 4, s)
}

struct Instruction {
    op: Op,
    args: Vec<u32>,
}

impl Instruction {
    fn new(op: Op, args: Vec<u32>) -> Self {
        Self { op, args }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            Op::End
            | Op::Return
            | Op::EndLoop
            | Op::BreakLoop
            | Op::Else
            | Op::EndIf
            | Op::CaseDefault
            | Op::EndCaseGroup
            | Op::BreakSwitch
            | Op::EndSwitch
            | Op::Unbind
            | Op::Thread
            | Op::EndThread
            | Op::ChildThread
            | Op::EndChildThread => {
                writeln!(f, "{:?}", self.op)
            }
            _ => panic!(),
        }
    }
}

fn fmt_var(var: i32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if var <= -220000000 {
        write!(f, "{}", (var + 230000000) as f32 / 1024.0)
    } else if var <= -200000000 {
        write!(f, "FlagArray[{}]", var + 210000000)
    } else if var <= -180000000 {
        write!(f, "Array[{}]", var + 190000000)
    } else if var <= -160000000 {
        write!(f, "GlobalBytes[{}]", var + 170000000)
    } else if var <= -140000000 {
        write!(f, "AreaBytes[{}]", var + 150000000)
    } else if var <= -120000000 {
        write!(f, "GameFlag[{}]", var + 130000000)
    } else if var <= -100000000 {
        write!(f, "AreaFlag[{}]", var + 110000000)
    } else if var <= -80000000 {
        write!(f, "MapFlag[{}]", var + 90000000)
    } else if var <= -60000000 {
        write!(f, "LocalFlag[{}]", var + 70000000)
    } else if var <= -40000000 {
        write!(f, "MapVar[{}]", var + 50000000)
    } else if var <= -20000000 {
        write!(f, "Var[{}]", var + 30000000)
    } else if var > 0 {
        write!(f, "0x{:X}", var)
    } else {
        write!(f, "{}", var)
    }
}
