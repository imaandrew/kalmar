use std::{
    fmt,
    io::{Cursor, Write},
};

use crate::{
    compiler::Op,
    error::DecompilerError,
    lexer::{Literal, Number, Span, Token, TokenKind},
    parser::{BinOp, Expr, ExprKind, Stmt, UnOp},
    StringManager,
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
    fn read_u32(&mut self) -> Result<u32, DecompilerError> {
        self.pos += 4;
        self.inner
            .as_ref()
            .get(self.pos - 4..self.pos)
            .and_then(|x| x.try_into().ok())
            .map(u32::from_be_bytes)
            .ok_or(DecompilerError::CursorOutOfBounds(
                self.pos,
                self.inner.as_ref().len(),
            ))
    }

    fn peek(&mut self) -> Result<u32, DecompilerError> {
        self.inner
            .as_ref()
            .get(self.pos..self.pos + 4)
            .and_then(|x| x.try_into().ok())
            .map(u32::from_be_bytes)
            .ok_or(DecompilerError::CursorOutOfBounds(
                self.pos + 4,
                self.inner.as_ref().len(),
            ))
    }

    fn seek(&mut self, amt: usize) {
        self.pos += amt;
    }
}

pub struct Decompiler<'a, 'b> {
    literals: &'b mut StringManager<'a>,
}

impl<'a, 'b> Decompiler<'a, 'b> {
    pub fn new(literals: &'b mut StringManager<'a>) -> Self {
        Self { literals }
    }

    pub fn decompile_script(&mut self, code: &[u8]) -> Result<Stmt, DecompilerError> {
        let mut c = Cur::new(code);
        let mut block = vec![];

        while Op::from_u32(c.peek()?)? != Op::End {
            let s = self.decompile_inst(&mut c)?;
            println!("{:?}", s);
            block.push(s)
        }

        Ok(Stmt::Script(
            new_token(
                TokenKind::Identifier,
                Some(Literal::Identifier(self.literals.add("yoooo"))),
            ),
            Box::new(Stmt::Block(block)),
        ))
    }

    fn decompile_inst(&mut self, c: &mut Cur<&[u8]>) -> Result<Stmt, DecompilerError> {
        let op = Op::from_u32(c.read_u32()?)?;
        let num_args = c.read_u32()?;

        if let Some(x) = op.get_arg_count() {
            if num_args != x {
                return Err(DecompilerError::UnexpectOpArgCount(op, num_args, x));
            };
        }

        Ok(match op {
            Op::Return => Stmt::Return,
            Op::Label => {
                let l = Literal::Number(Number::Int(c.read_u32()?));
                Stmt::Label(new_token(TokenKind::Identifier, Some(l)))
            }
            Op::Goto => {
                let l = Literal::Number(Number::Int(c.read_u32()?));
                Stmt::Goto(new_token(TokenKind::Identifier, Some(l)))
            }
            Op::Loop => {
                let expr = self.decompile_expr(c)?;
                let expr = if let ExprKind::Identifier(Token {
                    val: Some(Literal::Number(Number::Int(0))),
                    ..
                }) = expr
                {
                    None
                } else {
                    Some(expr.into())
                };
                let mut body = vec![];
                loop {
                    if Op::from_u32(c.peek()?)? == Op::EndLoop {
                        c.seek(8);
                        break;
                    }
                    body.push(self.decompile_inst(c)?);
                }

                Stmt::Loop(expr, Box::new(Stmt::Block(body)))
            }
            Op::BreakLoop => Stmt::BreakLoop,
            Op::IfEq => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::Equal, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfNe => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::NotEqual, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfLt => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::Less, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfGt => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::Greater, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfLe => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::LessEq, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfGe => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::GreaterEq, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfFlag => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::BinOp(BinOp::BitAnd, Box::new(lhs), Box::new(rhs));

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::IfNotFlag => {
                let lhs = self.decompile_expr(c)?.into();
                let rhs = self.decompile_expr(c)?.into();
                let expr = ExprKind::UnOp(
                    UnOp::Bang,
                    Box::new(ExprKind::BinOp(BinOp::BitAnd, Box::new(lhs), Box::new(rhs)).into()),
                );

                self.decompile_if_stmt(c, expr.into())?
            }
            Op::Switch | Op::SwitchConst => {
                let val = self.decompile_expr(c)?.into();
                let mut case = vec![];

                'a: loop {
                    let op = Op::from_u32(c.read_u32()?)?;
                    let args = c.read_u32()?;
                    let op_arg_count = op.get_arg_count().unwrap();
                    if op_arg_count != args {
                        return Err(DecompilerError::UnexpectOpArgCount(op, op_arg_count, args));
                    }
                    let expr = if op == Op::CaseDefault {
                        ExprKind::Default
                    } else if op_arg_count == 1 {
                        let op = match op {
                            Op::CaseEq => UnOp::Equal,
                            Op::CaseNe => UnOp::NotEqual,
                            Op::CaseGt => UnOp::Greater,
                            Op::CaseGe => UnOp::GreaterEq,
                            Op::CaseLt => UnOp::Less,
                            Op::CaseLe => UnOp::LessEq,
                            Op::CaseFlag => UnOp::Ampersand,
                            Op::CaseOrEq => {
                                let mut val = self.decompile_expr(c)?.into();
                                let mut block = vec![];
                                loop {
                                    let op = Op::from_u32(c.peek()?)?;
                                    if op == Op::CaseOrEq {
                                        c.seek(4);
                                        let args = c.read_u32()?;
                                        let op_arg_count = op.get_arg_count().unwrap();
                                        if op_arg_count != args {
                                            return Err(DecompilerError::UnexpectOpArgCount(
                                                op,
                                                op_arg_count,
                                                args,
                                            ));
                                        }
                                        let v = self.decompile_expr(c)?.into();
                                        val = ExprKind::BinOp(
                                            BinOp::BitOr,
                                            Box::new(val),
                                            Box::new(v),
                                        )
                                        .into();
                                    } else {
                                        while !matches!(
                                            Op::from_u32(c.peek()?)?,
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
                                            block.push(self.decompile_inst(c)?);
                                        }
                                        break;
                                    }
                                }
                                if Op::from_u32(c.peek()?)? == Op::EndCaseGroup {
                                    c.seek(8);
                                }
                                case.push(Stmt::Case(val, Box::new(Stmt::Block(block))));
                                continue;
                            }
                            Op::CaseAndEq => {
                                let mut val = self.decompile_expr(c)?.into();
                                let mut block = vec![];
                                loop {
                                    let op = Op::from_u32(c.peek()?)?;
                                    if op == Op::CaseAndEq {
                                        c.seek(4);
                                        let args = c.read_u32()?;
                                        let op_arg_count = op.get_arg_count().unwrap();
                                        if op_arg_count != args {
                                            return Err(DecompilerError::UnexpectOpArgCount(
                                                op,
                                                op_arg_count,
                                                args,
                                            ));
                                        }
                                        let v = self.decompile_expr(c)?.into();
                                        val = ExprKind::BinOp(
                                            BinOp::BitAnd,
                                            Box::new(val),
                                            Box::new(v),
                                        )
                                        .into();
                                    } else {
                                        while !matches!(
                                            Op::from_u32(c.peek()?)?,
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
                                            block.push(self.decompile_inst(c)?);
                                        }
                                        break;
                                    }
                                }
                                if Op::from_u32(c.peek()?)? == Op::EndCaseGroup {
                                    c.seek(8);
                                }
                                case.push(Stmt::Case(val, Box::new(Stmt::Block(block))));
                                continue;
                            }
                            e => panic!("{:?}", e),
                        };
                        let val = self.decompile_expr(c)?.into();
                        ExprKind::UnOp(op, Box::new(val))
                    } else if op == Op::CaseRange {
                        let val1 = self.decompile_expr(c)?.into();
                        let val2 = self.decompile_expr(c)?.into();
                        ExprKind::BinOp(BinOp::Range, Box::new(val1), Box::new(val2))
                    } else if op == Op::EndSwitch {
                        break 'a;
                    } else {
                        panic!("{:?}", op)
                    };
                    let mut block = vec![];
                    while !matches!(
                        Op::from_u32(c.peek()?)?,
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
                        block.push(self.decompile_inst(c)?);
                    }

                    case.push(Stmt::Case(expr.into(), Box::new(Stmt::Block(block))));
                }

                Stmt::Switch(val, Box::new(Stmt::Block(case)))
            }
            Op::BreakSwitch => Stmt::BreakCase,
            Op::Set | Op::SetF => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Assign,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::SetConst => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Assign,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::UnOp(UnOp::Ampersand, Box::new(self.decompile_expr(c)?.into()))
                            .into(),
                    ),
                )
                .into(),
            ),
            Op::Add | Op::AddF => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::PlusEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::Sub | Op::SubF => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::MinusEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::Mul | Op::MulF => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::StarEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::Div | Op::DivF => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::DivEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::Mod => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::ModEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::UseBuf => Stmt::Expr(
                ExprKind::ArrayAssign(
                    new_token(
                        TokenKind::Identifier,
                        Some(Literal::Identifier(self.literals.add("Buffer"))),
                    ),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::BufRead1 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Arrow,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::Identifier(new_token(
                            TokenKind::Identifier,
                            Some(Literal::Identifier(self.literals.add("Buffer"))),
                        ))
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::BufRead2 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("Buffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::BufRead3 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Comma,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(self.decompile_expr(c)?.into()),
                        )
                        .into(),
                    ),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("Buffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::BufRead4 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Comma,
                            Box::new(
                                ExprKind::BinOp(
                                    BinOp::Comma,
                                    Box::new(self.decompile_expr(c)?.into()),
                                    Box::new(self.decompile_expr(c)?.into()),
                                )
                                .into(),
                            ),
                            Box::new(self.decompile_expr(c)?.into()),
                        )
                        .into(),
                    ),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("Buffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::BufPeek => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Assign,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::Identifier(new_token(
                            TokenKind::Identifier,
                            Some(Literal::Identifier(self.literals.add("Buffer"))),
                        ))
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::UseFBuf => Stmt::Expr(
                ExprKind::ArrayAssign(
                    new_token(
                        TokenKind::Identifier,
                        Some(Literal::Identifier(self.literals.add("FBuffer"))),
                    ),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::FBufRead1 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Arrow,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::Identifier(new_token(
                            TokenKind::Identifier,
                            Some(Literal::Identifier(self.literals.add("FBuffer"))),
                        ))
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::FBufRead2 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("FBuffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::FBufRead3 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Comma,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(self.decompile_expr(c)?.into()),
                        )
                        .into(),
                    ),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("FBuffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::FBufRead4 => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Comma,
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Comma,
                            Box::new(
                                ExprKind::BinOp(
                                    BinOp::Comma,
                                    Box::new(self.decompile_expr(c)?.into()),
                                    Box::new(self.decompile_expr(c)?.into()),
                                )
                                .into(),
                            ),
                            Box::new(self.decompile_expr(c)?.into()),
                        )
                        .into(),
                    ),
                    Box::new(
                        ExprKind::BinOp(
                            BinOp::Arrow,
                            Box::new(self.decompile_expr(c)?.into()),
                            Box::new(
                                ExprKind::Identifier(new_token(
                                    TokenKind::Identifier,
                                    Some(Literal::Identifier(self.literals.add("FBuffer"))),
                                ))
                                .into(),
                            ),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::FBufPeek => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::Assign,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::Identifier(new_token(
                            TokenKind::Identifier,
                            Some(Literal::Identifier(self.literals.add("FBuffer"))),
                        ))
                        .into(),
                    ),
                )
                .into(),
            ),
            Op::UseArray => Stmt::Expr(
                ExprKind::ArrayAssign(
                    new_token(
                        TokenKind::Identifier,
                        Some(Literal::Identifier(self.literals.add("Array"))),
                    ),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::UseFlags => Stmt::Expr(
                ExprKind::ArrayAssign(
                    new_token(
                        TokenKind::Identifier,
                        Some(Literal::Identifier(self.literals.add("Flags"))),
                    ),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::BitwiseAnd => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::AndEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::BitwiseAndConst => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::AndEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::UnOp(UnOp::Ampersand, Box::new(self.decompile_expr(c)?.into()))
                            .into(),
                    ),
                )
                .into(),
            ),
            Op::BitwiseOr => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::OrEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(self.decompile_expr(c)?.into()),
                )
                .into(),
            ),
            Op::BitwiseOrConst => Stmt::Expr(
                ExprKind::BinOp(
                    BinOp::OrEq,
                    Box::new(self.decompile_expr(c)?.into()),
                    Box::new(
                        ExprKind::UnOp(UnOp::Ampersand, Box::new(self.decompile_expr(c)?.into()))
                            .into(),
                    ),
                )
                .into(),
            ),
            Op::Call => {
                let addr = c.read_u32()?;
                let mut args = vec![];
                for _ in 0..num_args {
                    args.push(self.decompile_expr(c)?.into());
                }
                Stmt::Expr(
                    ExprKind::FuncCall(
                        new_token(TokenKind::Number, Some(Literal::Number(Number::Int(addr)))),
                        args,
                    )
                    .into(),
                )
            }
            Op::Jump => Stmt::Jump(new_token(
                TokenKind::Number,
                Some(Literal::Number(Number::Int(c.read_u32()?))),
            )),
            Op::Thread => {
                let mut block = vec![];
                while Op::from_u32(c.peek()?)? != Op::EndThread {
                    block.push(self.decompile_inst(c)?);
                }
                Stmt::Thread(Box::new(Stmt::Block(block)))
            }
            Op::ChildThread => {
                let mut block = vec![];
                while Op::from_u32(c.peek()?)? != Op::EndThread {
                    block.push(self.decompile_inst(c)?);
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
                };
                let mut args = vec![];
                for _ in 0..num_args {
                    args.push(self.decompile_expr(c)?.into());
                }
                Stmt::Expr(
                    ExprKind::FuncCall(
                        new_token(
                            TokenKind::Identifier,
                            Some(Literal::Identifier(self.literals.add(func))),
                        ),
                        args,
                    )
                    .into(),
                )
            }
            Op::End => return Err(DecompilerError::UnexpectedEndToken),
            e => panic!("{:?}", e),
        })
    }

    fn decompile_if_stmt(
        &mut self,
        c: &mut Cur<&[u8]>,
        expr: Expr,
    ) -> Result<Stmt, DecompilerError> {
        let mut if_block = vec![];
        let mut else_stmt = None;

        loop {
            let next_op = Op::from_u32(c.peek()?)?;
            match next_op {
                Op::EndIf => {
                    c.seek(8);
                    break;
                }
                Op::Else => {
                    c.seek(8);

                    if matches!(
                        Op::from_u32(c.peek()?)?,
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
                            Some(Box::new(self.decompile_inst(c)?)),
                            None,
                        )));
                        continue;
                    }

                    let mut block = vec![];
                    while Op::from_u32(c.peek()?)? != Op::EndIf {
                        block.push(self.decompile_inst(c)?);
                    }

                    else_stmt = Some(Box::new(Stmt::Else(
                        None,
                        Some(Box::new(Stmt::Block(block))),
                    )));
                    c.seek(8);
                    break;
                }
                _ => {
                    while !matches!(Op::from_u32(c.peek()?)?, Op::EndIf | Op::Else) {
                        if_block.push(self.decompile_inst(c)?);
                    }
                }
            }
        }

        let if_stmt = Stmt::If(expr, Box::new(Stmt::Block(if_block)));

        Ok(Stmt::IfElse(Box::new(if_stmt), else_stmt))
    }

    fn decompile_expr(&mut self, c: &mut Cur<&[u8]>) -> Result<ExprKind, DecompilerError> {
        let var = c.read_u32()? as i32;
        Ok(if var <= -220000000 {
            ExprKind::Identifier(new_token(
                TokenKind::Number,
                Some(Literal::Number(Number::Float(
                    (var + 230000000) as f32 / 1024.0,
                ))),
            ))
        } else if var <= -200000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("FlagArray"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 210000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -180000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("Array"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 190000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -160000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("GlobalBytes"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 170000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -140000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("AreaBytes"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 150000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -120000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("GameFlag"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 130000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -100000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("AreaFlag"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 110000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -80000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("MapFlag"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 90000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -60000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("LocalFlag"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 70000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -40000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("MapVar"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 50000000))),
                    ))
                    .into(),
                ),
            )
        } else if var <= -20000000 {
            ExprKind::Array(
                new_token(
                    TokenKind::Identifier,
                    Some(Literal::Identifier(self.literals.add("Var"))),
                ),
                Box::new(
                    ExprKind::Identifier(new_token(
                        TokenKind::Identifier,
                        Some(Literal::Number(Number::Int(var as u32 + 30000000))),
                    ))
                    .into(),
                ),
            )
        } else {
            ExprKind::Identifier(new_token(
                TokenKind::Number,
                Some(Literal::Number(Number::Int(var as u32))),
            ))
        })
    }
}

fn new_token(kind: TokenKind, val: Option<Literal>) -> Token {
    Token {
        kind,
        val,
        span: Span::default(),
    }
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
