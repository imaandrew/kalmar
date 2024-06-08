use std::fmt::Display;

use crate::{
    error::NewKalmarError,
    lexer::{Literal, Token, TokenKind},
    StringManager,
};

#[derive(Debug)]
pub enum Stmt {
    Script(Token, Box<Self>),
    Block(Vec<Self>),
    Return,
    BreakLoop,
    BreakCase,
    Label(Token),
    Goto(Token),
    Loop(Option<Expr>, Box<Self>),
    IfElse(Box<Self>, Option<Box<Self>>),
    If(Expr, Box<Self>),
    Else(Option<Box<Stmt>>, Option<Box<Stmt>>),
    Jump(Token),
    Thread(Box<Self>),
    ChildThread(Box<Self>),
    Expr(Expr),
    Switch(Expr, Box<Self>),
    Case(Expr, Box<Self>),
    Empty,
}

impl Default for Stmt {
    fn default() -> Self {
        Self::Empty
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn recursive_fmt(
            f: &mut std::fmt::Formatter<'_>,
            stmt: &Stmt,
            indent_level: usize,
        ) -> std::fmt::Result {
            let indent = " ".repeat((indent_level + 1) * 2);
            match stmt {
                Stmt::Script(id, stmts) => {
                    writeln!(f, "Script {}", id.val.unwrap())?;
                    recursive_fmt(f, stmts, indent_level)
                }
                Stmt::Block(stmts) => {
                    for s in stmts {
                        write!(f, "{}", indent)?;
                        recursive_fmt(f, s, indent_level + 1)?;
                    }
                    Ok(())
                }
                Stmt::Return => writeln!(f, "Return"),
                Stmt::BreakLoop => writeln!(f, "BreakLoop"),
                Stmt::BreakCase => writeln!(f, "BreakCase"),
                Stmt::Label(l) => writeln!(f, "Label {}", l.val.unwrap()),
                Stmt::Goto(l) => writeln!(f, "Goto {}", l.val.unwrap()),
                Stmt::Loop(e, s) => {
                    if let Some(e) = e.as_ref() {
                        writeln!(f, "Loop {}", e.kind)?;
                    } else {
                        writeln!(f, "Loop")?;
                    }
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::IfElse(i, e) => {
                    recursive_fmt(f, i, indent_level)?;
                    for s in e {
                        let indent = " ".repeat(indent_level * 2);
                        write!(f, "{}", indent)?;
                        recursive_fmt(f, s, indent_level + 1)?;
                    }

                    Ok(())
                }
                Stmt::If(e, s) => {
                    writeln!(f, "If {}", e.kind)?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Else(i, e) => {
                    if let Some(i) = i {
                        write!(f, "Else ")?;
                        recursive_fmt(f, i, indent_level - 1)
                    } else if let Some(e) = e {
                        writeln!(f, "Else ")?;
                        recursive_fmt(f, e, indent_level - 1)
                    } else {
                        unreachable!()
                    }
                }
                Stmt::Jump(l) => writeln!(f, "Jump {}", l.val.unwrap()),
                Stmt::Thread(s) => {
                    writeln!(f, "Thread")?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::ChildThread(s) => {
                    writeln!(f, "ChildThread")?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Expr(e) => writeln!(f, "{}", e.kind),
                Stmt::Switch(e, s) => {
                    writeln!(f, "Switch {}", e.kind)?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Case(e, s) => {
                    writeln!(f, "Case {}", e.kind)?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Empty => writeln!(f, "Empty"),
            }
        }

        recursive_fmt(f, self, 0)
    }
}

#[derive(Debug, Clone, Copy, strum_macros::Display)]
pub enum UnOp {
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "!")]
    Bang,
    #[strum(serialize = "&")]
    Ampersand,
    #[strum(serialize = "==")]
    Equal,
    #[strum(serialize = "!=")]
    NotEqual,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = ">=")]
    GreaterEq,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = "<=")]
    LessEq,
}

impl TryFrom<Token> for UnOp {
    type Error = NewKalmarError;

    fn try_from(t: Token) -> Result<Self, Self::Error> {
        match t.kind {
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::And => Ok(Self::Ampersand),
            TokenKind::EqEq => Ok(Self::Equal),
            TokenKind::BangEq => Ok(Self::NotEqual),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            _ => Err(NewKalmarError::InvalidOperator("unary", t)),
        }
    }
}

impl UnOp {
    fn precedence(&self) -> (u8, u8) {
        match self {
            Self::Bang | Self::Minus | Self::Ampersand => (20, 19),
            Self::Equal
            | Self::NotEqual
            | Self::Greater
            | Self::GreaterEq
            | Self::Less
            | Self::LessEq => (10, 9),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, strum_macros::Display)]
pub enum BinOp {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Star,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "%")]
    Mod,
    #[strum(serialize = "|")]
    BitOr,
    #[strum(serialize = "&")]
    BitAnd,
    #[strum(serialize = "+=")]
    PlusEq,
    #[strum(serialize = "-=")]
    MinusEq,
    #[strum(serialize = "*=")]
    StarEq,
    #[strum(serialize = "/=")]
    DivEq,
    #[strum(serialize = "%=")]
    ModEq,
    #[strum(serialize = "|=")]
    OrEq,
    #[strum(serialize = "&=")]
    AndEq,
    #[strum(serialize = "==")]
    Equal,
    #[strum(serialize = "!=")]
    NotEqual,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = ">=")]
    GreaterEq,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = "<=")]
    LessEq,
    #[strum(serialize = "..")]
    Range,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = "<-")]
    Arrow,
    #[strum(serialize = ",")]
    Comma,
}

impl TryFrom<Token> for BinOp {
    type Error = NewKalmarError;

    fn try_from(t: Token) -> Result<Self, Self::Error> {
        match t.kind {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Star),
            TokenKind::Slash => Ok(Self::Div),
            TokenKind::Percent => Ok(Self::Mod),
            TokenKind::Pipe => Ok(Self::BitOr),
            TokenKind::And => Ok(Self::BitAnd),
            TokenKind::PlusEq => Ok(Self::PlusEq),
            TokenKind::MinusEq => Ok(Self::MinusEq),
            TokenKind::StarEq => Ok(Self::StarEq),
            TokenKind::SlashEq => Ok(Self::DivEq),
            TokenKind::PercentEq => Ok(Self::ModEq),
            TokenKind::OrEq => Ok(Self::OrEq),
            TokenKind::AndEq => Ok(Self::AndEq),
            TokenKind::EqEq => Ok(Self::Equal),
            TokenKind::BangEq => Ok(Self::NotEqual),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            TokenKind::Range => Ok(Self::Range),
            TokenKind::Eq => Ok(Self::Assign),
            TokenKind::Comma => Ok(Self::Comma),
            TokenKind::Arrow => Ok(Self::Arrow),
            _ => Err(NewKalmarError::InvalidOperator("binary", t)),
        }
    }
}

impl BinOp {
    fn precedence(&self) -> (u8, u8) {
        match self {
            Self::Range | Self::Comma => (90, 91),
            Self::Star | Self::Div | Self::Mod => (80, 81),
            Self::Plus | Self::Minus => (70, 71),
            Self::Greater | Self::GreaterEq | Self::Less | Self::LessEq => (60, 61),
            Self::Equal | Self::NotEqual => (50, 51),
            Self::BitAnd => (40, 41),
            Self::BitOr => (30, 31),
            Self::Assign
            | Self::Arrow
            | Self::PlusEq
            | Self::MinusEq
            | Self::StarEq
            | Self::DivEq
            | Self::ModEq
            | Self::OrEq
            | Self::AndEq => (20, 19),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    fn new(kind: ExprKind, t: &Token) -> Self {
        Self {
            kind,
            span: Span {
                line: t.line,
                col: t.col,
                len: t.len,
            },
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Identifier(Token),
    Array(Token, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    FuncCall(Token, Vec<Expr>),
    ArrayAssign(Token, Box<Expr>),
    Default,
}

impl ExprKind {
    pub fn get_literal(&self) -> Option<Literal> {
        match &self {
            Self::Identifier(l) => Some(l.val.unwrap()),
            _ => None,
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(l) => write!(f, "{}", l.val.unwrap()),
            Self::Array(l, e) => write!(f, "{}[{}]", l.val.unwrap(), e.kind),
            Self::UnOp(op, e) => f.write_fmt(format_args!("{} {}", op, e.kind)),
            Self::BinOp(op, l, r) => f.write_fmt(format_args!("{} {} {}", l.kind, op, r.kind)),
            Self::FuncCall(l, args) => {
                write!(f, "{} ", l.val.unwrap())?;
                for a in args {
                    write!(f, "( {} ) ", a.kind)?
                }
                Ok(())
            }
            Self::Default => write!(f, "Default"),
            Self::ArrayAssign(l, e) => write!(f, "{} = {}", l.val.unwrap(), e.kind),
        }
    }
}

pub struct Parser<'parsr, 'smgr> {
    tokens: &'parsr Vec<Token>,
    literals: &'parsr mut StringManager<'smgr>,
    curr: usize,
    verbose: bool,
    parsing_func_args: bool,
}

impl<'parsr, 'smgr> Parser<'parsr, 'smgr> {
    pub fn new(tokens: &'parsr Vec<Token>, literals: &'parsr mut StringManager<'smgr>) -> Self {
        Parser {
            tokens,
            literals,
            curr: 0,
            verbose: false,
            parsing_func_args: false,
        }
    }

    pub fn parse(&mut self, verbose: bool) -> Result<Vec<Stmt>, NewKalmarError> {
        let mut stmts = vec![];
        self.verbose = verbose;
        self.skip_newlines()?;
        loop {
            if self.at_end() {
                break;
            }
            stmts.push(self.declaration()?);
            if !self.at_end() {
                self.assert(TokenKind::Newline)?;
            }
            self.skip_newlines()?;
        }

        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt, NewKalmarError> {
        self.assert(TokenKind::KwScr)?;
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(Stmt::Script(
            *ident,
            Box::new(self.block(Self::statement, true)?),
        ))
    }

    fn block<F>(&mut self, stmt_func: F, end_stmt_newlines: bool) -> Result<Stmt, NewKalmarError>
    where
        F: Fn(&mut Self) -> Result<Stmt, NewKalmarError>,
    {
        self.consume(TokenKind::LBrace)?;
        self.assert(TokenKind::Newline)?;
        self.skip_newlines()?;
        let mut stmts = vec![];

        while !self.peek(TokenKind::RBrace)? {
            stmts.push(stmt_func(self)?);
            if end_stmt_newlines {
                self.assert(TokenKind::Newline)?;
            }
            self.skip_newlines()?;
        }

        self.assert(TokenKind::RBrace)?;

        Ok(Stmt::Block(stmts))
    }

    fn statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let t = self.pop()?;
        let s = match t.kind {
            TokenKind::KwReturn => Ok(Stmt::Return),
            TokenKind::KwBreakLoop => Ok(Stmt::BreakLoop),
            TokenKind::KwBreakCase => Ok(Stmt::BreakCase),
            TokenKind::KwGoto => self.goto_statement(),
            TokenKind::KwLoop => self.loop_statement(),
            TokenKind::KwIf => self.if_else_statement(),
            TokenKind::KwJump => self.jump_statement(),
            TokenKind::KwThread => self.thread_statement(),
            TokenKind::KwChildThread => self.child_thread_statement(),
            TokenKind::KwSwitch => self.switch_statement(),
            TokenKind::Identifier => {
                if self.peek(TokenKind::Colon)? {
                    self.rewind()?;
                    return self.label_statement();
                }
                self.rewind()?;
                Ok(Stmt::Expr(self.expr(0)?))
            }
            _ => Err(NewKalmarError::ExpectedStmt(*t)),
        }?;

        Ok(s)
    }

    fn goto_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let ident = self.consume(TokenKind::Identifier)?;

        Ok(Stmt::Goto(*ident))
    }

    fn label_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let lbl = *self.consume(TokenKind::Identifier)?;
        self.assert(TokenKind::Colon)?;

        Ok(Stmt::Label(lbl))
    }

    fn loop_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let loop_count = if self.peek(TokenKind::LBrace)? {
            None
        } else {
            Some(self.expr(0)?)
        };

        let block = self.block(Self::statement, true)?;

        Ok(Stmt::Loop(loop_count, Box::new(block)))
    }

    fn if_else_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let if_stmt = self.if_statement()?;

        let else_stmt = if self.peek_over_newlines(TokenKind::KwElse) {
            self.skip_newlines()?;
            self.assert(TokenKind::KwElse)?;
            Some(Box::new(self.else_statement()?))
        } else {
            None
        };

        Ok(Stmt::IfElse(Box::new(if_stmt), else_stmt))
    }

    fn if_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let if_expr = self.expr(0)?;
        let if_block = self.block(Self::statement, true)?;
        Ok(Stmt::If(if_expr, Box::new(if_block)))
    }

    fn else_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        Ok(if self.peek(TokenKind::KwIf)? {
            self.assert(TokenKind::KwIf)?;
            let if_stmt = self.if_else_statement()?;
            Stmt::Else(Some(Box::new(if_stmt)), None)
        } else {
            let else_block = self.block(Self::statement, true)?;
            Stmt::Else(None, Some(Box::new(else_block)))
        })
    }

    fn jump_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(Stmt::Jump(*ident))
    }

    fn thread_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        Ok(Stmt::Thread(Box::new(self.block(Self::statement, true)?)))
    }

    fn child_thread_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        Ok(Stmt::ChildThread(Box::new(
            self.block(Self::statement, true)?,
        )))
    }

    fn switch_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let val = self.expr(0)?;
        let block = self.block(Self::case_statement, false)?;

        Ok(Stmt::Switch(val, Box::new(block)))
    }

    fn case_statement(&mut self) -> Result<Stmt, NewKalmarError> {
        let case = match self.kind()? {
            TokenKind::KwDefault => Expr::new(ExprKind::Default, self.pop()?),
            _ => self.expr(0)?,
        };
        let block = self.block(Self::statement, true)?;

        Ok(Stmt::Case(case, Box::new(block)))
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, NewKalmarError> {
        let t = *self.pop()?;
        let mut left = match t.kind {
            TokenKind::Number | TokenKind::Boolean => Expr::new(ExprKind::Identifier(t), &t),
            TokenKind::Identifier => match self.kind()? {
                TokenKind::LBracket => {
                    self.pop()?;
                    let idx = self.expr(0)?;
                    let rb = self.consume(TokenKind::RBracket)?;
                    Expr {
                        kind: ExprKind::Array(t, Box::new(idx)),
                        span: Span {
                            line: t.line,
                            col: t.col,
                            len: rb.col + rb.len - t.len,
                        },
                    }
                }
                TokenKind::LParen => {
                    self.pop()?;
                    let mut args = vec![];
                    self.parsing_func_args = true;
                    loop {
                        match self.kind()? {
                            TokenKind::RParen => {
                                let rp = *self.pop()?;
                                self.parsing_func_args = false;
                                return Ok(Expr {
                                    kind: ExprKind::FuncCall(t, args),
                                    span: Span {
                                        line: t.line,
                                        col: t.col,
                                        len: rp.col + rp.len - t.col,
                                    },
                                });
                            }
                            TokenKind::Comma => {
                                self.pop()?;
                            }
                            _ => args.push(self.expr(0)?),
                        }
                    }
                }
                _ => Expr::new(ExprKind::Identifier(t), &t),
            },
            TokenKind::Minus
            | TokenKind::Bang
            | TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Greater
            | TokenKind::GreaterEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::And => {
                let op = UnOp::try_from(t)?;
                let r = self.expr(op.precedence().1)?;
                let s = r.span;
                Expr {
                    kind: ExprKind::UnOp(op, Box::new(r)),
                    span: Span {
                        line: t.line,
                        col: t.col,
                        len: s.col + s.len - t.col,
                    },
                }
            }
            TokenKind::LParen => {
                let expr = self.expr(0)?;
                let rp = self.consume(TokenKind::RParen)?;
                Expr {
                    kind: expr.kind,
                    span: Span {
                        line: t.line,
                        col: t.col,
                        len: rp.col + rp.len - t.col,
                    },
                }
            }
            _ => return Err(NewKalmarError::ExpectedExpr(t)),
        };

        loop {
            let t = *self.pop()?;
            if self.parsing_func_args && t.kind == TokenKind::Comma {
                self.rewind()?;
                break;
            }
            let op = match BinOp::try_from(t) {
                Ok(op) => op,
                Err(e) => {
                    // TODO: ew
                    if matches!(
                        t.kind,
                        TokenKind::LBrace
                            | TokenKind::RBracket
                            | TokenKind::RParen
                            | TokenKind::Newline
                    ) {
                        self.rewind()?;
                        break;
                    }
                    return Err(e);
                }
            };

            if op.precedence().0 < min_prec {
                self.rewind()?;
                break;
            }

            let right = self.expr(op.precedence().1)?;
            if op == BinOp::Assign {
                if let ExprKind::Identifier(
                    lit @ Token {
                        val: Some(Literal::Identifier(s)),
                        ..
                    },
                ) = &left.kind
                {
                    let s = self.literals.get(*s).unwrap();
                    if matches!(
                        s.to_string().as_str(),
                        "Buffer" | "FBuffer" | "Array" | "FlagArray"
                    ) {
                        let s = right.span;
                        left = Expr {
                            kind: ExprKind::ArrayAssign(*lit, Box::new(right)),
                            span: Span {
                                line: left.span.line,
                                col: left.span.col,
                                len: s.col + s.len - left.span.col,
                            },
                        };
                        continue;
                    }
                }
            }
            let l_span = left.span;
            let r_span = right.span;
            left = Expr {
                kind: ExprKind::BinOp(op, Box::new(left), Box::new(right)),
                span: Span {
                    line: l_span.line,
                    col: l_span.col,
                    len: r_span.col + r_span.len - l_span.col,
                },
            };
        }
        Ok(left)
    }

    fn bound_check_tokens(&self) -> Result<(), NewKalmarError> {
        if self.at_end() {
            return Err(NewKalmarError::UnexpectedEndTokenStream);
        }
        Ok(())
    }

    fn pop(&mut self) -> Result<&Token, NewKalmarError> {
        self.bound_check_tokens()?;
        self.curr += 1;
        Ok(self.tokens.get(self.curr - 1).unwrap())
    }

    fn kind(&mut self) -> Result<TokenKind, NewKalmarError> {
        self.bound_check_tokens()?;
        Ok(self.tokens.get(self.curr).unwrap().kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Result<&Token, NewKalmarError> {
        let t = self.pop()?;
        if t.kind == kind {
            return Ok(t);
        }

        Err(NewKalmarError::UnexpectedToken(*t, kind))
    }

    fn peek(&mut self, kind: TokenKind) -> Result<bool, NewKalmarError> {
        self.bound_check_tokens()?;
        let tok = self.tokens.get(self.curr).unwrap();
        Ok(tok.kind == kind)
    }

    fn peek_over_newlines(&mut self, kind: TokenKind) -> bool {
        let mut i = self.curr;
        while let Some(t) = self.tokens.get(i) {
            if t.kind != TokenKind::Newline {
                return t.kind == kind;
            }
            i += 1;
        }

        false
    }

    fn assert(&mut self, kind: TokenKind) -> Result<(), NewKalmarError> {
        let t = self.pop()?;
        if t.kind != kind {
            Err(NewKalmarError::UnexpectedToken(*t, kind))
        } else {
            Ok(())
        }
    }

    fn skip_newlines(&mut self) -> Result<(), NewKalmarError> {
        while !self.at_end() && self.kind()? == TokenKind::Newline {
            self.pop()?;
        }
        Ok(())
    }

    fn rewind(&mut self) -> Result<(), NewKalmarError> {
        if self.curr > 0 {
            self.curr -= 1;
            return Ok(());
        }
        panic!()
    }

    fn at_end(&self) -> bool {
        self.curr >= self.tokens.len() || self.tokens.get(self.curr).unwrap().kind == TokenKind::Eof
    }
}
