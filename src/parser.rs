use std::fmt::Display;

use crate::{
    error::KalmarError,
    lexer::{Lexer, Literal, Token, TokenKind},
};

#[derive(Debug)]
pub enum Stmt {
    Script(Literal, Box<Self>),
    Block(Vec<Self>),
    Return,
    BreakLoop,
    BreakCase,
    Label(Literal),
    Goto(Literal),
    Loop(Option<Expr>, Box<Self>),
    IfElse(Box<Self>, Option<Box<Self>>),
    If(Expr, Box<Self>),
    Else(Option<Box<Stmt>>, Option<Box<Stmt>>),
    Jump(Literal),
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
                    writeln!(f, "Script {}", id)?;
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
                Stmt::Label(l) => writeln!(f, "Label {}", l),
                Stmt::Goto(l) => writeln!(f, "Goto {}", l),
                Stmt::Loop(e, s) => {
                    if let Some(e) = e.as_ref() {
                        writeln!(f, "Loop {}", e)?;
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
                    writeln!(f, "If {}", e)?;
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
                Stmt::Jump(l) => writeln!(f, "Jump {}", l),
                Stmt::Thread(s) => {
                    writeln!(f, "Thread")?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::ChildThread(s) => {
                    writeln!(f, "ChildThread")?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Expr(e) => writeln!(f, "{}", e),
                Stmt::Switch(e, s) => {
                    writeln!(f, "Switch {}", e)?;
                    recursive_fmt(f, s, indent_level)
                }
                Stmt::Case(e, s) => {
                    writeln!(f, "Case {}", e)?;
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

impl TryFrom<TokenKind> for UnOp {
    type Error = KalmarError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::And => Ok(Self::Ampersand),
            TokenKind::EqEq => Ok(Self::Equal),
            TokenKind::BangEq => Ok(Self::NotEqual),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            _ => Err(KalmarError::InvalidOperator("unary", value)),
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

impl TryFrom<TokenKind> for BinOp {
    type Error = KalmarError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
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
            _ => Err(KalmarError::InvalidOperator("binary", value)),
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

#[derive(Debug)]
pub enum Expr {
    Identifier(Literal),
    Array(Literal, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    FuncCall(Literal, Vec<Expr>),
    ArrayAssign(Literal, Box<Expr>),
    Default,
}

impl Expr {
    pub fn get_literal(&self) -> Option<&Literal> {
        match &self {
            Self::Identifier(l) => Some(l),
            _ => None,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(l) => write!(f, "{}", l),
            Expr::Array(l, e) => write!(f, "{}[{}]", l, e),
            Expr::UnOp(op, e) => f.write_fmt(format_args!("{} {}", op, e)),
            Expr::BinOp(op, l, r) => f.write_fmt(format_args!("{} {} {}", l, op, r)),
            Expr::FuncCall(l, args) => {
                write!(f, "{} ", l)?;
                for a in args {
                    write!(f, "( {} ) ", a)?
                }
                Ok(())
            }
            Expr::Default => write!(f, "Default"),
            Expr::ArrayAssign(l, e) => write!(f, "{} = {}", l, e),
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    tokens: Vec<Token>,
    verbose: bool,
    parsing_func_args: bool,
}

impl Parser {
    pub fn new(data: &str) -> Self {
        Parser {
            lexer: Lexer::new(data),
            tokens: vec![],
            verbose: false,
            parsing_func_args: false,
        }
    }

    pub fn parse(&mut self, verbose: bool) -> Result<Vec<Stmt>, KalmarError> {
        let mut stmts = vec![];
        self.verbose = verbose;
        self.skip_newlines()?;
        loop {
            if self.lexer.at_end() {
                break;
            }
            stmts.push(self.declaration()?);
            if !self.lexer.at_end() {
                self.assert(TokenKind::Newline)?;
            }
            self.skip_newlines()?;
        }

        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt, KalmarError> {
        self.assert(TokenKind::KwScr)?;
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(Stmt::Script(
            ident.val.unwrap(),
            Box::new(self.block(Self::statement, true)?),
        ))
    }

    fn block<F>(&mut self, stmt_func: F, end_stmt_newlines: bool) -> Result<Stmt, KalmarError>
    where
        F: Fn(&mut Self) -> Result<Stmt, KalmarError>,
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

    fn statement(&mut self) -> Result<Stmt, KalmarError> {
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
                    self.tokens.push(t);
                    return self.label_statement();
                }
                self.tokens.push(t);
                Ok(Stmt::Expr(self.expr(0)?))
            }
            _ => Err(KalmarError::ExpectedStmt(t.kind)),
        }?;

        Ok(s)
    }

    fn goto_statement(&mut self) -> Result<Stmt, KalmarError> {
        let ident = self.consume(TokenKind::Identifier)?;

        Ok(Stmt::Goto(ident.val.unwrap()))
    }

    fn label_statement(&mut self) -> Result<Stmt, KalmarError> {
        let ident = self.consume(TokenKind::Identifier)?;
        self.assert(TokenKind::Colon)?;

        Ok(Stmt::Label(ident.val.unwrap()))
    }

    fn loop_statement(&mut self) -> Result<Stmt, KalmarError> {
        let loop_count = if self.peek(TokenKind::LBrace)? {
            None
        } else {
            Some(self.expr(0)?)
        };

        let block = self.block(Self::statement, true)?;

        Ok(Stmt::Loop(loop_count, Box::new(block)))
    }

    fn if_else_statement(&mut self) -> Result<Stmt, KalmarError> {
        let if_stmt = self.if_statement()?;

        let else_stmt = if self.peek_over_newlines(TokenKind::KwElse)? {
            self.skip_newlines()?;
            self.assert(TokenKind::KwElse)?;
            Some(Box::new(self.else_statement()?))
        } else {
            None
        };

        Ok(Stmt::IfElse(Box::new(if_stmt), else_stmt))
    }

    fn if_statement(&mut self) -> Result<Stmt, KalmarError> {
        let if_expr = self.expr(0)?;
        let if_block = self.block(Self::statement, true)?;
        Ok(Stmt::If(if_expr, Box::new(if_block)))
    }

    fn else_statement(&mut self) -> Result<Stmt, KalmarError> {
        Ok(if self.peek(TokenKind::KwIf)? {
            self.assert(TokenKind::KwIf)?;
            let if_stmt = self.if_else_statement()?;
            Stmt::Else(Some(Box::new(if_stmt)), None)
        } else {
            let else_block = self.block(Self::statement, true)?;
            Stmt::Else(None, Some(Box::new(else_block)))
        })
    }

    fn jump_statement(&mut self) -> Result<Stmt, KalmarError> {
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(Stmt::Jump(ident.val.unwrap()))
    }

    fn thread_statement(&mut self) -> Result<Stmt, KalmarError> {
        Ok(Stmt::Thread(Box::new(self.block(Self::statement, true)?)))
    }

    fn child_thread_statement(&mut self) -> Result<Stmt, KalmarError> {
        Ok(Stmt::ChildThread(Box::new(
            self.block(Self::statement, true)?,
        )))
    }

    fn switch_statement(&mut self) -> Result<Stmt, KalmarError> {
        let val = self.expr(0)?;
        let block = self.block(Self::case_statement, false)?;

        Ok(Stmt::Switch(val, Box::new(block)))
    }

    fn case_statement(&mut self) -> Result<Stmt, KalmarError> {
        let case = match self.kind()? {
            TokenKind::KwDefault => {
                self.pop()?;
                Expr::Default
            }
            _ => self.expr(0)?,
        };
        let block = self.block(Self::statement, true)?;

        Ok(Stmt::Case(case, Box::new(block)))
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, KalmarError> {
        let t = self.pop()?;
        let mut left = match t.kind {
            TokenKind::Number | TokenKind::Boolean => Expr::Identifier(t.val.unwrap()),
            TokenKind::Identifier => match self.kind()? {
                TokenKind::LBracket => {
                    self.pop()?;
                    let idx = self.expr(0)?;
                    self.assert(TokenKind::RBracket)?;
                    Expr::Array(t.val.unwrap(), Box::new(idx))
                }
                TokenKind::LParen => {
                    self.pop()?;
                    let mut args = vec![];
                    self.parsing_func_args = true;
                    loop {
                        match self.kind()? {
                            TokenKind::RParen => {
                                self.pop()?;
                                self.parsing_func_args = false;
                                return Ok(Expr::FuncCall(t.val.unwrap(), args));
                            }
                            TokenKind::Comma => {
                                self.pop()?;
                            }
                            _ => args.push(self.expr(0)?),
                        }
                    }
                }
                _ => Expr::Identifier(t.val.unwrap()),
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
                let op = UnOp::try_from(t.kind)?;
                let r = self.expr(op.precedence().1)?;
                Expr::UnOp(op, Box::new(r))
            }
            TokenKind::LParen => {
                let expr = self.expr(0)?;
                self.assert(TokenKind::RParen)?;
                expr
            }
            _ => return Err(KalmarError::ExpectedExpr(t.kind)),
        };

        loop {
            let t = self.pop()?;
            if self.parsing_func_args && t.kind == TokenKind::Comma {
                self.tokens.push(t);
                break;
            }
            let op = match BinOp::try_from(t.kind) {
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
                        self.tokens.push(t);
                        break;
                    }
                    return Err(e);
                }
            };

            if op.precedence().0 < min_prec {
                self.tokens.push(t);
                break;
            }

            let right = self.expr(op.precedence().1)?;
            if op == BinOp::Assign {
                if let Expr::Identifier(Literal::Identifier(s)) = &left {
                    if matches!(s.as_str(), "Buffer" | "FBuffer" | "Array" | "FlagArray") {
                        left =
                            Expr::ArrayAssign(Literal::Identifier(s.to_string()), Box::new(right));
                        continue;
                    }
                }
            }
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn pop(&mut self) -> Result<Token, KalmarError> {
        Ok(match self.tokens.pop() {
            Some(x) => x,
            None => {
                let x = self.lexer.lex()?;
                if self.verbose {
                    println!("{:?}", x);
                }
                x
            }
        })
    }

    fn kind(&mut self) -> Result<TokenKind, KalmarError> {
        let t = self.pop()?;
        let kind = t.kind;
        self.tokens.push(t);
        Ok(kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token, KalmarError> {
        let t = self.pop()?;
        if t.kind == kind {
            return Ok(t);
        }

        Err(KalmarError::UnexpectedToken(kind, t.kind))
    }

    fn peek(&mut self, kind: TokenKind) -> Result<bool, KalmarError> {
        let tok = self.pop()?;
        let ret = tok.kind == kind;
        self.tokens.push(tok);
        Ok(ret)
    }

    fn peek_over_newlines(&mut self, kind: TokenKind) -> Result<bool, KalmarError> {
        let mut tok = self.pop()?;
        let mut toks = vec![];
        while tok.kind == TokenKind::Newline {
            toks.push(tok);
            tok = self.pop()?;
        }
        let ret = tok.kind == kind;
        self.tokens.push(tok);
        self.tokens.append(&mut toks);
        Ok(ret)
    }

    fn assert(&mut self, kind: TokenKind) -> Result<(), KalmarError> {
        let k = self.pop()?.kind;
        if k != kind {
            Err(KalmarError::UnexpectedToken(kind, k))
        } else {
            Ok(())
        }
    }

    fn skip_newlines(&mut self) -> Result<(), KalmarError> {
        loop {
            let t = self.pop()?;
            if t.kind != TokenKind::Newline {
                self.tokens.push(t);
                break;
            }
        }
        Ok(())
    }
}
