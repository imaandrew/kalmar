use std::{borrow::Borrow, fmt::Display, rc::Rc};

use crate::{
    error::{Error, ErrorKind},
    lexer::{Lexer, Literal, Token, TokenKind},
};

#[derive(Debug)]
pub struct ASTNode {
    node: ASTType,
    token: Option<Rc<Token>>,
}

#[derive(Debug)]
enum ASTType {
    Stmt(Stmt),
    Expr(Expr),
}

impl ASTNode {
    fn stmt(stmt: Stmt, token: Option<Rc<Token>>) -> Self {
        Self {
            node: ASTType::Stmt(stmt),
            token,
        }
    }

    fn expr(expr: Expr, token: Option<Rc<Token>>) -> Self {
        Self {
            node: ASTType::Expr(expr),
            token,
        }
    }

    pub fn get_stmt(&self) -> &Stmt {
        match &self.node {
            ASTType::Stmt(s) => s,
            _ => panic!(),
        }
    }

    pub fn get_mut_stmt(&mut self) -> &mut Stmt {
        match &mut self.node {
            ASTType::Stmt(s) => s,
            _ => panic!(),
        }
    }

    pub fn get_expr(&self) -> &Expr {
        match &self.node {
            ASTType::Expr(e) => e,
            _ => panic!(),
        }
    }

    pub fn get_mut_expr(&mut self) -> &mut Expr {
        match &mut self.node {
            ASTType::Expr(e) => e,
            _ => panic!(),
        }
    }

    pub fn get_token(&self) -> &Option<Rc<Token>> {
        &self.token
    }
}

impl Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.node {
            ASTType::Stmt(s) => write!(f, "{}", s),
            ASTType::Expr(e) => write!(f, "{}", e),
        }
    }
}

impl Default for ASTNode {
    fn default() -> Self {
        Self {
            node: ASTType::Stmt(Stmt::Empty),
            token: None,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Script(Rc<Literal>, Box<ASTNode>),
    Block(Vec<ASTNode>),
    Return,
    BreakLoop,
    BreakCase,
    Label(Rc<Literal>),
    Goto(Rc<Literal>),
    Loop(Option<Box<ASTNode>>, Box<ASTNode>),
    IfElse(Box<ASTNode>, Vec<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>),
    Else(Option<Box<ASTNode>>, Option<Box<ASTNode>>),
    Jump(Rc<Literal>),
    Thread(Box<ASTNode>),
    ChildThread(Box<ASTNode>),
    Expr(Box<ASTNode>),
    Switch(Box<ASTNode>, Box<ASTNode>),
    Case(Box<ASTNode>, Box<ASTNode>),
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
                    recursive_fmt(f, stmts.get_stmt(), indent_level)
                }
                Stmt::Block(stmts) => {
                    for s in stmts {
                        write!(f, "{}", indent)?;
                        recursive_fmt(f, s.get_stmt(), indent_level + 1)?;
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
                    recursive_fmt(f, s.get_stmt(), indent_level)
                }
                Stmt::IfElse(i, e) => {
                    recursive_fmt(f, i.get_stmt(), indent_level)?;
                    for s in e {
                        let indent = " ".repeat(indent_level * 2);
                        write!(f, "{}", indent)?;
                        recursive_fmt(f, s.get_stmt(), indent_level + 1)?;
                    }

                    Ok(())
                }
                Stmt::If(e, s) => {
                    writeln!(f, "If {}", e)?;
                    recursive_fmt(f, s.get_stmt(), indent_level)
                }
                Stmt::Else(i, e) => {
                    if let Some(i) = i {
                        write!(f, "Else ")?;
                        recursive_fmt(f, i.get_stmt(), indent_level - 1)
                    } else if let Some(e) = e {
                        writeln!(f, "Else ")?;
                        recursive_fmt(f, e.get_stmt(), indent_level - 1)
                    } else {
                        unreachable!()
                    }
                }
                Stmt::Jump(l) => writeln!(f, "Jump {}", l),
                Stmt::Thread(s) => {
                    writeln!(f, "Thread")?;
                    recursive_fmt(f, s.get_stmt(), indent_level)
                }
                Stmt::ChildThread(s) => {
                    writeln!(f, "ChildThread")?;
                    recursive_fmt(f, s.get_stmt(), indent_level)
                }
                Stmt::Expr(e) => writeln!(f, "{}", e),
                Stmt::Switch(e, s) => {
                    writeln!(f, "Switch {}", e)?;
                    recursive_fmt(f, s.get_stmt(), indent_level)
                }
                Stmt::Case(e, s) => {
                    writeln!(f, "Case {}", e)?;
                    recursive_fmt(f, s.get_stmt(), indent_level)
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
    type Error = String;

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
            _ => Err(format!(
                "Cannot convert token of type: {:?} to unary operator",
                value
            )),
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
    type Error = String;

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
            _ => Err(format!(
                "Cannot convert token of type: {:?} to binary operator",
                value
            )),
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
    Identifier(Rc<Literal>),
    Array(Rc<Literal>, Box<ASTNode>),
    UnOp(UnOp, Box<ASTNode>),
    BinOp(BinOp, Box<ASTNode>, Box<ASTNode>),
    FuncCall(Rc<Literal>, Vec<ASTNode>),
    ArrayAssign(Rc<Literal>, Box<ASTNode>),
    Default,
}

impl Expr {
    pub fn get_literal(&self) -> Option<&Literal> {
        match &self {
            Self::Identifier(l) => Some(l.borrow()),
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
    tokens: Vec<Rc<Token>>,
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

    pub fn parse(&mut self, verbose: bool) -> Result<Vec<ASTNode>, Error> {
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

    fn declaration(&mut self) -> Result<ASTNode, Error> {
        self.assert(TokenKind::KwScr)?;
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(ASTNode::stmt(
            Stmt::Script(
                Rc::clone(ident.val.as_ref().unwrap()),
                Box::new(self.block(Self::statement, true)?),
            ),
            Some(Rc::clone(&ident)),
        ))
    }

    fn block<F>(&mut self, stmt_func: F, end_stmt_newlines: bool) -> Result<ASTNode, Error>
    where
        F: Fn(&mut Self) -> Result<ASTNode, Error>,
    {
        let t = self.consume(TokenKind::LBrace)?;
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

        Ok(ASTNode::stmt(Stmt::Block(stmts), Some(Rc::clone(&t))))
    }

    fn statement(&mut self) -> Result<ASTNode, Error> {
        let t = self.pop()?;
        let t = Rc::new(t);
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
                    self.tokens.push(Rc::clone(&t));
                    return self.label_statement();
                }
                self.tokens.push(Rc::clone(&t));
                Ok(Stmt::Expr(Box::new(self.expr(0)?)))
            }
            _ => Err(Error::new(t.loc, ErrorKind::ExpectedStmt(Rc::clone(&t)))),
        }?;

        Ok(ASTNode::stmt(s, Some(Rc::clone(&t))))
    }

    fn goto_statement(&mut self) -> Result<Stmt, Error> {
        let ident = self.consume(TokenKind::Identifier)?;

        Ok(Stmt::Goto(Rc::clone(ident.val.as_ref().unwrap())))
    }

    fn label_statement(&mut self) -> Result<ASTNode, Error> {
        let ident = self.consume(TokenKind::Identifier)?;
        self.assert(TokenKind::Colon)?;

        Ok(ASTNode::stmt(
            Stmt::Label(Rc::clone(ident.val.as_ref().unwrap())),
            Some(Rc::clone(&ident)),
        ))
    }

    fn loop_statement(&mut self) -> Result<Stmt, Error> {
        let loop_count = if self.peek(TokenKind::LBrace)? {
            None
        } else {
            Some(Box::new(self.expr(0)?))
        };

        let block = self.block(Self::statement, true)?;

        Ok(Stmt::Loop(loop_count, Box::new(block)))
    }

    fn if_else_statement(&mut self) -> Result<Stmt, Error> {
        let if_stmt = self.if_statement()?;

        let mut else_stmts = vec![];

        while self.peek_over_newlines(TokenKind::KwElse)? {
            self.skip_newlines()?;
            self.assert(TokenKind::KwElse)?;
            else_stmts.push(self.else_statement()?);
        }

        Ok(Stmt::IfElse(Box::new(if_stmt), else_stmts))
    }

    fn if_statement(&mut self) -> Result<ASTNode, Error> {
        let if_expr = self.expr(0)?;
        let if_block = self.block(Self::statement, true)?;
        Ok(ASTNode::stmt(
            Stmt::If(Box::new(if_expr), Box::new(if_block)),
            None,
        ))
    }

    fn else_statement(&mut self) -> Result<ASTNode, Error> {
        Ok(if self.peek(TokenKind::KwIf)? {
            self.assert(TokenKind::KwIf)?;
            let if_stmt = self.if_statement()?;
            ASTNode::stmt(Stmt::Else(Some(Box::new(if_stmt)), None), None)
        } else {
            let else_block = self.block(Self::statement, true)?;
            ASTNode::stmt(Stmt::Else(None, Some(Box::new(else_block))), None)
        })
    }

    fn jump_statement(&mut self) -> Result<Stmt, Error> {
        let ident = self.consume(TokenKind::Identifier)?;
        Ok(Stmt::Jump(Rc::clone(ident.val.as_ref().unwrap())))
    }

    fn thread_statement(&mut self) -> Result<Stmt, Error> {
        Ok(Stmt::Thread(Box::new(self.block(Self::statement, true)?)))
    }

    fn child_thread_statement(&mut self) -> Result<Stmt, Error> {
        Ok(Stmt::ChildThread(Box::new(
            self.block(Self::statement, true)?,
        )))
    }

    fn switch_statement(&mut self) -> Result<Stmt, Error> {
        let val = self.expr(0)?;
        let block = self.block(Self::case_statement, false)?;

        Ok(Stmt::Switch(Box::new(val), Box::new(block)))
    }

    fn case_statement(&mut self) -> Result<ASTNode, Error> {
        let case = match self.kind()? {
            TokenKind::KwDefault => ASTNode::expr(Expr::Default, Some(Rc::clone(&self.pop()?))),
            _ => self.expr(0)?,
        };
        let block = self.block(Self::statement, true)?;

        Ok(ASTNode::stmt(
            Stmt::Case(Box::new(case), Box::new(block)),
            None,
        ))
    }

    fn expr(&mut self, min_prec: u8) -> Result<ASTNode, Error> {
        let t = self.pop()?;
        let mut left = ASTNode::expr(
            match t.kind {
                TokenKind::Number | TokenKind::Boolean => {
                    Expr::Identifier(Rc::clone(t.val.as_ref().unwrap()))
                }
                TokenKind::Identifier => match self.kind()? {
                    TokenKind::LBracket => {
                        self.pop()?;
                        let idx = self.expr(0)?;
                        self.assert(TokenKind::RBracket)?;
                        Expr::Array(Rc::clone(t.val.as_ref().unwrap()), Box::new(idx))
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
                                    return Ok(ASTNode::expr(
                                        Expr::FuncCall(Rc::clone(t.val.as_ref().unwrap()), args),
                                        Some(Rc::clone(&t)),
                                    ));
                                }
                                TokenKind::Comma => {
                                    self.pop()?;
                                }
                                _ => args.push(self.expr(0)?),
                            }
                        }
                    }
                    _ => Expr::Identifier(Rc::clone(t.val.as_ref().unwrap())),
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
                    let op = UnOp::try_from(t.kind).unwrap();
                    let r = self.expr(op.precedence().1)?;
                    Expr::UnOp(op, Box::new(r))
                }
                TokenKind::LParen => {
                    let expr = self.expr(0)?;
                    self.assert(TokenKind::RParen)?;
                    match expr.node {
                        ASTType::Expr(e) => e,
                        _ => unreachable!(),
                    }
                }
                _ => return Err(Error::new(t.loc, ErrorKind::ExpectedExpr(t))),
            },
            Some(Rc::clone(&t)),
        );

        loop {
            let t = self.pop()?;
            if self.parsing_func_args && t.kind == TokenKind::Comma {
                self.tokens.push(t);
                break;
            }
            let op = match BinOp::try_from(t.kind) {
                Ok(op) => op,
                Err(_) => {
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
                    return Err(Error::new(t.loc, ErrorKind::ExpectedBinOp(t)));
                }
            };

            if op.precedence().0 < min_prec {
                self.tokens.push(t);
                break;
            }

            let right = self.expr(op.precedence().1)?;
            if op == BinOp::Assign {
                if let ASTType::Expr(Expr::Identifier(l)) = &left.node {
                    let s = match l.as_ref() {
                        Literal::Identifier(i) => i,
                        _ => unreachable!(),
                    };
                    if matches!(s.as_str(), "buffer" | "fbuffer" | "array" | "flag_array") {
                        left = ASTNode::expr(
                            Expr::ArrayAssign(Rc::clone(l), Box::new(right)),
                            Some(Rc::clone(&t)),
                        );
                        continue;
                    }
                }
            }
            left = ASTNode::expr(
                Expr::BinOp(op, Box::new(left), Box::new(right)),
                Some(Rc::clone(&t)),
            );
        }
        Ok(left)
    }

    fn pop(&mut self) -> Result<Rc<Token>, Error> {
        Ok(match self.tokens.pop() {
            Some(x) => x,
            None => {
                let x = self.lexer.lex()?;
                if self.verbose {
                    println!("{:?}", x);
                }
                x.into()
            }
        })
    }

    fn kind(&mut self) -> Result<TokenKind, Error> {
        let t = self.pop()?;
        let kind = t.kind;
        self.tokens.push(t);
        Ok(kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Rc<Token>, Error> {
        let t = self.pop()?;
        if t.kind == kind {
            return Ok(t);
        }

        Err(Error::new(
            t.loc,
            ErrorKind::UnexpectedToken(kind, Rc::clone(&t)),
        ))
    }

    fn peek(&mut self, kind: TokenKind) -> Result<bool, Error> {
        let tok = self.pop()?;
        let ret = tok.kind == kind;
        self.tokens.push(tok);
        Ok(ret)
    }

    fn peek_over_newlines(&mut self, kind: TokenKind) -> Result<bool, Error> {
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

    fn assert(&mut self, kind: TokenKind) -> Result<(), Error> {
        assert_eq!(self.pop()?.kind, kind);
        Ok(())
    }

    fn skip_newlines(&mut self) -> Result<(), Error> {
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
