use crate::lexer::{Lexer, Literal, Token, TokenKind};

#[derive(Debug)]
pub enum Stmt {
    Script(String, Box<Stmt>),
    Block(Vec<Stmt>),
    Return,
    BreakLoop,
    BreakCase,
    NewArray(Expr),
    Label(String),
    Goto(String),
    Loop(Expr, Box<Stmt>),
    IfElse(Box<Stmt>, Vec<Stmt>),
    If(Expr, Box<Stmt>),
    Else(Option<Box<Stmt>>, Option<Box<Stmt>>),
    Jump(String),
    Thread(Box<Stmt>),
    ChildThread(Box<Stmt>),
    Expr(Expr),
    Switch(Expr, Box<Stmt>),
    CaseStmt(Expr, Box<Stmt>),
}

#[derive(Debug)]
pub enum Expr {
    Identifier(Literal),
    Array(Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    Default,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExprType {
    Loop,
    IfElse,
    Case,
    Assign,
    AssignExpr,
    VarIndex,
}

impl Expr {
    fn get_bin_op(&self) -> Option<BinOp> {
        match self {
            Expr::BinOp(op, _, _) => Some(*op),
            Expr::Identifier(_) => None,
            _ => panic!("{:?} does not have a binary operator", self),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Plus,
    Minus,
    EqEq,
    BangEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Addr,
    Bang,
}

impl TryFrom<TokenKind> for UnOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::EqEq => Ok(Self::EqEq),
            TokenKind::BangEq => Ok(Self::BangEq),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            TokenKind::And => Ok(Self::Addr),
            TokenKind::Bang => Ok(Self::Bang),
            e => Err(format!("Cannot convert {:?} to a UnOp", e)),
        }
    }
}

impl UnOp {
    fn precedence(&self, ty: ExprType) -> u8 {
        match self {
            Self::Plus | Self::Minus | Self::Addr | Self::Bang => 90,
            Self::EqEq
            | Self::BangEq
            | Self::Greater
            | Self::GreaterEq
            | Self::Less
            | Self::LessEq
                if ty == ExprType::Case =>
            {
                20
            }
            _ => panic!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    EqEq,
    BangEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Percent,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    And,
    KwOr,
    KwAnd,
    KwDefault,
    Comma,
    Range,
}

impl TryFrom<TokenKind> for BinOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Star),
            TokenKind::Slash => Ok(Self::Slash),
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::EqEq => Ok(Self::EqEq),
            TokenKind::BangEq => Ok(Self::BangEq),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            TokenKind::Percent => Ok(Self::Percent),
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::PlusEq => Ok(Self::PlusEq),
            TokenKind::MinusEq => Ok(Self::MinusEq),
            TokenKind::StarEq => Ok(Self::StarEq),
            TokenKind::SlashEq => Ok(Self::SlashEq),
            TokenKind::KwAnd => Ok(Self::KwAnd),
            TokenKind::KwOr => Ok(Self::KwOr),
            TokenKind::KwDefault => Ok(Self::KwDefault),
            TokenKind::And => Ok(Self::And),
            TokenKind::Comma => Ok(Self::Comma),
            TokenKind::Range => Ok(Self::Range),
            e => Err(format!("Cannot convert {:?} to a BinOp", e)),
        }
    }
}

impl BinOp {
    fn precedence(&self, ty: ExprType) -> Option<u8> {
        let res = match self {
            Self::KwOr | Self::KwAnd | Self::Comma => 10,
            Self::Eq | Self::PlusEq | Self::MinusEq | Self::StarEq | Self::SlashEq => {
                assert_eq!(ty, ExprType::Assign);
                20
            }
            Self::And => 30,
            Self::EqEq | Self::BangEq => 40,
            Self::Less | Self::LessEq | Self::Greater | Self::GreaterEq => 50,
            Self::Range => 60,
            Self::Plus | Self::Minus => 70,
            Self::Star | Self::Slash | Self::Percent => 80,
            _ => return None,
        };

        Some(res)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PostOp {
    LParen,
    LBracket,
}

impl TryFrom<TokenKind> for PostOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::LParen => Ok(Self::LParen),
            TokenKind::LBracket => Ok(Self::LBracket),
            e => Err(format!("Cannot convert {:?} to a PostOp", e)),
        }
    }
}

impl PostOp {
    fn precedence(&self) -> u8 {
        match self {
            Self::LBracket | Self::LParen => 100,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(data: &str) -> Self {
        Parser {
            lexer: Lexer::new(data),
            tokens: vec![],
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        self.skip_newlines();
        while !self.lexer.at_end() {
            stmts.push(self.declaration());
            if !self.lexer.at_end() {
                self.assert(TokenKind::Newline);
            }
            self.skip_newlines();
        }

        stmts
    }

    fn declaration(&mut self) -> Stmt {
        self.assert(TokenKind::KwScr);
        let ident = self.consume(TokenKind::Identifier);
        Stmt::Script(ident.get_ident(), Box::new(self.block(Self::statement)))
    }

    fn block<F>(&mut self, stmt_func: F) -> Stmt
    where
        F: Fn(&mut Self) -> Stmt,
    {
        self.assert(TokenKind::LBrace);
        self.assert(TokenKind::Newline);
        self.skip_newlines();
        let mut stmts = vec![];

        while !self.peek(TokenKind::RBrace) {
            stmts.push(stmt_func(self));
            self.assert(TokenKind::Newline);
            self.skip_newlines();
        }

        self.assert(TokenKind::RBrace);

        Stmt::Block(stmts)
    }

    fn statement(&mut self) -> Stmt {
        let t = self.pop();
        match t.kind {
            TokenKind::KwReturn => Stmt::Return,
            TokenKind::KwBreakLoop => Stmt::BreakLoop,
            TokenKind::KwBreakCase => Stmt::BreakCase,
            TokenKind::KwGoto => self.goto_statement(),
            TokenKind::KwLoop => self.loop_statement(),
            TokenKind::KwIf => self.if_else_statement(),
            TokenKind::KwJump => self.jump_statement(),
            TokenKind::KwThread => self.thread_statement(),
            TokenKind::KwChildThread => self.child_thread_statement(),
            TokenKind::KwSwitch => self.switch_statement(),
            TokenKind::KwNew => self.array_init_statement(),
            TokenKind::Identifier => {
                if self.peek(TokenKind::Colon) {
                    self.tokens.push(t);
                    return self.label_statement();
                }
                self.tokens.push(t);
                Stmt::Expr(self.expr(0, ExprType::Assign))
            }
            e => panic!("parsing not implemented for: {:?}", e),
        }
    }

    fn goto_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        Stmt::Goto(ident.get_ident())
    }

    fn label_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        self.assert(TokenKind::Colon);
        Stmt::Label(ident.get_ident())
    }

    fn loop_statement(&mut self) -> Stmt {
        let loop_count = self.expr(0, ExprType::Loop);

        let block = self.block(Self::statement);

        Stmt::Loop(loop_count, Box::new(block))
    }

    fn if_else_statement(&mut self) -> Stmt {
        let if_stmt = self.if_statement();

        let mut else_stmts = vec![];

        while self.peek_over_newlines(TokenKind::KwElse) {
            self.skip_newlines();
            let tok = self.consume(TokenKind::KwElse);
            self.tokens.push(tok);
            else_stmts.push(self.else_statement());
        }

        Stmt::IfElse(Box::new(if_stmt), else_stmts)
    }

    fn if_statement(&mut self) -> Stmt {
        let if_expr = self.expr(0, ExprType::IfElse);
        let if_block = self.block(Self::statement);
        Stmt::If(if_expr, Box::new(if_block))
    }

    fn else_statement(&mut self) -> Stmt {
        self.assert(TokenKind::KwElse);
        if self.peek(TokenKind::KwIf) {
            self.assert(TokenKind::KwIf);
            let if_stmt = self.if_statement();
            Stmt::Else(Some(Box::new(if_stmt)), None)
        } else {
            let else_block = self.block(Self::statement);
            Stmt::Else(None, Some(Box::new(else_block)))
        }
    }

    fn jump_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        Stmt::Jump(ident.get_ident())
    }

    fn thread_statement(&mut self) -> Stmt {
        Stmt::Thread(Box::new(self.block(Self::statement)))
    }

    fn child_thread_statement(&mut self) -> Stmt {
        Stmt::ChildThread(Box::new(self.block(Self::statement)))
    }

    fn switch_statement(&mut self) -> Stmt {
        let val = self.expr(0, ExprType::Loop);
        let block = self.block(Self::case_statement);

        Stmt::Switch(val, Box::new(block))
    }

    fn case_statement(&mut self) -> Stmt {
        let case = match self.pop().kind {
            TokenKind::KwCase => self.expr(0, ExprType::Case),
            TokenKind::KwDefault => Expr::Default,
            _ => panic!(),
        };
        let block = self.block(Self::statement);

        Stmt::CaseStmt(case, Box::new(block))
    }

    fn array_init_statement(&mut self) -> Stmt {
        Stmt::NewArray(self.expr(0, ExprType::Assign))
    }

    //TODO: check to make sure case and/or exprs are only when type is ==
    fn expr(&mut self, min_prec: u8, expr_type: ExprType) -> Expr {
        let tok = self.pop();
        let mut left = match tok.kind {
            TokenKind::Number => match tok.val.unwrap() {
                Literal::Number(n) => Expr::Identifier(Literal::Number(n)),
                x => panic!("bad literal: {:?}", x),
            },
            TokenKind::LParen => {
                let left = self.expr(0, expr_type);
                self.assert(TokenKind::RParen);
                left
            }
            TokenKind::Plus | TokenKind::Minus => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                if right.get_bin_op() == Some(BinOp::EqEq) {
                    panic!("Cannot negate an equality");
                }
                Expr::UnOp(op, Box::new(right))
            }
            TokenKind::And => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                match right {
                    Expr::Array(_, _) => Expr::UnOp(op, Box::new(right)),
                    _ => panic!(),
                }
            }
            TokenKind::Bang => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                Expr::UnOp(op, Box::new(right))
            }
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Greater
            | TokenKind::GreaterEq
            | TokenKind::Less
            | TokenKind::LessEq
                if expr_type == ExprType::Case =>
            {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                Expr::UnOp(op, Box::new(right))
            }
            TokenKind::Identifier => Expr::Identifier(tok.val.unwrap()),
            _ => panic!(),
        };

        loop {
            let t = self.pop();

            if let Ok(op) = PostOp::try_from(t.kind) {
                if op.precedence() < min_prec {
                    self.tokens.push(t);
                    break;
                }

                left = match op {
                    PostOp::LBracket => {
                        let right = self.expr(0, ExprType::VarIndex);
                        self.assert(TokenKind::RBracket);
                        Expr::Array(Box::new(left), Box::new(right))
                    }
                    PostOp::LParen => {
                        let mut args = vec![];
                        while self.kind() != TokenKind::RParen {
                            args.push(self.expr(0, ExprType::Loop));
                            if self.kind() != TokenKind::Comma {
                                break;
                            }
                            self.assert(TokenKind::Comma);
                        }
                        self.assert(TokenKind::RParen);
                        Expr::FuncCall(Box::new(left), args)
                    }
                };
                continue;
            }

            let op = match BinOp::try_from(t.kind) {
                Ok(op) => op,
                Err(_) => {
                    self.tokens.push(t);
                    break;
                }
            };

            if let Some(prec) = op.precedence(expr_type) {
                if prec < min_prec {
                    self.tokens.push(t);
                    break;
                }

                let right = if matches!(
                    op,
                    BinOp::Eq | BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::SlashEq
                ) {
                    self.expr(prec - 1, ExprType::AssignExpr)
                } else {
                    self.expr(prec + 1, expr_type)
                };
                left = Expr::BinOp(op, Box::new(left), Box::new(right));
                continue;
            }
            break;
        }
        left
    }

    fn pop(&mut self) -> Token {
        self.tokens.pop().unwrap_or_else(|| self.lexer.lex())
    }

    fn kind(&mut self) -> TokenKind {
        let t = self.pop();
        let kind = t.kind;
        self.tokens.push(t);
        kind
    }

    fn consume(&mut self, kind: TokenKind) -> Token {
        if self.peek(kind) {
            return self.pop();
        }

        panic!()
    }

    fn peek(&mut self, kind: TokenKind) -> bool {
        let tok = self.pop();
        let ret = tok.kind == kind;
        self.tokens.push(tok);
        ret
    }

    fn peek_over_newlines(&mut self, kind: TokenKind) -> bool {
        let mut tok = self.pop();
        let mut toks = vec![];
        while tok.kind == TokenKind::Newline {
            toks.push(tok);
            tok = self.pop();
        }
        let ret = tok.kind == kind;
        self.tokens.push(tok);
        self.tokens.append(&mut toks);
        ret
    }

    fn assert(&mut self, kind: TokenKind) {
        assert_eq!(self.pop().kind, kind)
    }

    fn skip_newlines(&mut self) {
        loop {
            let t = self.pop();
            if t.kind != TokenKind::Newline {
                self.tokens.push(t);
                break;
            }
        }
    }
}
