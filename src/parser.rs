use std::collections::HashMap;

use crate::lexer::{Lexer, Literal, Number, Token, TokenKind};

#[derive(Debug)]
pub enum Stmt {
    Script(Literal, Box<Stmt>),
    Block(Vec<Stmt>),
    Return,
    BreakLoop,
    BreakCase,
    Label(u32),
    Goto(u32),
    Loop(Expr, Box<Stmt>),
    IfElse(Box<Stmt>, Vec<Stmt>),
    If(Expr, Box<Stmt>),
    Else(Option<Box<Stmt>>, Option<Box<Stmt>>),
    Jump(Literal),
    Thread(Box<Stmt>),
    ChildThread(Box<Stmt>),
    Expr(Expr),
    Switch(Expr, Box<Stmt>),
    CaseStmt(Expr, Box<Stmt>),
}

#[derive(Clone, Debug)]
pub enum ExprEnum {
    Identifier(Literal),
    Array(Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    FuncCall(Box<Expr>, Vec<Expr>),
    NewArray(Box<Expr>, Box<Expr>),
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Var,
    Ident,
    FuncResult,
    NewArray,
    CaseOrAnd,
    None,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub expr: ExprEnum,
    pub ty: Type,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
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
            Self::Minus | Self::Addr | Self::Bang => 90,
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
    PercentEq,
    OrEq,
    AndEq,
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
            TokenKind::PercentEq => Ok(Self::PercentEq),
            TokenKind::OrEq => Ok(Self::OrEq),
            TokenKind::AndEq => Ok(Self::AndEq),
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
            Self::Eq
            | Self::PlusEq
            | Self::MinusEq
            | Self::StarEq
            | Self::SlashEq
            | Self::PercentEq
            | Self::OrEq
            | Self::AndEq => {
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
    labels: HashMap<String, u32>,
    verbose: bool,
}

impl Parser {
    pub fn new(data: &str) -> Self {
        Parser {
            lexer: Lexer::new(data),
            tokens: vec![],
            labels: HashMap::new(),
            verbose: false,
        }
    }

    pub fn parse(&mut self, verbose: bool) -> Vec<Stmt> {
        let mut stmts = vec![];
        self.verbose = verbose;
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
        Stmt::Script(ident.val.unwrap(), Box::new(self.block(Self::statement)))
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

        let val = match ident.val.unwrap() {
            Literal::Number(n) => format!("{}", n.as_u32()),
            Literal::Identifier(i) => i,
            _ => panic!(),
        };

        Stmt::Goto(*self.labels.get(&val).unwrap())
    }

    fn label_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        self.assert(TokenKind::Colon);

        assert!(self.labels.len() <= 16);

        let val = match ident.val.unwrap() {
            Literal::Number(n) => format!("{}", n.as_u32()),
            Literal::Identifier(i) => i,
            _ => panic!(),
        };

        assert!(!self.labels.contains_key(&val));
        self.labels.insert(val, self.labels.len() as u32);

        Stmt::Label(self.labels.len() as u32 - 1)
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
        assert_eq!(if_expr.ty, Type::Bool);
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
        Stmt::Jump(ident.val.unwrap())
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
            TokenKind::KwCase => {
                let case = self.expr(0, ExprType::Case);
                match case.expr {
                    ExprEnum::UnOp(op, _) => assert!(!matches!(op, UnOp::Minus | UnOp::Bang)),
                    ExprEnum::BinOp(op, _, _) => assert!(matches!(op, BinOp::KwAnd | BinOp::KwOr)),
                    e => panic!("{:?}", e),
                }
                case
            }
            TokenKind::KwDefault => Expr {
                expr: ExprEnum::Default,
                ty: Type::None,
            },
            _ => panic!(),
        };
        let block = self.block(Self::statement);

        Stmt::CaseStmt(case, Box::new(block))
    }

    //TODO: check to make sure case and/or exprs are only when type is ==
    fn expr(&mut self, min_prec: u8, expr_type: ExprType) -> Expr {
        let tok = self.pop();
        let mut left = match tok.kind {
            TokenKind::Number => {
                let lit = tok.val.unwrap();
                match lit {
                    Literal::Number(Number::Byte(_))
                    | Literal::Number(Number::Short(_))
                    | Literal::Number(Number::Integer(_)) => Expr {
                        expr: ExprEnum::Identifier(lit),
                        ty: Type::Int,
                    },
                    Literal::Number(Number::Float(_)) => Expr {
                        expr: ExprEnum::Identifier(lit),
                        ty: Type::Float,
                    },
                    x => panic!("bad literal: {:?}", x),
                }
            }
            TokenKind::LParen => {
                let left = self.expr(0, expr_type);
                self.assert(TokenKind::RParen);
                left
            }
            TokenKind::Plus | TokenKind::Minus => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                let t = right.ty;
                assert!(matches!(t, Type::Int | Type::Float));
                Expr {
                    expr: ExprEnum::UnOp(op, Box::new(right)),
                    ty: t,
                }
            }
            TokenKind::And => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                assert_eq!(right.ty, Type::Var);
                Expr {
                    expr: ExprEnum::UnOp(op, Box::new(right)),
                    ty: Type::Var,
                }
            }
            TokenKind::Bang => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type);
                assert_eq!(right.ty, Type::Bool);
                Expr {
                    expr: ExprEnum::UnOp(op, Box::new(right)),
                    ty: Type::Bool,
                }
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
                let right_ty = right.ty;
                assert!(matches!(right_ty, Type::Int | Type::Float | Type::Var));
                Expr {
                    expr: ExprEnum::UnOp(op, Box::new(right)),
                    ty: right_ty,
                }
            }
            TokenKind::Identifier => Expr {
                expr: ExprEnum::Identifier(tok.val.unwrap()),
                ty: Type::Ident,
            },
            TokenKind::KwNew => match self.expr(min_prec, expr_type).expr {
                ExprEnum::Array(a, b) => Expr {
                    expr: ExprEnum::NewArray(a, b),
                    ty: Type::NewArray,
                },
                _ => panic!(),
            },
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
                        Expr {
                            expr: ExprEnum::Array(Box::new(left), Box::new(right)),
                            ty: Type::Var,
                        }
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
                        Expr {
                            expr: ExprEnum::FuncCall(Box::new(left), args),
                            ty: Type::FuncResult,
                        }
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
                    BinOp::Eq
                        | BinOp::PlusEq
                        | BinOp::MinusEq
                        | BinOp::StarEq
                        | BinOp::SlashEq
                        | BinOp::PercentEq
                ) {
                    self.expr(prec - 1, ExprType::AssignExpr)
                } else if matches!(op, BinOp::KwAnd | BinOp::KwOr) {
                    self.expr(prec - 1, expr_type)
                } else {
                    self.expr(prec + 1, expr_type)
                };
                let ty = match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Slash => {
                        assert!(
                            matches!(left.ty, Type::Int | Type::Float)
                                && matches!(right.ty, Type::Int | Type::Float)
                        );
                        if left.ty == Type::Float || right.ty == Type::Float {
                            Type::Float
                        } else {
                            Type::Int
                        }
                    }
                    BinOp::Percent => {
                        assert!(left.ty == Type::Int && right.ty == Type::Int);
                        Type::Int
                    }
                    BinOp::EqEq
                    | BinOp::BangEq
                    | BinOp::Greater
                    | BinOp::GreaterEq
                    | BinOp::Less
                    | BinOp::LessEq => {
                        assert!(
                            matches!(left.ty, Type::Int | Type::Float | Type::Var)
                                && matches!(right.ty, Type::Int | Type::Float | Type::Var)
                        );
                        Type::Bool
                    }
                    BinOp::Eq => {
                        assert!(
                            left.ty == Type::Var
                                && matches!(
                                    right.ty,
                                    Type::Int | Type::Float | Type::Var | Type::NewArray
                                )
                        );
                        Type::None
                    }
                    BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::SlashEq => {
                        assert!(
                            left.ty == Type::Var
                                && matches!(right.ty, Type::Int | Type::Float | Type::Var)
                        );
                        Type::None
                    }
                    BinOp::PercentEq | BinOp::OrEq | BinOp::AndEq => {
                        assert!(left.ty == Type::Var && matches!(right.ty, Type::Int | Type::Var));
                        Type::None
                    }
                    BinOp::And => {
                        assert!(matches!(left.ty, Type::Int | Type::Var) && right.ty == Type::Int);
                        Type::Bool
                    }
                    BinOp::Comma => {
                        assert!(
                            matches!(left.ty, Type::Int | Type::Float | Type::Var | Type::Bool)
                                && matches!(
                                    right.ty,
                                    Type::Int | Type::Float | Type::Var | Type::Bool
                                )
                        );
                        Type::Var
                    }
                    BinOp::Range => {
                        assert!(left.ty == Type::Int && right.ty == Type::Int);
                        Type::None
                    }
                    BinOp::KwOr | BinOp::KwAnd => {
                        assert!(
                            matches!(left.ty, Type::Int | Type::Var)
                                && matches!(right.ty, Type::Int | Type::Var | Type::CaseOrAnd)
                        );
                        Type::CaseOrAnd
                    }
                    BinOp::KwDefault => panic!(),
                };
                left = Expr {
                    expr: ExprEnum::BinOp(op, Box::new(left), Box::new(right)),
                    ty,
                };
                continue;
            }
            break;
        }
        left
    }

    fn pop(&mut self) -> Token {
        self.tokens.pop().unwrap_or_else(|| {
            let x = self.lexer.lex();
            if self.verbose {
                println!("{:?}", x);
            }
            x
        })
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
