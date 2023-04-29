use crate::lexer::{Lexer, Literal, Token, TokenKind};

#[derive(Debug)]
pub enum Stmt {
    Script(String, Box<Stmt>),
    Block(Vec<Stmt>),
    Return,
    BreakLoop,
    BreakCase,
    Goto(String),
    Loop(Expr, Box<Stmt>),
    Jump(String),
    Thread(Box<Stmt>),
    ChildThread(Box<Stmt>),
}

#[derive(Debug)]
pub enum Expr {
    Int(u32),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ExprType {
    Loop,
    IfElse,
    Case,
    Assign,
}

impl Expr {
    fn get_bin_op(&self) -> Option<BinOp> {
        match self {
            Expr::BinOp(op, _, _) => Some(*op),
            Expr::Int(_) => None,
            _ => panic!("{:?} does not have a binary operator", self),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Plus,
    Minus,
}

impl TryFrom<TokenKind> for UnOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            e => Err(format!("Cannot convert {:?} to a UnOp", e)),
        }
    }
}

impl UnOp {
    fn precedence(&self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 4,
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
            e => Err(format!("Cannot convert {:?} to a BinOp", e)),
        }
    }
}

impl BinOp {
    fn precedence(&self, ty: ExprType) -> Option<u8> {
        let res = match self {
            Self::EqEq
            | Self::BangEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq
                if ty != ExprType::IfElse =>
            {
                panic!("Cannot have comparison in loop expr")
            }
            Self::EqEq
            | Self::BangEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => 1,
            Self::Plus | Self::Minus => 2,
            Self::Star | Self::Slash | Self::Percent => 3,
            _ => return None,
        };

        Some(res)
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
        while !self.lexer.at_end() {
            stmts.push(self.declaration());
        }

        stmts
    }

    fn declaration(&mut self) -> Stmt {
        self.assert(TokenKind::KwScr);
        let ident = self.consume(TokenKind::Identifier);
        if let Literal::Identifier(string) = ident.val.unwrap() {
            return Stmt::Script(string, Box::new(self.block()));
        }
        panic!()
    }

    fn block(&mut self) -> Stmt {
        self.assert(TokenKind::LBrace);
        self.skip_newlines();
        let mut stmts = vec![];

        while !self.peek(TokenKind::RBrace) {
            stmts.push(self.statement());
            self.assert(TokenKind::Newline);
        }

        self.assert(TokenKind::RBrace);

        Stmt::Block(stmts)
    }

    fn statement(&mut self) -> Stmt {
        match self.pop().kind {
            TokenKind::KwReturn => Stmt::Return,
            TokenKind::KwBreakLoop => Stmt::BreakLoop,
            TokenKind::KwBreakCase => Stmt::BreakCase,
            TokenKind::KwGoto => self.goto_statement(),
            TokenKind::KwLoop => self.loop_statement(),
            TokenKind::KwJump => self.jump_statement(),
            TokenKind::KwThread => self.thread_statement(),
            TokenKind::KwChildThread => self.child_thread_statement(),
            e => panic!("parsing not implemented for: {:?}", e),
        }
    }

    fn goto_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        if let Literal::Identifier(string) = ident.val.unwrap() {
            return Stmt::Goto(string);
        }
        panic!()
    }

    fn loop_statement(&mut self) -> Stmt {
        let loop_count = self.expr(0, ExprType::Loop);

        let block = self.block();

        Stmt::Loop(loop_count, Box::new(block))
    }

    fn jump_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        if let Literal::Identifier(string) = ident.val.unwrap() {
            return Stmt::Jump(string);
        }
        panic!()
    }

    fn thread_statement(&mut self) -> Stmt {
        Stmt::Thread(Box::new(self.block()))
    }

    fn child_thread_statement(&mut self) -> Stmt {
        Stmt::ChildThread(Box::new(self.block()))
    }

    fn expr(&mut self, min_prec: u8, expr_type: ExprType) -> Expr {
        let tok = self.pop();
        let mut left = match tok.kind {
            TokenKind::Number => match tok.val.unwrap() {
                Literal::Number(n) => Expr::Int(n.as_u32()),
                x => panic!("bad literal: {:?}", x),
            },
            TokenKind::LParen => {
                let left = self.expr(0, expr_type);
                self.assert(TokenKind::RParen);
                left
            }
            TokenKind::Plus | TokenKind::Minus => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(), expr_type);
                if right.get_bin_op() == Some(BinOp::EqEq) {
                    panic!("Cannot negate an equality");
                }
                Expr::UnOp(op, Box::new(right))
            }
            x => panic!("bad token: {:?}", x),
        };

        loop {
            let t = self.pop();
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

                let right = self.expr(prec + 1, expr_type);
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
