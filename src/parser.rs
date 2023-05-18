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
    Expr(Expr),
    Switch(Expr, Box<Stmt>),
    CaseStmt(Expr, Box<Stmt>),
    FuncCall(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Int(u32),
    Var(Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
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
            Expr::Int(_) => None,
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
            e => Err(format!("Cannot convert {:?} to a UnOp", e)),
        }
    }
}

impl UnOp {
    fn precedence(&self, ty: ExprType) -> u8 {
        match self {
            Self::Plus | Self::Minus => 50,
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
    Or,
    And,
    Default,
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
            TokenKind::KwAnd => Ok(Self::And),
            TokenKind::KwOr => Ok(Self::Or),
            TokenKind::KwDefault => Ok(Self::Default),
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
            | Self::GreaterEq => {
                assert_ne!(ty, ExprType::IfElse);
                20
            }
            Self::Plus | Self::Minus => 40,
            Self::Star | Self::Slash | Self::Percent => 50,
            Self::Eq | Self::PlusEq | Self::MinusEq | Self::StarEq | Self::SlashEq => {
                assert_eq!(ty, ExprType::Assign);
                10
            }
            Self::Default => 10,
            Self::Or | Self::And => 30,
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
            return Stmt::Script(string, Box::new(self.block(Self::statement)));
        }
        panic!()
    }

    fn block<F>(&mut self, stmt_func: F) -> Stmt
    where
        F: Fn(&mut Self) -> Stmt,
    {
        self.assert(TokenKind::LBrace);
        self.skip_newlines();
        let mut stmts = vec![];

        while !self.peek(TokenKind::RBrace) {
            stmts.push(stmt_func(self));
            self.assert(TokenKind::Newline);
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
            TokenKind::KwJump => self.jump_statement(),
            TokenKind::KwThread => self.thread_statement(),
            TokenKind::KwChildThread => self.child_thread_statement(),
            TokenKind::KwSwitch => self.switch_statement(),
            TokenKind::Var => {
                self.tokens.push(t);
                Stmt::Expr(self.expr(0, ExprType::Assign).unwrap())
            }
            TokenKind::Identifier => {
                let next = self.pop();
                match next.kind {
                    TokenKind::LParen => {
                        self.tokens.push(t);
                        self.function_call_statement()
                    }
                    _ => todo!(),
                }
            }
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
        let loop_count = self.expr(0, ExprType::Loop).unwrap();

        let block = self.block(Self::statement);

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
        Stmt::Thread(Box::new(self.block(Self::statement)))
    }

    fn child_thread_statement(&mut self) -> Stmt {
        Stmt::ChildThread(Box::new(self.block(Self::statement)))
    }

    fn switch_statement(&mut self) -> Stmt {
        let val = self.expr(0, ExprType::Loop).unwrap();
        let block = self.block(Self::case_statement);

        Stmt::Switch(val, Box::new(block))
    }

    fn case_statement(&mut self) -> Stmt {
        let case = match self.pop().kind {
            TokenKind::KwCase => self.expr(0, ExprType::Case).unwrap(),
            TokenKind::KwDefault => Expr::Default,
            _ => panic!(),
        };
        let block = self.block(Self::statement);

        Stmt::CaseStmt(case, Box::new(block))
    }

    fn function_call_statement(&mut self) -> Stmt {
        let func = self.pop().get_ident();

        let mut args = vec![];

        while self.kind() != TokenKind::RParen {
            args.push(self.expr(0, ExprType::Loop).unwrap());
            if self.kind() != TokenKind::Comma {
                break;
            }
            self.consume(TokenKind::Comma);
        }

        self.consume(TokenKind::RParen);

        Stmt::FuncCall(func, args)
    }

    //TODO: check to make sure case and/or exprs are only when type is ==
    fn expr(&mut self, min_prec: u8, expr_type: ExprType) -> Option<Expr> {
        let tok = self.pop();
        let mut left = match tok.kind {
            TokenKind::Number => match tok.val.unwrap() {
                Literal::Number(n) => Expr::Int(n.as_u32()),
                x => panic!("bad literal: {:?}", x),
            },
            TokenKind::LParen => {
                let left = self.expr(0, expr_type).unwrap();
                self.assert(TokenKind::RParen);
                left
            }
            TokenKind::Plus | TokenKind::Minus => {
                let op = UnOp::try_from(tok.kind).unwrap();
                let right = self.expr(op.precedence(expr_type), expr_type).unwrap();
                if right.get_bin_op() == Some(BinOp::EqEq) {
                    panic!("Cannot negate an equality");
                }
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
                let right = self.expr(op.precedence(expr_type), expr_type).unwrap();
                Expr::UnOp(op, Box::new(right))
            }
            TokenKind::Var if expr_type != ExprType::VarIndex => {
                self.assert(TokenKind::LBracket);
                let val = self.expr(0, ExprType::VarIndex).unwrap();
                self.assert(TokenKind::RBracket);
                Expr::Var(Box::new(val))
            }
            _ => return None,
            //x => panic!("bad token: {:?}", x),
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

                let right = if expr_type == ExprType::Assign {
                    self.expr(prec - 1, ExprType::AssignExpr).unwrap()
                } else {
                    self.expr(prec + 1, expr_type).unwrap()
                };
                left = Expr::BinOp(op, Box::new(left), Box::new(right));
                continue;
            }
            break;
        }
        Some(left)
    }

    fn pop(&mut self) -> Token {
        if self.tokens.len() > 1 {
            panic!("YOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO");
        }
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
