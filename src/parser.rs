use crate::lexer::{Lexer, Literal, Token, TokenKind};

#[derive(Debug)]
pub enum Stmt {
    Script(Literal, Box<Stmt>),
    Block(Vec<Stmt>),
    Return,
    BreakLoop,
    BreakCase,
    Label(Literal),
    Goto(Literal),
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

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Minus,
    Bang,
    Ampersand,
    New,
    Equal,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
}

impl TryFrom<TokenKind> for UnOp {
    type Error = String;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::And => Ok(Self::Ampersand),
            TokenKind::KwNew => Ok(Self::New),
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
            Self::Bang | Self::Minus | Self::Ampersand | Self::New => (10, 9),
            Self::Equal
            | Self::NotEqual
            | Self::Greater
            | Self::GreaterEq
            | Self::Less
            | Self::LessEq => (20, 19),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Div,
    Mod,
    BitOr,
    BitAnd,
    PlusEq,
    MinusEq,
    StarEq,
    DivEq,
    ModEq,
    OrEq,
    AndEq,
    Equal,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Range,
    Assign,
    Or,
    And,
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
            Self::Range | Self::Or | Self::And => (20, 21),
            Self::Star | Self::Div | Self::Mod => (30, 31),
            Self::Plus | Self::Minus => (40, 41),
            Self::Greater | Self::GreaterEq | Self::Less | Self::LessEq => (50, 51),
            Self::Equal | Self::NotEqual => (60, 61),
            Self::BitAnd => (70, 71),
            Self::BitOr => (80, 81),
            Self::Assign
            | Self::PlusEq
            | Self::MinusEq
            | Self::StarEq
            | Self::DivEq
            | Self::ModEq
            | Self::OrEq
            | Self::AndEq => (90, 89),
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
    Default,
}

pub struct Parser {
    lexer: Lexer,
    tokens: Vec<Token>,
    verbose: bool,
}

impl Parser {
    pub fn new(data: &str) -> Self {
        Parser {
            lexer: Lexer::new(data),
            tokens: vec![],
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
        Stmt::Script(
            ident.val.unwrap(),
            Box::new(self.block(Self::statement, true)),
        )
    }

    fn block<F>(&mut self, stmt_func: F, end_stmt_newlines: bool) -> Stmt
    where
        F: Fn(&mut Self) -> Stmt,
    {
        self.assert(TokenKind::LBrace);
        self.assert(TokenKind::Newline);
        self.skip_newlines();
        let mut stmts = vec![];

        while !self.peek(TokenKind::RBrace) {
            stmts.push(stmt_func(self));
            if end_stmt_newlines {
                self.assert(TokenKind::Newline);
            }
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
                Stmt::Expr(self.expr(0))
            }
            e => panic!("parsing not implemented for: {:?}", e),
        }
    }

    fn goto_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);

        Stmt::Goto(ident.val.unwrap())
    }

    fn label_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        self.assert(TokenKind::Colon);

        Stmt::Label(ident.val.unwrap())
    }

    fn loop_statement(&mut self) -> Stmt {
        let loop_count = self.expr(0);

        let block = self.block(Self::statement, true);

        Stmt::Loop(loop_count, Box::new(block))
    }

    fn if_else_statement(&mut self) -> Stmt {
        let if_stmt = self.if_statement();

        let mut else_stmts = vec![];

        while self.peek_over_newlines(TokenKind::KwElse) {
            self.skip_newlines();
            self.assert(TokenKind::KwElse);
            else_stmts.push(self.else_statement());
        }

        Stmt::IfElse(Box::new(if_stmt), else_stmts)
    }

    fn if_statement(&mut self) -> Stmt {
        let if_expr = self.expr(0);
        let if_block = self.block(Self::statement, true);
        Stmt::If(if_expr, Box::new(if_block))
    }

    fn else_statement(&mut self) -> Stmt {
        if self.peek(TokenKind::KwIf) {
            self.assert(TokenKind::KwIf);
            let if_stmt = self.if_statement();
            Stmt::Else(Some(Box::new(if_stmt)), None)
        } else {
            let else_block = self.block(Self::statement, true);
            Stmt::Else(None, Some(Box::new(else_block)))
        }
    }

    fn jump_statement(&mut self) -> Stmt {
        let ident = self.consume(TokenKind::Identifier);
        Stmt::Jump(ident.val.unwrap())
    }

    fn thread_statement(&mut self) -> Stmt {
        Stmt::Thread(Box::new(self.block(Self::statement, true)))
    }

    fn child_thread_statement(&mut self) -> Stmt {
        Stmt::ChildThread(Box::new(self.block(Self::statement, true)))
    }

    fn switch_statement(&mut self) -> Stmt {
        let val = self.expr(0);
        let block = self.block(Self::case_statement, false);

        Stmt::Switch(val, Box::new(block))
    }

    fn case_statement(&mut self) -> Stmt {
        let case = match self.pop().kind {
            TokenKind::KwCase => self.expr(0),
            TokenKind::KwDefault => Expr::Default,
            _ => panic!(),
        };
        let block = self.block(Self::statement, true);

        Stmt::CaseStmt(case, Box::new(block))
    }

    fn expr(&mut self, min_prec: u8) -> Expr {
        let t = self.pop();
        let mut left = match t.kind {
            TokenKind::Number => Expr::Identifier(t.val.unwrap()),
            TokenKind::Identifier => match self.kind() {
                TokenKind::LBracket => {
                    self.pop();
                    let idx = self.expr(0);
                    self.assert(TokenKind::RBracket);
                    Expr::Array(t.val.unwrap(), Box::new(idx))
                }
                TokenKind::LParen => {
                    self.pop();
                    let mut args = vec![];
                    loop {
                        match self.kind() {
                            TokenKind::RParen => {
                                self.pop();
                                return Expr::FuncCall(t.val.unwrap(), args);
                            }
                            TokenKind::Comma => {
                                self.pop();
                            }
                            _ => args.push(self.expr(0)),
                        }
                    }
                }
                _ => panic!(),
            },
            TokenKind::Minus
            | TokenKind::Bang
            | TokenKind::Eq
            | TokenKind::BangEq
            | TokenKind::Greater
            | TokenKind::GreaterEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::And => {
                let op = UnOp::try_from(t.kind).unwrap();
                let r = self.expr(op.precedence().1);
                Expr::UnOp(op, Box::new(r))
            }
            TokenKind::LParen => {
                let expr = self.expr(0);
                self.assert(TokenKind::RParen);
                expr
            }
            _ => panic!(),
        };

        loop {
            let t = self.pop();
            let op = BinOp::try_from(t.kind).unwrap();

            if op.precedence().0 < min_prec {
                break;
            }

            let right = self.expr(op.precedence().1);
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
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
