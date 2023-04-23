use crate::lexer::{Lexer, Literal, Token, TokenKind};

enum Stmt {
    Script(String, Box<Stmt>),
    Block(Vec<Stmt>),
    Return,
    BreakLoop,
    BreakCase,
    Goto(String),
    Loop(u32, Box<Stmt>),
    Jump(String),
    Thread(Box<Stmt>),
    ChildThread(Box<Stmt>),
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

    pub fn parse(&mut self) {
        let mut stmts = vec![];
        while !self.lexer.at_end() {
            stmts.push(self.declaration());
        }
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
            _ => todo!(),
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
        let loop_count = match self.pop().val.unwrap() {
            Literal::Number(num) => num.as_u32(),
            _ => 0,
        };

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

    fn check(&mut self, kind: TokenKind) -> bool {
        self.lexer.lex().kind == kind
    }

    fn assert(&mut self, kind: TokenKind) {
        assert_eq!(self.lexer.lex().kind, kind)
    }

    fn skip_newlines(&mut self) {
        while self.check(TokenKind::Newline) {}
    }
}
