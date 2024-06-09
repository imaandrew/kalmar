use std::{
    ops::{Add, BitAnd, Div, Mul, Neg, Rem, Sub},
    str::FromStr,
};

use crate::{error::KalmarError, StringManager, SymbolIndex};

use std::string::ToString;
use strum_macros::Display;
use strum_macros::EnumString;

#[derive(Debug, Default, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

impl Span {
    pub fn new(line: usize, col: usize, len: usize) -> Self {
        Self { line, col, len }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub val: Option<Literal>,
    pub span: Span,
}

#[derive(Copy, Clone, Display, Debug, EnumString, PartialEq, Eq)]
pub enum TokenKind {
    #[strum(serialize = "scr")]
    KwScr,
    #[strum(serialize = "return")]
    KwReturn,
    #[strum(serialize = "goto")]
    KwGoto,
    #[strum(serialize = "loop")]
    KwLoop,
    #[strum(serialize = "bloop")]
    KwBreakLoop,
    #[strum(serialize = "bcase")]
    KwBreakCase,
    #[strum(serialize = "switch")]
    KwSwitch,
    #[strum(serialize = "thread")]
    KwThread,
    #[strum(serialize = "cthread")]
    KwChildThread,
    #[strum(serialize = "if")]
    KwIf,
    #[strum(serialize = "else")]
    KwElse,
    #[strum(serialize = "default")]
    KwDefault,
    #[strum(serialize = "jump")]
    KwJump,
    #[strum(serialize = "==")]
    EqEq,
    #[strum(serialize = "!=")]
    BangEq,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = ">=")]
    GreaterEq,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = "<=")]
    LessEq,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Star,
    #[strum(serialize = "/")]
    Slash,
    #[strum(serialize = "%")]
    Percent,
    #[strum(serialize = "|")]
    Pipe,
    #[strum(serialize = "&")]
    And,
    #[strum(serialize = "+=")]
    PlusEq,
    #[strum(serialize = "-=")]
    MinusEq,
    #[strum(serialize = "*=")]
    StarEq,
    #[strum(serialize = "/=")]
    SlashEq,
    #[strum(serialize = "%=")]
    PercentEq,
    #[strum(serialize = "|=")]
    OrEq,
    #[strum(serialize = "&=")]
    AndEq,
    #[strum(serialize = "!")]
    Bang,
    #[strum(serialize = "(")]
    LParen,
    #[strum(serialize = ")")]
    RParen,
    #[strum(serialize = "{")]
    LBrace,
    #[strum(serialize = "}")]
    RBrace,
    #[strum(serialize = "[")]
    LBracket,
    #[strum(serialize = "]")]
    RBracket,
    #[strum(serialize = "=")]
    Eq,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = "..")]
    Range,
    #[strum(serialize = "<-")]
    Arrow,
    #[strum(disabled)]
    Number,
    Newline,
    #[strum(disabled)]
    Identifier,
    #[strum(disabled)]
    Boolean,
    #[strum(disabled)]
    Whitespace,
    #[strum(disabled)]
    None,
    Eof,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Display)]
pub enum Number {
    Int(u32),
    Float(f32),
}

impl Number {
    pub fn as_u32(&self) -> u32 {
        match *self {
            Number::Int(x) => x,
            Number::Float(x) => (x * 1024.0 - 230000000.0) as i32 as u32,
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(*self, Number::Float(_))
    }
}

macro_rules! number_ops {
    ($($op_trait:ident $op_fn:ident $op:tt),*) => {
        $(
            impl $op_trait for Number {
                type Output = Number;

                fn $op_fn(self, rhs: Self) -> Self::Output {
                    match (&self, &rhs) {
                        (Number::Float(x), Number::Float(y)) => Number::Float(x $op y),
                        (Number::Float(x), y) | (y, Number::Float(x)) => Number::Float(x $op y.as_u32() as f32),
                        _ => {
                            let result = self.as_u32() $op rhs.as_u32();
                            Number::Int(result)
                        }
                    }
                }
            }
        )*
    };
}

number_ops! { Add add +, Sub sub -, Mul mul *, Div div / }

macro_rules! int_ops {
    ($($op_trait:ident $op_fn:ident $op:tt),*) => {
        $(
            impl $op_trait for Number {
                type Output = Number;

                fn $op_fn(self, rhs: Self) -> Self::Output {
                    match (&self, &rhs) {
                        (Number::Float(_), _) | (_, Number::Float(_)) => unreachable!(),
                        _ => {
                            let result = self.as_u32() $op rhs.as_u32();
                            Number::Int(result)
                        }
                    }
                }
            }
        )*
    };
}

int_ops! { Rem rem %, BitAnd bitand & }

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match &self {
            Number::Int(x) => Number::Int(x.wrapping_neg()),
            Number::Float(x) => Number::Float(-x),
        }
    }
}

#[derive(Copy, Clone, Debug, Display)]
pub enum Literal {
    Identifier(SymbolIndex),
    Number(Number),
    Boolean(bool),
}

pub struct Lexer<'lexr, 'smgr> {
    data: Vec<char>,
    literals: &'lexr mut StringManager<'smgr>,
    curr: usize,
    line: usize,
    col: isize,
}

impl<'lexr, 'smgr> Lexer<'lexr, 'smgr> {
    pub fn new(literals: &'lexr mut StringManager<'smgr>) -> Self {
        Lexer {
            data: literals.text.chars().collect(),
            literals,
            curr: 0,
            line: 0,
            col: -1,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Vec<KalmarError>> {
        let mut tokens = vec![];
        let mut err = vec![];
        while !self.at_end() {
            match self.lex_token() {
                Ok(t) if t.kind == TokenKind::Whitespace => (),
                Ok(t) => tokens.push(t),
                Err(e) => err.push(e),
            }
        }
        tokens.push(self.create_token(TokenKind::Eof));

        if err.is_empty() {
            Ok(tokens)
        } else {
            Err(err)
        }
    }

    fn lex_token(&mut self) -> Result<Token, KalmarError> {
        let c = self.next();
        match c {
            '(' => Ok(self.create_token(TokenKind::LParen)),
            ')' => Ok(self.create_token(TokenKind::RParen)),
            '{' => Ok(self.create_token(TokenKind::LBrace)),
            '}' => Ok(self.create_token(TokenKind::RBrace)),
            '[' => Ok(self.create_token(TokenKind::LBracket)),
            ']' => Ok(self.create_token(TokenKind::RBracket)),
            ':' => Ok(self.create_token(TokenKind::Colon)),
            ',' => Ok(self.create_token(TokenKind::Comma)),
            '/' if matches!(self.peek(), '/' | '*') => {
                if self.peek() == '/' {
                    while self.peek() != '\n' && self.peek() != '\0' {
                        self.next();
                    }
                    Ok(self.create_token(TokenKind::Whitespace))
                } else {
                    self.next();
                    while self.peek() != '*' || self.peek_over() != '/' {
                        self.next();
                    }
                    self.next();
                    self.next();
                    Ok(self.create_token(TokenKind::Whitespace))
                }
            }
            '=' | '!' | '>' | '<' | '+' | '-' | '*' | '/' | '%' | '|' | '&' | '.' => {
                let x = format!("{}{}", c, self.peek());
                if let Ok(kind) = TokenKind::from_str(&x) {
                    self.next();
                    Ok(self.create_token(kind))
                } else {
                    Ok(self.create_token(TokenKind::from_str(&c.to_string()).unwrap()))
                }
            }
            ' ' | '\r' | '\t' => Ok(self.create_token(TokenKind::Whitespace)),
            '\n' => {
                let t = Ok(self.create_token(TokenKind::Newline));
                self.line += 1;
                self.col = -1;
                t
            }
            _ if c.is_ascii_digit() => self.number(c),
            _ if c.is_alphabetic() || c == '_' => Ok(self.identifier()?),
            _ => {
                let idx = self
                    .literals
                    .add(&self.literals.text[self.curr - 1..self.curr]);
                let t = self.create_token_literal(
                    TokenKind::None,
                    self.curr - 1,
                    Some(Literal::Identifier(idx)),
                );
                Err(KalmarError::UnexpectedChar(t))
            }
        }
    }

    fn number(&mut self, c: char) -> Result<Token, KalmarError> {
        let mut base = 10;
        let mut start = self.curr - 1;
        if c == '0' {
            match self.peek() {
                'b' => {
                    base = 2;
                    self.next();
                    start += 1;
                    if !self.consume_decimal_digits() {
                        let idx = self
                            .literals
                            .add(&self.literals.text[self.curr - 2..self.curr]);
                        let t = Token {
                            kind: TokenKind::None,
                            val: Some(Literal::Identifier(idx)),
                            span: Span::new(self.line, self.col as usize - 1, 2),
                        };
                        return Err(KalmarError::BaseMissingNumber(t));
                    }
                }
                'o' => {
                    base = 8;
                    self.next();
                    start += 1;
                    if !self.consume_decimal_digits() {
                        let idx = self
                            .literals
                            .add(&self.literals.text[self.curr - 2..self.curr]);
                        let t = Token {
                            kind: TokenKind::None,
                            val: Some(Literal::Identifier(idx)),
                            span: Span::new(self.line, self.col as usize - 1, 2),
                        };
                        return Err(KalmarError::BaseMissingNumber(t));
                    }
                }
                'x' => {
                    base = 16;
                    self.next();
                    start += 1;
                    if !self.consume_hex_digits() {
                        let idx = self
                            .literals
                            .add(&self.literals.text[self.curr - 2..self.curr]);
                        let t = Token {
                            kind: TokenKind::None,
                            val: Some(Literal::Identifier(idx)),
                            span: Span::new(self.line, self.col as usize - 1, 2),
                        };
                        return Err(KalmarError::BaseMissingNumber(t));
                    }
                }
                '0'..='9' => {
                    self.consume_decimal_digits();
                }
                '.' => (),
                _ => {
                    return Ok(self.create_token_literal(
                        TokenKind::Number,
                        start,
                        Some(Literal::Number(Number::Int(0))),
                    ))
                }
            }
        } else {
            self.consume_decimal_digits();
        }

        if self.peek() == '.' && self.peek_over() != '.' {
            self.next();
            self.consume_decimal_digits();
            return Ok(self.create_token_literal(
                TokenKind::Number,
                start,
                Some(Literal::Number(Number::Float(
                    self.literals.text[start..self.curr].parse().unwrap(),
                ))),
            ));
        }

        Ok(self.create_token_literal(
            TokenKind::Number,
            start,
            Some(Literal::Number(Number::Int(
                u32::from_str_radix(&self.literals.text[start..self.curr], base).unwrap(),
            ))),
        ))
    }

    fn consume_decimal_digits(&mut self) -> bool {
        let mut non_empty = false;
        while self.peek().is_ascii_digit() {
            self.next();
            non_empty = true;
        }
        non_empty
    }

    fn consume_hex_digits(&mut self) -> bool {
        let mut non_empty = false;
        while self.peek().is_ascii_hexdigit() {
            self.next();
            non_empty = true;
        }
        non_empty
    }

    fn identifier(&mut self) -> Result<Token, KalmarError> {
        let start = self.curr - 1;

        while self.peek().is_alphabetic() || self.peek() == '_' {
            self.next();
        }

        let text = &self.literals.text[start..self.curr];

        if let Ok(kind) = TokenKind::from_str(text) {
            return Ok(self.create_token_literal(kind, start, None));
        };

        Ok(if text == "true" {
            self.create_token_literal(TokenKind::Boolean, start, Some(Literal::Boolean(true)))
        } else if text == "false" {
            self.create_token_literal(TokenKind::Boolean, start, Some(Literal::Boolean(false)))
        } else {
            let idx = self.literals.add(&self.literals.text[start..self.curr]);
            self.create_token_literal(TokenKind::Identifier, start, Some(Literal::Identifier(idx)))
        })
    }

    fn next(&mut self) -> char {
        self.curr += 1;
        self.col += 1;
        *self.data.get(self.curr - 1).unwrap()
    }

    fn peek(&self) -> char {
        *self.data.get(self.curr).unwrap()
    }

    fn peek_over(&self) -> char {
        *self.data.get(self.curr + 1).unwrap()
    }

    pub fn at_end(&self) -> bool {
        self.curr >= self.data.len()
    }

    fn create_token(&self, kind: TokenKind) -> Token {
        self.create_token_literal(kind, self.curr - 1, None)
    }

    fn create_token_literal(
        &self,
        kind: TokenKind,
        start: usize,
        literal: Option<Literal>,
    ) -> Token {
        Token {
            kind,
            val: literal,
            span: Span::new(
                self.line,
                self.col as usize + start + 1 - self.curr,
                self.curr - start,
            ),
        }
    }
}
