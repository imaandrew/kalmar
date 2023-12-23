use std::{
    fmt::Display,
    ops::{Add, BitAnd, Div, Mul, Neg, Rem, Sub},
    rc::Rc,
    str::FromStr,
};

use crate::error::{Error, ErrorKind};

use std::string::ToString;
use strum_macros::Display;
use strum_macros::EnumString;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub val: Option<Rc<Literal>>,
    pub loc: (usize, usize),
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
    Eof,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Number {
    Int(u32),
    Float(f32),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(x) => write!(f, "{}", x),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
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

#[derive(Clone, Debug)]
pub enum Literal {
    Identifier(String),
    Number(Number),
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Identifier(s) => write!(f, "{}", s),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

pub struct Lexer {
    data: Vec<char>,
    col: usize,
    line: usize,
    start: usize,
    curr: usize,
    line_start: usize,
}

impl Lexer {
    pub fn new(data: &str) -> Self {
        Lexer {
            data: data.chars().collect(),
            col: 1,
            line: 1,
            start: 0,
            curr: 0,
            line_start: 0,
        }
    }

    pub fn lex(&mut self) -> Result<Token, Error> {
        if !self.at_end() {
            self.col += self.curr - self.start;
            self.start = self.curr;
            if let Some(t) = self.lex_token()? {
                return Ok(t);
            }
            return self.lex();
        }
        Ok(self.create_token(TokenKind::Eof))
    }

    fn lex_token(&mut self) -> Result<Option<Token>, Error> {
        let c = self.next();
        match c {
            '(' | ')' | '{' | '}' | '[' | ']' | ':' | ',' => Ok(Some(
                self.create_token(TokenKind::from_str(&c.to_string()).unwrap()),
            )),
            '=' | '!' | '>' | '<' | '+' | '-' | '*' | '/' | '%' | '|' | '&' | '.' => {
                if c == '/' {
                    if self.peek() == '/' {
                        while self.peek() != '\n' && self.peek() != '\0' {
                            self.next();
                        }
                        return self.lex_token();
                    } else if self.peek() == '*' {
                        self.next();
                        while self.peek() != '*' || self.peek_over() != '/' {
                            self.next();
                        }
                        self.curr += 2;
                        return self.lex_token();
                    }
                }
                let x = format!("{}{}", c, self.peek());
                if let Ok(kind) = TokenKind::from_str(&x) {
                    self.curr += 1;
                    Ok(Some(self.create_token(kind)))
                } else {
                    Ok(Some(self.create_token(
                        TokenKind::from_str(&c.to_string()).unwrap(),
                    )))
                }
            }
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                let t = self.create_token(TokenKind::Newline);
                self.line += 1;
                self.col = 0;
                self.line_start = self.curr;
                Ok(Some(t))
            }
            _ if c.is_alphanumeric() || c == '_' => Ok(Some(self.identifier())),
            _ => Err(Error::new(
                (self.line, self.col),
                ErrorKind::UnexpectedChar(c),
            )),
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric()
            || self.peek() == '_'
            || (self.peek() == '.' && self.peek_over() != '.')
        {
            self.next();
        }

        let text = self.data[self.start..self.curr].iter().collect::<String>();

        if text.contains('.') {
            if let Ok(num) = text.parse() {
                return self.create_token_literal(
                    TokenKind::Number,
                    Some(Literal::Number(Number::Float(num))),
                );
            }
        }

        if text.chars().next().unwrap().is_numeric() {
            if text.len() < 3 {
                return self.create_token_literal(
                    TokenKind::Number,
                    Some(Literal::Number(Number::Int(text.parse().unwrap()))),
                );
            }

            let base = match &text[..2] {
                "0b" => 2,
                "0o" => 8,
                "0x" => 16,
                _ => 10,
            };

            return self.create_token_literal(
                TokenKind::Number,
                Some(Literal::Number(Number::Int(
                    u32::from_str_radix(if base != 10 { &text[2..] } else { &text }, base).unwrap(),
                ))),
            );
        }

        if let Ok(kind) = TokenKind::from_str(&text) {
            return self.create_token(kind);
        };

        self.create_token_literal(TokenKind::Identifier, Some(Literal::Identifier(text)))
    }

    fn next(&mut self) -> char {
        self.curr += 1;
        *self.data.get(self.curr - 1).unwrap()
    }

    fn peek(&self) -> char {
        *self.data.get(self.curr).unwrap_or(&'\0')
    }

    fn peek_over(&self) -> char {
        *self.data.get(self.curr + 1).unwrap_or(&'\0')
    }

    pub fn at_end(&self) -> bool {
        self.curr >= self.data.len()
    }

    fn create_token(&self, kind: TokenKind) -> Token {
        self.create_token_literal(kind, None)
    }

    fn create_token_literal(&self, kind: TokenKind, literal: Option<Literal>) -> Token {
        Token {
            kind,
            loc: (self.line, self.col),
            val: literal.map(Rc::new),
        }
    }
}
