use std::str::FromStr;

use strum_macros::EnumString;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub val: Option<Literal>,
    pub loc: (usize, isize),
}

impl Token {
    pub fn is_last(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }

    pub fn get_ident(&self) -> String {
        if let Token {
            kind: TokenKind::Identifier,
            val: Some(Literal::Identifier(s)),
            loc: _,
        } = self
        {
            return s.to_string();
        }
        panic!("{:?}", self);
    }
}

#[derive(Copy, Clone, Debug, EnumString, PartialEq, Eq)]
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
    #[strum(serialize = "case")]
    KwCase,
    #[strum(serialize = "thread")]
    KwThread,
    #[strum(serialize = "cthread")]
    KwChildThread,
    #[strum(serialize = "if")]
    KwIf,
    #[strum(serialize = "else")]
    KwElse,
    #[strum(serialize = "and")]
    KwAnd,
    #[strum(serialize = "or")]
    KwOr,
    #[strum(serialize = "default")]
    KwDefault,
    #[strum(serialize = "jump")]
    KwJump,
    #[strum(serialize = "new")]
    KwNew,
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
    #[strum(serialize = "_")]
    Underscore,
    #[strum(serialize = "#")]
    Hash,
    #[strum(serialize = "..")]
    Range,
    #[strum(disabled)]
    Number,
    #[strum(disabled)]
    String,
    Newline,
    #[strum(disabled)]
    Identifier,
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Byte(u8),
    Short(u16),
    Integer(u32),
    Float(f32),
}

impl Number {
    pub fn as_u32(&self) -> u32 {
        match *self {
            Number::Byte(x) => x as u32,
            Number::Short(x) => x as u32,
            Number::Integer(x) => x,
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    Str(String),
    Number(Number),
}

pub struct Lexer {
    data: Vec<char>,
    col: isize,
    line: usize,
    start: usize,
    curr: usize,
}

impl Lexer {
    pub fn new(data: &str) -> Self {
        Lexer {
            data: data.chars().collect(),
            col: 0,
            line: 1,
            start: 0,
            curr: 0,
        }
    }

    pub fn lex(&mut self) -> Token {
        if !self.at_end() {
            self.col += (self.curr - self.start) as isize;
            self.start = self.curr;
            if let Some(t) = self.lex_token() {
                return t;
            }
            return self.lex();
        }
        self.create_token(TokenKind::Eof)
    }

    fn lex_token(&mut self) -> Option<Token> {
        let c = self.next();
        match c {
            '(' | ')' | '{' | '}' | '[' | ']' | ':' | ',' | '_' | '#' => {
                Some(self.create_token(TokenKind::from_str(&c.to_string()).unwrap()))
            }
            '=' | '!' | '>' | '<' | '+' | '-' | '*' | '/' | '%' | '|' | '&' | '.' => {
                let x = format!("{}{}", c, self.peek());
                if let Ok(kind) = TokenKind::from_str(&x) {
                    self.curr += 1;
                    Some(self.create_token(kind))
                } else {
                    Some(self.create_token(TokenKind::from_str(&c.to_string()).unwrap()))
                }
            }
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                self.col = -1;
                Some(self.create_token(TokenKind::Newline))
            }
            _ if c.is_alphanumeric() || c == '_' => Some(self.identifier()),
            _ => panic!(),
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric()
            || self.peek() == '_'
            || (self.peek() == '.' && self.peek_x(2) != '.')
        {
            self.next();
        }

        let text = self.data[self.start..self.curr].iter().collect::<String>();

        if text.contains('.') {
            if let Ok(num) = text.parse() {
                self.create_token_literal(
                    TokenKind::Number,
                    Some(Literal::Number(Number::Float(num))),
                );
            }
        }

        if text.chars().next().unwrap().is_numeric() {
            if text.len() < 3 {
                return self.create_token_literal(
                    TokenKind::Number,
                    Some(Literal::Number(Number::Integer(text.parse().unwrap()))),
                );
            }

            let base = match &text[..2] {
                "0b" => 2,
                "0o" => 8,
                "0x" => 16,
                _ => 10,
            };

            let (size, num) = if let Some(t) = text.find('u') {
                let ty = &text[t..];
                let num = if base != 10 { &text[2..t] } else { &text[..t] };
                (ty, num)
            } else if base != 10 {
                ("u32", &text[2..])
            } else {
                ("u32", &text[..])
            };

            let num = match size {
                "u8" => Number::Byte(u8::from_str_radix(num, base).unwrap()),
                "u16" => Number::Short(u16::from_str_radix(num, base).unwrap()),
                "u32" => Number::Integer(u32::from_str_radix(num, base).unwrap()),
                _ => panic!(),
            };

            return self.create_token_literal(TokenKind::Number, Some(Literal::Number(num)));
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

    fn peek_x(&self, x: usize) -> char {
        *self.data.get(self.curr + x - 1).unwrap_or(&'\0')
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
            val: literal,
        }
    }
}
