use std::str::FromStr;

use strum_macros::EnumString;

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    loc: (usize, isize),
}

#[derive(Debug, EnumString)]
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
    Number(Number),
    #[strum(disabled)]
    String(String),
    Newline,
    #[strum(disabled)]
    Identifier(String),
}

#[derive(Debug)]
pub enum Number {
    Byte(u8),
    Short(u16),
    Integer(u32),
    Float(f32),
}

pub struct Lexer {
    data: Vec<char>,
    pub tokens: Vec<Token>,
    col: isize,
    line: usize,
    start: usize,
    curr: usize,
}

impl Lexer {
    pub fn new(data: &str) -> Self {
        Lexer {
            data: data.chars().collect(),
            tokens: vec![],
            col: 0,
            line: 0,
            start: 0,
            curr: 0,
        }
    }

    pub fn lex(&mut self) {
        while !self.at_end() {
            self.col += (self.curr - self.start) as isize;
            self.start = self.curr;
            if let Some(t) = self.lex_token() {
                self.tokens.push(t);
            }
        }
    }

    fn lex_token(&mut self) -> Option<Token> {
        let c = self.next();
        match c {
            '(' | ')' | '{' | '}' | '[' | ']' | ':' | ',' | '_' | '#' => {
                self.create_token(TokenKind::from_str(&c.to_string()).unwrap())
            }
            '=' | '!' | '>' | '<' | '+' | '-' | '*' | '/' | '%' | '|' | '&' => {
                let x = format!("{}{}", c, self.peek());
                if let Ok(kind) = TokenKind::from_str(&x) {
                    self.curr += 1;
                    self.create_token(kind)
                } else {
                    self.create_token(TokenKind::from_str(&c.to_string()).unwrap())
                }
            }
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                self.col = -1;
                None
            }
            _ if c.is_alphanumeric() => self.identifier(),
            _ => panic!(),
        }
    }

    fn identifier(&mut self) -> Option<Token> {
        while self.peek().is_alphanumeric() || self.peek() == '_' || self.peek() == '.' {
            self.next();
        }

        let text = self.data[self.start..self.curr].iter().collect::<String>();

        if text.contains('.') {
            if let Ok(num) = text.parse() {
                self.create_token(TokenKind::Number(Number::Float(num)));
            }
        }

        if text.chars().next().unwrap().is_numeric() {
            if text.len() < 3 {
                return self
                    .create_token(TokenKind::Number(Number::Integer(text.parse().unwrap())));
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

            return self.create_token(TokenKind::Number(num));
        }

        if let Ok(kind) = TokenKind::from_str(&text) {
            return self.create_token(kind);
        };

        self.create_token(TokenKind::Identifier(text))
    }

    fn next(&mut self) -> char {
        self.curr += 1;
        *self.data.get(self.curr - 1).unwrap()
    }

    fn peek(&self) -> char {
        *self.data.get(self.curr).unwrap_or(&'\0')
    }

    fn at_end(&self) -> bool {
        self.curr == self.data.len()
    }

    fn create_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token {
            kind,
            loc: (self.line, self.col),
        })
    }
}
