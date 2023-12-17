use std::fmt;

use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct Error {
    pub pos: (usize, usize),
    pub kind: ErrorKind,
    pub underline_len: usize,
    pub line: String,
}

impl Error {
    pub fn new(pos: (usize, usize), kind: ErrorKind, line: String) -> Self {
        let l = kind.get_len();
        Self {
            pos,
            kind,
            line,
            underline_len: l,
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    ExpectedBinOp(Token),
}

impl ErrorKind {
    fn get_len(&self) -> usize {
        match self {
            Self::UnexpectedChar(_) => 1,
            Self::ExpectedBinOp(t) => match t.kind {
                TokenKind::Number | TokenKind::Identifier => format!("{}", t.val.as_ref().unwrap()),
                _ => format!("{}", t.kind),
            }
            .len(),
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "unexpected character: {}", c),
            Self::ExpectedBinOp(t) => {
                let i = match t.kind {
                    TokenKind::Number | TokenKind::Identifier => {
                        format!("{}", t.val.as_ref().unwrap())
                    }
                    _ => format!("{}", t.kind),
                };
                write!(f, "expected binary operator, found: {}", i)
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let margin = self.pos.0.to_string().len();
        let line_start = self.pos.1 - 1;
        let line_len = self.underline_len;
        writeln!(f, "error: {}", self.kind)?;
        writeln!(f, "{:>margin$} |", "")?;
        writeln!(f, "{:<margin$} | {}", self.pos.0, self.line)?;
        writeln!(f, "{:>margin$} | {:>line_start$}{:^>line_len$}", "", "", "")?;
        writeln!(f, "{:>margin$} |", "")
    }
}
