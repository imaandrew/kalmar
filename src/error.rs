use std::{fmt, io::Write, rc::Rc};
use thiserror::Error;

use crate::compiler::Op;
use crate::lexer::{Token, TokenKind};
use crate::sem_checker::Type;
use std::io::IsTerminal;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
pub struct ContextPrinter<'a> {
    ctxt: Vec<&'a str>,
}

impl<'a> ContextPrinter<'a> {
    pub fn new(ctxt: &'a str) -> Self {
        Self {
            ctxt: ctxt.lines().collect(),
        }
    }

    pub fn print(&self, err: &Error) -> Result<(), std::io::Error> {
        let margin = err.pos.0.to_string().len();
        let line_start = err.pos.1;
        let line_len = err.underline_len;
        let mut stderr = StandardStream::stderr(if std::io::stdin().is_terminal() {
            ColorChoice::Always
        } else {
            ColorChoice::Never
        });
        let mut c = ColorSpec::new();

        macro_rules! writeln_colour {
            ($colour:ident, $($arg:tt)*) => {{
                stderr.set_color(c.set_fg(Some(Color::$colour)))?;
                writeln!(&mut stderr, $($arg)*)?;
            }};
            ($($arg:tt)*) => {{
                stderr.set_color(c.set_fg(None))?;
                writeln!(&mut stderr, $($arg)*)?;
            }};
        }

        macro_rules! write_colour {
            ($colour:ident, $($arg:tt)*) => {{
                stderr.set_color(c.set_fg(Some(Color::$colour)))?;
                write!(&mut stderr, $($arg)*)?;
            }};
        }

        write_colour!(Red, "error");
        writeln_colour!(": {}", err);

        writeln_colour!(Blue, "{:>margin$} |", "");

        write!(&mut stderr, "{:<margin$} | ", err.pos.0,)?;
        writeln_colour!("{}", self.ctxt.get(err.pos.0 - 1).unwrap());

        write_colour!(Blue, "{:>margin$} |", "");
        writeln_colour!(Red, "{:>line_start$}{:^>line_len$}", "", "");

        writeln_colour!(Blue, "{:>margin$} |", "");
        Ok(())
    }
}

#[derive(Debug)]
pub struct Error {
    pub pos: (usize, usize),
    pub kind: ErrorKind,
    pub underline_len: usize,
}

impl Error {
    pub fn new(pos: (usize, usize), kind: ErrorKind) -> Self {
        let l = kind.get_len();
        Self {
            pos,
            kind,
            underline_len: l,
        }
    }
}

#[derive(Error, Debug)]
pub enum KalmarError {
    #[error("unexpected character `{0}` when lexing")]
    UnexpectedChar(char),
    #[error("could not parse `{0}` as int")]
    IntParseError(String),
    #[error("unexpected token: expected `{0}`, found `{1}`")]
    UnexpectedToken(TokenKind, TokenKind),
    #[error("expected `{0}` operator, found `{1}`")]
    InvalidOperator(&'static str, TokenKind),
    #[error("expected statement, found `{0}`")]
    ExpectedStmt(TokenKind),
    #[error("expected expression, found `{0}`")]
    ExpectedExpr(TokenKind),
    #[error("script `{0}` redeclared")]
    RedeclaredScript(String),
    #[error("label `{0}` redeclared")]
    RedeclaredLabel(String),
    #[error("cannot have more than 16 labels per script")]
    TooManyLabels,
    #[error("undeclared reference to `{0:?}`")]
    UndeclaredReference(Vec<String>),
    #[error("mismatched types: expected `{0}`, found `{1}`")]
    TypeMismatch(Type, Type),
    #[error("mismatched types: expected one of `{0:?}`, found `{1}`")]
    TypesMismatch(Vec<Type>, Type),
    #[error("opcode {0} is stating {1} args instead of expected {2}")]
    UnexpectOpArgCount(Op, u32, u32),
    #[error("idk come up with something")]
    CursorOutOfBounds(usize, usize),
    #[error("unexpected end token")]
    UnexpectedEndToken,
    #[error("invalid opcode {0}")]
    InvalidOpcode(u32),
    #[error("referencing undefined function `{0}`")]
    UndefinedFunction(String),
    #[error("referencing undefined symbol `{0}`")]
    UndefinedSymbol(String),
    #[error("unexpected end of token stream")]
    UnexpectedEndTokenStream,
    #[error("base prefix `{0}` missing number")]
    BaseMissingNumber(String),
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    ExpectedBinOp(Rc<Token>),
    UnexpectedToken(TokenKind, Rc<Token>),
    ExpectedStmt(Rc<Token>),
    ExpectedExpr(Rc<Token>),
    RedeclaredLabel(Rc<Token>),
    RedeclaredScr(Rc<Token>),
}

impl ErrorKind {
    fn get_len(&self) -> usize {
        match self {
            Self::UnexpectedChar(_) => 1,
            Self::ExpectedStmt(t)
            | Self::ExpectedBinOp(t)
            | Self::ExpectedExpr(t)
            | Self::UnexpectedToken(_, t)
            | Self::RedeclaredLabel(t)
            | Self::RedeclaredScr(t) => match t.kind {
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
            Self::UnexpectedToken(ex, fnd) => {
                write!(f, "expected token of type: {:?}, found: {:?}", ex, fnd.kind)
            }
            Self::ExpectedStmt(t) => write!(f, "expected statement, found: {:?}", t.kind),
            Self::ExpectedExpr(t) => write!(f, "expected expression, found: {:?}", t.kind),
            Self::RedeclaredLabel(t) => write!(f, "label: {} redeclared", t.val.as_ref().unwrap()),
            Self::RedeclaredScr(t) => write!(f, "script: {} redeclared", t.val.as_ref().unwrap()),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
