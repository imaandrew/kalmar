use std::error::Error;
use std::io::Write;
use thiserror::Error;

use crate::compiler::Op;
use crate::lexer::{Literal, Token, TokenKind};
use crate::parser::Span;
use crate::sem_checker::Type;
use crate::StringManager;
use std::io::IsTerminal;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
/*
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
*/

#[derive(Error, Debug)]
pub enum KalmarError {
    #[error("unexpected character `{0}` when lexing")]
    UnexpectedChar(char),
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

pub enum NewKalmarError {
    UnexpectedChar(Token),
    BaseMissingNumber(Token),
    UnexpectedToken(Token, TokenKind),
    InvalidOperator(&'static str, Token),
    ExpectedStmt(Token),
    ExpectedExpr(Token),
    UnexpectedEndTokenStream,
    TooManyLabels(Token),
    RedeclaredScript(Token),
    RedeclaredLabel(Token),
    InvalidType(Span, Type, Type),
    InvalidTypes(Span, Vec<Type>, Type),
    UnequalTypes(Span, Type, Span, Type),
}

pub struct ErrorPrinter<'err, 'smgr> {
    file_name: &'err str,
    literals: &'err StringManager<'smgr>,
}

impl<'err, 'smgr> ErrorPrinter<'err, 'smgr> {
    pub fn new(file_name: &'err str, literals: &'err StringManager<'smgr>) -> Self {
        Self {
            file_name,
            literals,
        }
    }

    pub fn print(&self, error: &NewKalmarError) -> Result<(), std::io::Error> {
        let (msg, line, line_num, col_num, len) = match error {
            NewKalmarError::UnexpectedChar(t) => {
                let line = self.literals.err_context(t.line);
                let char = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("unexpected character: {}", char);
                (msg, line, t.line, t.col, t.len)
            }
            NewKalmarError::BaseMissingNumber(t) => {
                let line = self.literals.err_context(t.line);
                let base = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("found base prefix `{}`, but missing number", base);
                (msg, line, t.line, t.col, t.len)
            }
            NewKalmarError::UnexpectedToken(found, expected) => {
                let line = self.literals.err_context(found.line);
                let msg = format!("expected `{}` token, found `{}`", expected, found.kind);
                (msg, line, found.line, found.col, found.len)
            }
            NewKalmarError::InvalidOperator(expected, found) => {
                let line = self.literals.err_context(found.line);
                let msg = format!("expected `{}` operator, found `{}`", expected, found.kind);
                (msg, line, found.line, found.col, found.len)
            }
            NewKalmarError::ExpectedStmt(stmt) => {
                let line = self.literals.err_context(stmt.line);
                let msg = format!("expected statement, found `{}`", stmt.kind);
                (msg, line, stmt.line, stmt.col, stmt.len)
            }
            NewKalmarError::ExpectedExpr(stmt) => {
                let line = self.literals.err_context(stmt.line);
                let msg = format!("expected expression, found `{}`", stmt.kind);
                (msg, line, stmt.line, stmt.col, stmt.len)
            }
            NewKalmarError::UnexpectedEndTokenStream => {
                let line = self.literals.lines.last().unwrap();
                let msg = "unexpected end of token stream".to_string();
                (msg, *line, self.literals.lines.len() - 1, line.len(), 1)
            }
            NewKalmarError::TooManyLabels(t) => {
                let line = self.literals.err_context(t.line);
                let lbl = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!(
                    "cannot create new label `{}`, already at max 16 labels",
                    lbl
                );
                (msg, line, t.line, t.col, t.len)
            }
            NewKalmarError::RedeclaredScript(t) | NewKalmarError::RedeclaredLabel(t) => {
                let line = self.literals.err_context(t.line);
                let lbl = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("redeclaration of `{}`", lbl);
                (msg, line, t.line, t.col, t.len)
            }
            NewKalmarError::InvalidType(span, expected, found) => {
                let line = self.literals.err_context(span.line);
                let msg = format!("invalid type, expected `{}`, found `{}`", expected, found);
                (msg, line, span.line, span.col, span.len)
            }
            NewKalmarError::InvalidTypes(span, expected, found) => {
                let line = self.literals.err_context(span.line);
                let msg = format!(
                    "invalid type, expected one of `{:?}`, found `{}`",
                    expected, found
                );
                (msg, line, span.line, span.col, span.len)
            }
            NewKalmarError::UnequalTypes(l_span, l_type, r_span, r_type) => {
                let line = self.literals.err_context(l_span.line);
                let msg = format!(
                    "type mismatch, lhs has type `{}`, rhs has type `{}`",
                    l_type, r_type
                );
                // TODO: highlight both types independently
                (
                    msg,
                    line,
                    l_span.line,
                    l_span.col,
                    r_span.len + r_span.col - l_span.col,
                )
            }
        };

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
            ($($arg:tt)*) => {{
                stderr.set_color(c.set_fg(None))?;
                write!(&mut stderr, $($arg)*)?;
            }};
        }

        let margin = line_num.to_string().len();

        write_colour!("{}:{}:{}: ", self.file_name, line_num + 1, col_num + 1);
        write_colour!(Red, "error");
        writeln_colour!(": {}", msg);

        write_colour!("{:<margin$} | ", line_num + 1);
        writeln_colour!("{}", line);

        write_colour!("{:>margin$} | ", "");
        writeln_colour!(Red, "{:>col_num$}{:^>len$}", "", "");
        stderr.set_color(c.set_fg(None))?;
        Ok(())
    }
}
