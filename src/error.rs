use std::io::Write;
use thiserror::Error;

use crate::compiler::Op;
use crate::lexer::{Literal, Span, Token, TokenKind};
use crate::sem_checker::Type;
use crate::StringManager;
use std::io::IsTerminal;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[derive(Error, Debug)]
pub enum DecompilerError {
    #[error("undeclared reference to `{0:?}`")]
    UndeclaredReference(Vec<String>),
    #[error("opcode {0} is stating {1} args instead of expected {2}")]
    UnexpectOpArgCount(Op, u32, u32),
    #[error("idk come up with something")]
    CursorOutOfBounds(usize, usize),
    #[error("unexpected end token")]
    UnexpectedEndToken,
    #[error("invalid opcode {0}")]
    InvalidOpcode(u32),
}

#[derive(Debug)]
pub enum KalmarError {
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
    UndefinedFunction(Token),
    UndeclaredIdentifier(Token),
    InvalidAssign(Token),
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

    pub fn print(&self, error: &KalmarError) -> Result<(), std::io::Error> {
        let (msg, line, line_num, col_num, len) = match error {
            KalmarError::UnexpectedChar(t) => {
                let line = self.literals.err_context(t.span.line);
                let char = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("unexpected character: {}", char);
                (msg, line, t.span.line, t.span.col, t.span.len)
            }
            KalmarError::BaseMissingNumber(t) => {
                let line = self.literals.err_context(t.span.line);
                let base = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("found base prefix `{}`, but missing number", base);
                (msg, line, t.span.line, t.span.col, t.span.len)
            }
            KalmarError::UnexpectedToken(found, expected) => {
                let line = self.literals.err_context(found.span.line);
                let msg = format!("expected `{}` token, found `{}`", expected, found.kind);
                (msg, line, found.span.line, found.span.col, found.span.len)
            }
            KalmarError::InvalidOperator(expected, found) => {
                let line = self.literals.err_context(found.span.line);
                let msg = format!("expected `{}` operator, found `{}`", expected, found.kind);
                (msg, line, found.span.line, found.span.col, found.span.len)
            }
            KalmarError::ExpectedStmt(stmt) => {
                let line = self.literals.err_context(stmt.span.line);
                let msg = format!("expected statement, found `{}`", stmt.kind);
                (msg, line, stmt.span.line, stmt.span.col, stmt.span.len)
            }
            KalmarError::ExpectedExpr(stmt) => {
                let line = self.literals.err_context(stmt.span.line);
                let msg = format!("expected expression, found `{}`", stmt.kind);
                (msg, line, stmt.span.line, stmt.span.col, stmt.span.len)
            }
            KalmarError::UnexpectedEndTokenStream => {
                let line = self.literals.lines.last().unwrap();
                let msg = "unexpected end of token stream".to_string();
                (msg, *line, self.literals.lines.len() - 1, line.len(), 1)
            }
            KalmarError::TooManyLabels(t) => {
                let line = self.literals.err_context(t.span.line);
                let lbl = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!(
                    "cannot create new label `{}`, already at max 16 labels",
                    lbl
                );
                (msg, line, t.span.line, t.span.col, t.span.len)
            }
            KalmarError::RedeclaredScript(t) | KalmarError::RedeclaredLabel(t) => {
                let line = self.literals.err_context(t.span.line);
                let lbl = match t.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("redeclaration of `{}`", lbl);
                (msg, line, t.span.line, t.span.col, t.span.len)
            }
            KalmarError::InvalidType(span, expected, found) => {
                let line = self.literals.err_context(span.line);
                let msg = format!("invalid type, expected `{}`, found `{}`", expected, found);
                (msg, line, span.line, span.col, span.len)
            }
            KalmarError::InvalidTypes(span, expected, found) => {
                let line = self.literals.err_context(span.line);
                let msg = format!(
                    "invalid type, expected one of `{:?}`, found `{}`",
                    expected, found
                );
                (msg, line, span.line, span.col, span.len)
            }
            KalmarError::UnequalTypes(l_span, l_type, r_span, r_type) => {
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
            KalmarError::UndefinedFunction(f) => {
                let line = self.literals.err_context(f.span.line);
                let func = match f.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("reference to undefined function `{}`", func);
                (msg, line, f.span.line, f.span.col, f.span.len)
            }
            KalmarError::UndeclaredIdentifier(s) => {
                let line = self.literals.err_context(s.span.line);
                let sym = match s.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("reference to undefined identifier `{}`", sym);
                (msg, line, s.span.line, s.span.col, s.span.len)
            }
            KalmarError::InvalidAssign(s) => {
                let line = self.literals.err_context(s.span.line);
                let sym = match s.val.unwrap() {
                    Literal::Identifier(i) => self.literals.get(i).unwrap(),
                    _ => panic!(),
                };
                let msg = format!("invalid variable assign to identifier `{}`", sym);
                (msg, line, s.span.line, s.span.col, s.span.len)
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

        let margin = (line_num + 1).to_string().len();

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
