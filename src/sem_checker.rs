use strum_macros::Display;

use crate::{
    error::KalmarError,
    lexer::Literal,
    parser::{BinOp, Expr, Stmt, UnOp},
};

#[derive(Debug, PartialEq, Display, Copy, Clone)]
pub enum Type {
    Identifier,
    Integer,
    Float,
    Boolean,
    Var,
    Range,
    FuncCall,
    Assign,
    Case,
    VarList,
}

macro_rules! assert_types {
    ($lhs:expr, $pattern:pat, $($x:expr),+) => {{
        match $lhs {
            $pattern => Ok(()),
            t => {
                Err(KalmarError::TypesMismatch(vec![$($x),+], t))
            }
        }
    }};
    ($lhs:expr, $pattern:pat, $x:expr) => {{
        match $lhs {
            $pattern => Ok(()),
            t => {
                Err(KalmarError::TypeMismatch($x, t))
            }
        }
    }};
}

pub struct SemChecker<'a> {
    declared_scripts: Vec<&'a str>,
    referenced_scripts: Vec<&'a str>,
    declared_labels: Vec<&'a str>,
    referenced_labels: Vec<&'a str>,
}

impl Default for SemChecker<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl SemChecker<'_> {
    pub fn new() -> Self {
        Self {
            declared_scripts: vec![],
            referenced_scripts: vec![],
            declared_labels: vec![],
            referenced_labels: vec![],
        }
    }
}

impl<'a> SemChecker<'a> {
    pub fn check_ast(&mut self, ast: &'a Vec<Stmt>) -> Result<(), KalmarError> {
        self.check_nodes(ast)?;
        self.verify_referenced_identifiers(&self.declared_scripts, &self.referenced_scripts)?;
        Ok(())
    }

    fn check_nodes(&mut self, stmts: &'a Vec<Stmt>) -> Result<(), KalmarError> {
        for stmt in stmts {
            self.check_stmt_node(stmt)?;
        }
        Ok(())
    }

    fn check_stmt_node(&mut self, stmt: &'a Stmt) -> Result<(), KalmarError> {
        match stmt {
            Stmt::Script(i, s) => {
                self.declared_labels.clear();
                self.referenced_labels.clear();
                let script = self
                    .check_identifier_uniqueness(i, &self.declared_scripts)
                    .ok_or(KalmarError::RedeclaredScript(i.to_string()))?;
                self.declared_scripts.push(script);
                self.check_stmt_node(s)?;
                self.verify_referenced_identifiers(&self.declared_labels, &self.referenced_labels)?;
            }
            Stmt::Block(s) => self.check_nodes(s)?,
            Stmt::Label(l) => {
                if self.declared_labels.len() >= 16 {
                    return Err(KalmarError::TooManyLabels);
                }
                self.declared_labels.push(
                    self.check_identifier_uniqueness(l, &self.declared_labels)
                        .ok_or(KalmarError::RedeclaredLabel(l.to_string()))?,
                );
            }
            Stmt::Goto(l) => match l {
                Literal::Identifier(i) => {
                    if !self.referenced_labels.contains(&i.as_str()) {
                        self.referenced_labels.push(i);
                    }
                }
                _ => unreachable!(),
            },
            Stmt::Loop(e, s) => {
                if let Some(e) = e {
                    assert_types!(self.check_expr_node(e)?, Type::Integer, Type::Integer)?;
                }
                self.check_stmt_node(s)?;
            }
            Stmt::IfElse(i, e) => {
                self.check_stmt_node(i)?;
                if let Some(e) = e {
                    self.check_stmt_node(e)?;
                }
            }
            Stmt::If(e, s) => {
                assert_types!(self.check_expr_node(e)?, Type::Boolean, Type::Boolean)?;
                self.check_stmt_node(s)?;
            }
            Stmt::Else(i, b) => {
                assert_ne!(i.is_some(), b.is_some());
                if let Some(e) = i {
                    self.check_stmt_node(e)?;
                }

                if let Some(e) = b {
                    self.check_stmt_node(e)?;
                }
            }
            Stmt::Jump(l) => match l {
                Literal::Identifier(i) => {
                    if !self.referenced_scripts.contains(&i.as_str()) {
                        self.referenced_scripts.push(i);
                    }
                }
                _ => unreachable!(),
            },
            Stmt::Thread(s) => self.check_stmt_node(s)?,
            Stmt::ChildThread(s) => self.check_stmt_node(s)?,
            Stmt::Expr(e) => assert_types!(
                self.check_expr_node(e)?,
                Type::Assign | Type::FuncCall,
                Type::Assign,
                Type::FuncCall
            )?,
            Stmt::Switch(e, s) => {
                assert_types!(
                    self.check_expr_node(e)?,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                )?;
                self.check_stmt_node(s)?;
            }
            Stmt::Case(e, s) => {
                self.check_expr_node(e)?;
                self.check_stmt_node(s)?;
            }
            Stmt::Return | Stmt::BreakCase | Stmt::BreakLoop | Stmt::Empty => (),
        };

        Ok(())
    }

    fn check_expr_node(&self, expr: &Expr) -> Result<Type, KalmarError> {
        Ok(match expr {
            Expr::Identifier(l) => match l {
                Literal::Identifier(_) => Type::Identifier,
                Literal::Number(n) => {
                    if n.is_float() {
                        Type::Float
                    } else {
                        Type::Integer
                    }
                }
                Literal::Boolean(_) => Type::Boolean,
            },
            Expr::Array(_, e) => {
                assert_types!(self.check_expr_node(e)?, Type::Integer, Type::Integer)?;
                Type::Var
            }
            Expr::UnOp(op, expr) => self.check_unop_type(op, expr)?,
            Expr::BinOp(op, lhs, rhs) => self.check_binop_type(op, lhs, rhs)?,
            Expr::FuncCall(_, args) => {
                for e in args {
                    self.check_expr_node(e)?;
                }
                Type::FuncCall
            }
            Expr::ArrayAssign(_, e) => {
                assert_types!(
                    self.check_expr_node(e)?,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                )?;
                Type::Assign
            }
            Expr::Default => Type::Case,
        })
    }

    fn check_unop_type(&self, op: &UnOp, expr: &Expr) -> Result<Type, KalmarError> {
        let t = self.check_expr_node(expr)?;
        Ok(match op {
            UnOp::Minus => {
                assert_types!(t, Type::Integer | Type::Float, Type::Integer, Type::Float)?;
                t
            }
            UnOp::Bang => {
                assert_types!(t, Type::Boolean, Type::Boolean)?;
                Type::Boolean
            }
            UnOp::Equal
            | UnOp::NotEqual
            | UnOp::Greater
            | UnOp::GreaterEq
            | UnOp::Less
            | UnOp::LessEq => {
                assert_types!(
                    t,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Case
            }
            UnOp::Ampersand => {
                assert_types!(t, Type::Var | Type::Integer, Type::Var, Type::Integer)?;
                t
            }
        })
    }

    fn check_binop_type(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Result<Type, KalmarError> {
        let l_type = self.check_expr_node(lhs)?;
        let r_type = self.check_expr_node(rhs)?;
        Ok(match op {
            BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Div => {
                if l_type != r_type {
                    return Err(KalmarError::TypeMismatch(l_type, r_type));
                }
                assert_types!(
                    l_type,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                )?;
                l_type
            }
            BinOp::Mod | BinOp::BitOr => {
                assert_types!(l_type, Type::Integer, Type::Integer)?;
                assert_types!(r_type, Type::Integer, Type::Integer)?;
                l_type
            }
            BinOp::BitAnd => {
                assert_types!(l_type, Type::Var | Type::Integer, Type::Var, Type::Integer)?;
                assert_types!(r_type, Type::Integer, Type::Integer)?;
                if l_type == Type::Var {
                    Type::Boolean
                } else {
                    Type::Integer
                }
            }
            BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::DivEq => {
                assert_types!(l_type, Type::Var, Type::Var)?;
                assert_types!(
                    r_type,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Assign
            }
            BinOp::ModEq | BinOp::OrEq | BinOp::AndEq => {
                assert_types!(l_type, Type::Var, Type::Var)?;
                assert_types!(r_type, Type::Integer | Type::Var, Type::Integer, Type::Var)?;
                Type::Assign
            }
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Greater
            | BinOp::GreaterEq
            | BinOp::Less
            | BinOp::LessEq => {
                assert_types!(
                    l_type,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                assert_types!(
                    r_type,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                if l_type != Type::Var && r_type != Type::Var && l_type != r_type {
                    return Err(KalmarError::TypeMismatch(l_type, r_type));
                }
                Type::Boolean
            }
            BinOp::Assign => {
                assert_types!(l_type, Type::Var, Type::Var)?;
                assert_types!(
                    r_type,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Assign
            }
            BinOp::Range => {
                assert_types!(l_type, Type::Integer, Type::Integer)?;
                assert_types!(r_type, Type::Integer, Type::Integer)?;
                Type::Range
            }
            BinOp::Comma => {
                assert_types!(l_type, Type::Var | Type::VarList, Type::Var, Type::VarList)?;
                assert_types!(r_type, Type::Var, Type::Var)?;
                Type::VarList
            }
            BinOp::Arrow => {
                assert_types!(l_type, Type::Var | Type::VarList, Type::Var, Type::VarList)?;
                assert_types!(r_type, Type::Identifier, Type::Identifier)?;
                Type::Assign
            }
        })
    }

    fn check_identifier_uniqueness(
        &self,
        ident: &'a Literal,
        declared: &[&'a str],
    ) -> Option<&'a str> {
        let name = match ident {
            Literal::Identifier(i) => i,
            _ => unreachable!(),
        };

        if declared.contains(&name.as_str()) {
            return None;
        }

        Some(name)
    }

    fn verify_referenced_identifiers(
        &self,
        declared: &[&'a str],
        referenced: &Vec<&'a str>,
    ) -> Result<(), KalmarError> {
        let mut undeclared_references = vec![];
        for ident in referenced {
            if !declared.contains(ident) {
                //TODO: maybe get rid of the clone? doesn't matter too much as it only runs when we get an error
                undeclared_references.push(ident.to_string());
            }
        }

        if undeclared_references.is_empty() {
            Ok(())
        } else {
            Err(KalmarError::UndeclaredReference(undeclared_references))
        }
    }
}
