use strum_macros::Display;

use crate::{
    error::KalmarError,
    lexer::{Literal, Token},
    parser::{BinOp, Expr, ExprKind, Stmt, UnOp},
    StringManager,
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
    ($lhs:expr, $span:expr, $pattern:pat, $x:expr) => {{
        match $lhs {
            $pattern => Ok(()),
            t => {
                Err(KalmarError::InvalidType($span, $x, t))
            }
        }
    }};
    ($lhs:expr, $span:expr, $pattern:pat, $($x:expr),+) => {{
        match $lhs {
            $pattern => Ok(()),
            t => {
                Err(KalmarError::InvalidTypes($span, vec![$($x),+], t))
            }
        }
    }};
}

pub struct SemChecker<'a> {
    declared_scripts: Vec<&'a str>,
    referenced_scripts: Vec<&'a str>,
    declared_labels: Vec<&'a str>,
    referenced_labels: Vec<&'a str>,
    literals: &'a StringManager<'a>,
}

impl<'a> SemChecker<'a> {
    pub fn new(literals: &'a mut StringManager) -> Self {
        Self {
            declared_scripts: vec![],
            referenced_scripts: vec![],
            declared_labels: vec![],
            referenced_labels: vec![],
            literals,
        }
    }
}

impl<'a> SemChecker<'a> {
    pub fn check_ast(&mut self, ast: &'a [Stmt]) -> Result<(), KalmarError> {
        self.check_nodes(ast)?;
        self.verify_referenced_identifiers(&self.declared_scripts, &self.referenced_scripts)?;
        Ok(())
    }

    fn check_nodes(&mut self, stmts: &'a [Stmt]) -> Result<(), KalmarError> {
        for stmt in stmts {
            self.check_stmt_node(stmt)?;
        }
        Ok(())
    }

    fn check_stmt_node(&mut self, stmt: &'a Stmt) -> Result<(), KalmarError> {
        match stmt {
            Stmt::Script(
                t @ Token {
                    val: Some(Literal::Identifier(i)),
                    ..
                },
                s,
            ) => {
                self.declared_labels.clear();
                self.referenced_labels.clear();
                let script = self
                    .check_identifier_uniqueness(
                        self.literals.get(*i).unwrap(),
                        &self.declared_scripts,
                    )
                    .ok_or(KalmarError::RedeclaredScript(*t))?;
                self.declared_scripts.push(script);
                self.check_stmt_node(s)?;
                self.verify_referenced_identifiers(&self.declared_labels, &self.referenced_labels)?;
            }
            Stmt::Block(s) => self.check_nodes(s)?,
            Stmt::Label(
                t @ Token {
                    val: Some(Literal::Identifier(i)),
                    ..
                },
            ) => {
                if self.declared_labels.len() >= 16 {
                    return Err(KalmarError::TooManyLabels(*t));
                }
                self.declared_labels.push(
                    self.check_identifier_uniqueness(
                        self.literals.get(*i).unwrap(),
                        &self.declared_labels,
                    )
                    .ok_or(KalmarError::RedeclaredLabel(*t))?,
                );
            }
            Stmt::Goto(Token {
                val: Some(Literal::Identifier(i)),
                ..
            }) => {
                let l = self.literals.get(*i).unwrap();
                if !self.referenced_labels.contains(&l) {
                    self.referenced_labels.push(l);
                }
            }
            Stmt::Loop(e, s) => {
                if let Some(e) = e {
                    assert_types!(
                        self.check_expr_node(e)?,
                        e.span,
                        Type::Integer,
                        Type::Integer
                    )?;
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
                assert_types!(
                    self.check_expr_node(e)?,
                    e.span,
                    Type::Boolean,
                    Type::Boolean
                )?;
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
            Stmt::Jump(Token {
                val: Some(Literal::Identifier(i)),
                ..
            }) => {
                let l = self.literals.get(*i).unwrap();
                if !self.referenced_scripts.contains(&l) {
                    self.referenced_scripts.push(l);
                }
            }
            Stmt::Thread(s) => self.check_stmt_node(s)?,
            Stmt::ChildThread(s) => self.check_stmt_node(s)?,
            Stmt::Expr(e) => assert_types!(
                self.check_expr_node(e)?,
                e.span,
                Type::Assign | Type::FuncCall,
                Type::Assign,
                Type::FuncCall
            )?,
            Stmt::Switch(e, s) => {
                assert_types!(
                    self.check_expr_node(e)?,
                    e.span,
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
            _ => unreachable!(),
        };

        Ok(())
    }

    fn check_expr_node(&self, expr: &Expr) -> Result<Type, KalmarError> {
        Ok(match &expr.kind {
            ExprKind::Identifier(l) => match l.val.unwrap() {
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
            ExprKind::Array(_, e) => {
                assert_types!(
                    self.check_expr_node(e)?,
                    e.span,
                    Type::Integer,
                    Type::Integer
                )?;
                Type::Var
            }
            ExprKind::UnOp(op, expr) => self.check_unop_type(op, expr)?,
            ExprKind::BinOp(op, lhs, rhs) => self.check_binop_type(op, lhs, rhs)?,
            ExprKind::FuncCall(_, args) => {
                for e in args {
                    self.check_expr_node(e)?;
                }
                Type::FuncCall
            }
            ExprKind::ArrayAssign(_, e) => {
                assert_types!(
                    self.check_expr_node(e)?,
                    e.span,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                )?;
                Type::Assign
            }
            ExprKind::Default => Type::Case,
        })
    }

    fn check_unop_type(&self, op: &UnOp, expr: &Expr) -> Result<Type, KalmarError> {
        let t = self.check_expr_node(expr)?;
        Ok(match op {
            UnOp::Minus => {
                assert_types!(
                    t,
                    expr.span,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                )?;
                t
            }
            UnOp::Bang => {
                assert_types!(t, expr.span, Type::Boolean, Type::Boolean)?;
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
                    expr.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Case
            }
            UnOp::Ampersand => {
                assert_types!(
                    t,
                    expr.span,
                    Type::Var | Type::Integer,
                    Type::Var,
                    Type::Integer
                )?;
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
                    return Err(KalmarError::UnequalTypes(
                        lhs.span, l_type, rhs.span, r_type,
                    ));
                }
                assert_types!(
                    l_type,
                    lhs.span,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                )?;
                l_type
            }
            BinOp::Mod | BinOp::BitOr => {
                assert_types!(l_type, lhs.span, Type::Integer, Type::Integer)?;
                assert_types!(r_type, rhs.span, Type::Integer, Type::Integer)?;
                l_type
            }
            BinOp::BitAnd => {
                assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::Integer,
                    Type::Var,
                    Type::Integer
                )?;
                assert_types!(r_type, rhs.span, Type::Integer, Type::Integer)?;
                if l_type == Type::Var {
                    Type::Boolean
                } else {
                    Type::Integer
                }
            }
            BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::DivEq => {
                assert_types!(l_type, lhs.span, Type::Var, Type::Var)?;
                assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Assign
            }
            BinOp::ModEq | BinOp::OrEq | BinOp::AndEq => {
                assert_types!(l_type, lhs.span, Type::Var, Type::Var)?;
                assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                )?;
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
                    lhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                if l_type != Type::Var && r_type != Type::Var && l_type != r_type {
                    return Err(KalmarError::UnequalTypes(
                        lhs.span, l_type, rhs.span, r_type,
                    ));
                }
                Type::Boolean
            }
            BinOp::Assign => {
                assert_types!(l_type, lhs.span, Type::Var, Type::Var)?;
                assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                )?;
                Type::Assign
            }
            BinOp::Range => {
                assert_types!(l_type, lhs.span, Type::Integer, Type::Integer)?;
                assert_types!(r_type, rhs.span, Type::Integer, Type::Integer)?;
                Type::Range
            }
            BinOp::Comma => {
                assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::VarList,
                    Type::Var,
                    Type::VarList
                )?;
                assert_types!(r_type, rhs.span, Type::Var, Type::Var)?;
                Type::VarList
            }
            BinOp::Arrow => {
                assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::VarList,
                    Type::Var,
                    Type::VarList
                )?;
                assert_types!(r_type, rhs.span, Type::Identifier, Type::Identifier)?;
                Type::Assign
            }
        })
    }

    fn check_identifier_uniqueness(&self, ident: &'a str, declared: &[&'a str]) -> Option<&'a str> {
        if declared.contains(&ident) {
            return None;
        }

        Some(ident)
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
            panic!()
            //Err(KalmarError::UndeclaredReference(undeclared_references))
        }
    }
}
