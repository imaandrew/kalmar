use std::rc::Rc;

use crate::{
    error::{Error, ErrorKind},
    lexer::Literal,
    parser::{BinOp, Expr, Stmt, UnOp},
};

#[derive(Debug, PartialEq)]
enum Type {
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

pub struct SemChecker<'a> {
    declared_scripts: Vec<&'a String>,
    referenced_scripts: Vec<&'a String>,
    declared_labels: Vec<&'a String>,
    referenced_labels: Vec<&'a String>,
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
    pub fn check_ast(&mut self, ast: &'a Vec<Stmt>) -> Result<(), Error> {
        self.check_nodes(ast)?;
        self.verify_referenced_identifiers(&self.declared_scripts, &self.referenced_scripts);
        Ok(())
    }

    fn check_nodes(&mut self, stmts: &'a Vec<Stmt>) -> Result<(), Error> {
        for stmt in stmts {
            self.check_stmt_node(stmt)?;
        }
        Ok(())
    }

    fn check_stmt_node(&mut self, stmt: &'a Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Script(i, s) => {
                self.declared_labels.clear();
                self.referenced_labels.clear();
                let script = self
                    .check_identifier_uniqueness(i, &self.declared_scripts, || ())
                    .ok_or(Error::new(
                        stmt.get_token().as_ref().unwrap().loc,
                        ErrorKind::RedeclaredScr(Rc::clone(stmt.get_token().as_ref().unwrap())),
                    ))?;
                self.declared_scripts.push(script);
                self.check_stmt_node(s)?;
                self.verify_referenced_identifiers(&self.declared_labels, &self.referenced_labels);
            }
            Stmt::Block(s) => self.check_nodes(s)?,
            Stmt::Label(l) => self.declared_labels.push(
                self.check_identifier_uniqueness(l, &self.declared_labels, || {
                    if self.declared_labels.len() >= 16 {
                        panic!("Cannot have more than 16 labels per script");
                    }
                })
                .ok_or(Error::new(
                    stmt.get_token().as_ref().unwrap().loc,
                    ErrorKind::RedeclaredLabel(Rc::clone(stmt.get_token().as_ref().unwrap())),
                ))?,
            ),
            Stmt::Goto(l) => match l {
                Literal::Identifier(i) => {
                    if !self.referenced_labels.contains(&i) {
                        self.referenced_labels.push(i);
                    }
                }
                _ => panic!("Goto literal must be an identifier"),
            },
            Stmt::Loop(e, s) => {
                if let Some(e) = e {
                    assert_eq!(self.check_expr_node(e), Type::Integer);
                }
                self.check_stmt_node(s)?;
            }
            Stmt::IfElse(i, e) => {
                self.check_stmt_node(i)?;
                self.check_nodes(e)?;
            }
            Stmt::If(e, s) => {
                assert_eq!(self.check_expr_node(e), Type::Boolean);
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
                    if !self.referenced_scripts.contains(&i) {
                        self.referenced_scripts.push(i);
                    }
                }
                _ => panic!("Invalid jump target: {:?}", l),
            },
            Stmt::Thread(s) => self.check_stmt_node(s)?,
            Stmt::ChildThread(s) => self.check_stmt_node(s)?,
            Stmt::Expr(e) => {
                assert!(matches!(
                    self.check_expr_node(e),
                    Type::Assign | Type::FuncCall
                ));
            }
            Stmt::Switch(e, s) => {
                assert!(matches!(self.check_expr_node(e), Type::Integer | Type::Var));
                self.check_stmt_node(s)?;
            }
            Stmt::Case(e, s) => {
                self.check_expr_node(e);
                self.check_stmt_node(s)?;
            }
            Stmt::Return | Stmt::BreakCase | Stmt::BreakLoop | Stmt::Empty => (),
        };

        Ok(())
    }

    fn check_expr_node(&self, expr: &Expr) -> Type {
        match expr {
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
                assert_eq!(self.check_expr_node(e), Type::Integer);
                Type::Var
            }
            Expr::UnOp(op, expr) => self.check_unop_type(op, expr),
            Expr::BinOp(op, lhs, rhs) => self.check_binop_type(op, lhs, rhs),
            Expr::FuncCall(_, args) => {
                for e in args {
                    self.check_expr_node(e);
                }
                Type::FuncCall
            }
            Expr::ArrayAssign(_, e) => {
                assert!(matches!(self.check_expr_node(e), Type::Integer | Type::Var));
                Type::Assign
            }
            Expr::Default => Type::Case,
        }
    }

    fn check_unop_type(&self, op: &UnOp, expr: &Expr) -> Type {
        let t = self.check_expr_node(expr);
        match op {
            UnOp::Minus => {
                assert!(matches!(t, Type::Integer | Type::Float));
                t
            }
            UnOp::Bang => {
                assert_eq!(t, Type::Boolean);
                Type::Boolean
            }
            UnOp::Equal
            | UnOp::NotEqual
            | UnOp::Greater
            | UnOp::GreaterEq
            | UnOp::Less
            | UnOp::LessEq => {
                assert!(matches!(t, Type::Integer | Type::Float | Type::Var));
                Type::Case
            }
            UnOp::Ampersand => {
                assert!(matches!(t, Type::Var | Type::Integer));
                t
            }
        }
    }

    fn check_binop_type(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Type {
        let l_type = self.check_expr_node(lhs);
        let r_type = self.check_expr_node(rhs);
        match op {
            BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Div => {
                assert_eq!(l_type, r_type);
                matches!(l_type, Type::Integer | Type::Float);
                l_type
            }
            BinOp::Mod | BinOp::BitOr => {
                assert_eq!(l_type, r_type);
                assert_eq!(l_type, Type::Integer);
                l_type
            }
            BinOp::BitAnd => {
                assert!(matches!(l_type, Type::Var | Type::Integer));
                assert_eq!(r_type, Type::Integer);
                if l_type == Type::Var {
                    Type::Boolean
                } else {
                    Type::Integer
                }
            }
            BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::DivEq => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Float | Type::Var);
                Type::Assign
            }
            BinOp::ModEq | BinOp::OrEq | BinOp::AndEq => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Var);
                Type::Assign
            }
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Greater
            | BinOp::GreaterEq
            | BinOp::Less
            | BinOp::LessEq => {
                assert!(l_type == r_type || l_type == Type::Var || r_type == Type::Var);
                assert!(matches!(l_type, Type::Integer | Type::Float | Type::Var));
                assert!(matches!(r_type, Type::Integer | Type::Float | Type::Var));
                Type::Boolean
            }
            BinOp::Assign => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Float | Type::Var);
                Type::Assign
            }
            BinOp::Range => {
                assert_eq!(l_type, Type::Integer);
                assert_eq!(r_type, Type::Integer);
                Type::Range
            }
            BinOp::Comma => {
                assert!(matches!(l_type, Type::Var | Type::VarList));
                assert_eq!(r_type, Type::Var);
                Type::VarList
            }
            BinOp::Arrow => {
                assert!(matches!(l_type, Type::Var | Type::VarList));
                assert_eq!(r_type, Type::Identifier);
                Type::Assign
            }
        }
    }

    fn check_identifier_uniqueness<F>(
        &self,
        ident: &'a Literal,
        declared: &[&'a String],
        callback: F,
    ) -> Option<&'a String>
    where
        F: FnOnce(),
    {
        callback();

        let name = match ident {
            Literal::Identifier(i) => i,
            _ => unreachable!(),
        };

        if declared.contains(&name) {
            return None;
        }

        Some(name)
    }

    fn verify_referenced_identifiers(&self, declared: &[&'a String], referenced: &Vec<&'a String>) {
        let mut undeclared_references = vec![];
        for ident in referenced {
            if !declared.contains(ident) {
                undeclared_references.push(ident);
            }
        }

        if !undeclared_references.is_empty() {
            panic!("Undeclared references to: {:?}", undeclared_references);
        }
    }
}
