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
            Some($pattern) | None => Ok(()),
            Some(t) => Err(KalmarError::InvalidType($span, $x, t)),
        }
    }};
    ($lhs:expr, $span:expr, $pattern:pat, $($x:expr),+) => {{
        match $lhs {
            Some($pattern) | None => Ok(()),
            Some(t) => Err(KalmarError::InvalidTypes($span, vec![$($x),+], t)),
        }
    }};
}

pub struct SemChecker<'a> {
    declared_scripts: Vec<&'a str>,
    referenced_scripts: Vec<Token>,
    declared_labels: Vec<&'a str>,
    referenced_labels: Vec<Token>,
    literals: &'a StringManager<'a>,
    errors: Vec<KalmarError>,
}

impl<'a> SemChecker<'a> {
    pub fn new(literals: &'a mut StringManager) -> Self {
        Self {
            declared_scripts: vec![],
            referenced_scripts: vec![],
            declared_labels: vec![],
            referenced_labels: vec![],
            literals,
            errors: vec![],
        }
    }
}

impl<'a> SemChecker<'a> {
    pub fn check_ast(&mut self, ast: &'a [Stmt]) -> Result<(), Vec<KalmarError>> {
        self.check_nodes(ast);
        self.verify_referenced_identifiers(&self.declared_scripts, &self.referenced_scripts);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_nodes(&mut self, stmts: &'a [Stmt]) {
        for stmt in stmts {
            self.check_stmt_node(stmt);
        }
    }

    fn check_stmt_node(&mut self, stmt: &'a Stmt) {
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
                let script = self.check_identifier_uniqueness(
                    self.literals.get(*i).unwrap(),
                    &self.declared_scripts,
                );
                match script {
                    Some(script) => self.declared_scripts.push(script),
                    None => self.errors.push(KalmarError::RedeclaredScript(*t)),
                }
                self.check_stmt_node(s);
                let mut errors = self
                    .verify_referenced_identifiers(&self.declared_labels, &self.referenced_labels);
                self.errors.append(&mut errors);
            }
            Stmt::Block(s) => self.check_nodes(s),
            Stmt::Label(
                t @ Token {
                    val: Some(Literal::Identifier(i)),
                    ..
                },
            ) => {
                if self.declared_labels.len() >= 16 {
                    self.errors.push(KalmarError::TooManyLabels(*t));
                }

                match self.check_identifier_uniqueness(
                    self.literals.get(*i).unwrap(),
                    &self.declared_labels,
                ) {
                    Some(s) => self.declared_labels.push(s),
                    None => self.errors.push(KalmarError::RedeclaredLabel(*t)),
                }
            }
            Stmt::Goto(
                t @ Token {
                    val: Some(Literal::Identifier(i)),
                    ..
                },
            ) => {
                for t in &self.referenced_labels {
                    match t.val {
                        Some(Literal::Identifier(i2)) if *i == i2 => return,
                        _ => (),
                    }
                }
                self.referenced_labels.push(*t);
            }
            Stmt::Loop(e, s) => {
                if let Some(e) = e {
                    let t = self.check_expr_node(e);
                    self.push_err(assert_types!(t, e.span, Type::Integer, Type::Integer));
                }
                self.check_stmt_node(s);
            }
            Stmt::IfElse(i, e) => {
                self.check_stmt_node(i);
                if let Some(e) = e {
                    self.check_stmt_node(e);
                }
            }
            Stmt::If(e, s) => {
                let t = self.check_expr_node(e);
                self.push_err(assert_types!(t, e.span, Type::Boolean, Type::Boolean));
                self.check_stmt_node(s);
            }
            Stmt::Else(i, b) => {
                assert_ne!(i.is_some(), b.is_some());
                if let Some(e) = i {
                    self.check_stmt_node(e);
                }

                if let Some(e) = b {
                    self.check_stmt_node(e);
                }
            }
            Stmt::Jump(
                t @ Token {
                    val: Some(Literal::Identifier(i)),
                    ..
                },
            ) => {
                for t in &self.referenced_scripts {
                    match t.val {
                        Some(Literal::Identifier(i2)) if *i == i2 => return,
                        _ => (),
                    }
                }
                self.referenced_scripts.push(*t);
            }
            Stmt::Thread(s) => self.check_stmt_node(s),
            Stmt::ChildThread(s) => self.check_stmt_node(s),
            Stmt::Expr(e) => {
                let t = self.check_expr_node(e);
                self.push_err(assert_types!(
                    t,
                    e.span,
                    Type::Assign | Type::FuncCall,
                    Type::Assign,
                    Type::FuncCall
                ));
            }
            Stmt::Switch(e, s) => {
                let t = self.check_expr_node(e);
                self.push_err(assert_types!(
                    t,
                    e.span,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                ));
                self.check_stmt_node(s);
            }
            Stmt::Case(e, s) => {
                let t = self.check_expr_node(e);
                self.push_err(assert_types!(t, e.span, Type::Case, Type::Case));
                self.check_stmt_node(s);
            }
            Stmt::Return | Stmt::BreakCase | Stmt::BreakLoop | Stmt::Empty => (),
            _ => unreachable!(),
        }
    }

    fn check_expr_node(&mut self, expr: &Expr) -> Option<Type> {
        Some(match &expr.kind {
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
                let t = assert_types!(
                    self.check_expr_node(e),
                    e.span,
                    Type::Integer,
                    Type::Integer
                );
                self.push_err(t);
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
                let t = assert_types!(
                    self.check_expr_node(e),
                    e.span,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                );
                self.push_err(t);
                Type::Assign
            }
            ExprKind::Default => Type::Case,
        })
    }

    fn check_unop_type(&mut self, op: &UnOp, expr: &Expr) -> Option<Type> {
        let t = self.check_expr_node(expr);
        match op {
            UnOp::Minus => {
                self.push_err(assert_types!(
                    t,
                    expr.span,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                ));
                t
            }
            UnOp::Bang => {
                self.push_err(assert_types!(t, expr.span, Type::Boolean, Type::Boolean))?;
                t
            }
            UnOp::Equal
            | UnOp::NotEqual
            | UnOp::Greater
            | UnOp::GreaterEq
            | UnOp::Less
            | UnOp::LessEq => {
                self.push_err(assert_types!(
                    t,
                    expr.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                ))?;
                Some(Type::Case)
            }
            UnOp::Ampersand => {
                self.push_err(assert_types!(
                    t,
                    expr.span,
                    Type::Var | Type::Integer,
                    Type::Var,
                    Type::Integer
                ));
                t
            }
        }
    }

    fn check_binop_type(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<Type> {
        let l_type = self.check_expr_node(lhs);
        let r_type = self.check_expr_node(rhs);
        match op {
            BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Div => {
                let num_errs = self.errors.len();

                self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                ));

                self.push_err(assert_types!(
                    r_type,
                    lhs.span,
                    Type::Integer | Type::Float,
                    Type::Integer,
                    Type::Float
                ));

                if l_type.unwrap() != r_type.unwrap() {
                    self.errors.push(KalmarError::UnequalTypes(
                        lhs.span,
                        l_type.unwrap(),
                        rhs.span,
                        r_type.unwrap(),
                    ));
                }

                if self.errors.len() > num_errs {
                    None
                } else {
                    l_type
                }
            }
            BinOp::Mod | BinOp::BitOr => {
                let l_err = self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Integer,
                    Type::Integer
                ));
                let r_err = self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer,
                    Type::Integer
                ));

                if l_err.is_none() || r_err.is_none() {
                    None
                } else {
                    l_type
                }
            }
            BinOp::BitAnd => {
                let l_err = self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::Integer,
                    Type::Var,
                    Type::Integer
                ));
                let r_err = self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer,
                    Type::Integer
                ));

                if l_err.is_none() || r_err.is_none() {
                    return None;
                }

                Some(if l_type.unwrap() == Type::Var {
                    Type::Boolean
                } else {
                    Type::Integer
                })
            }
            BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::DivEq => {
                self.push_err(assert_types!(l_type, lhs.span, Type::Var, Type::Var));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                ));
                Some(Type::Assign)
            }
            BinOp::ModEq | BinOp::OrEq | BinOp::AndEq => {
                self.push_err(assert_types!(l_type, lhs.span, Type::Var, Type::Var));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Var,
                    Type::Integer,
                    Type::Var
                ));
                Some(Type::Assign)
            }
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Greater
            | BinOp::GreaterEq
            | BinOp::Less
            | BinOp::LessEq => {
                self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                ));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                ));
                if l_type.unwrap() != Type::Var && r_type.unwrap() != Type::Var && l_type != r_type
                {
                    self.errors.push(KalmarError::UnequalTypes(
                        lhs.span,
                        l_type.unwrap(),
                        rhs.span,
                        r_type.unwrap(),
                    ));
                }
                Some(Type::Boolean)
            }
            BinOp::Assign => {
                self.push_err(assert_types!(l_type, lhs.span, Type::Var, Type::Var));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer | Type::Float | Type::Var,
                    Type::Integer,
                    Type::Float,
                    Type::Var
                ));
                Some(Type::Assign)
            }
            BinOp::Range => {
                self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Integer,
                    Type::Integer
                ));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Integer,
                    Type::Integer
                ));
                Some(Type::Range)
            }
            BinOp::Comma => {
                self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::VarList,
                    Type::Var,
                    Type::VarList
                ));
                self.push_err(assert_types!(r_type, rhs.span, Type::Var, Type::Var));
                Some(Type::VarList)
            }
            BinOp::Arrow => {
                self.push_err(assert_types!(
                    l_type,
                    lhs.span,
                    Type::Var | Type::VarList,
                    Type::Var,
                    Type::VarList
                ));
                self.push_err(assert_types!(
                    r_type,
                    rhs.span,
                    Type::Identifier,
                    Type::Identifier
                ));
                Some(Type::Assign)
            }
        }
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
        referenced: &Vec<Token>,
    ) -> Vec<KalmarError> {
        let mut errors = vec![];
        for t in referenced {
            if let Token {
                val: Some(Literal::Identifier(i)),
                ..
            } = t
            {
                if !declared.contains(&self.literals.get(*i).unwrap()) {
                    errors.push(KalmarError::UndeclaredIdentifier(*t));
                }
            }
        }
        errors
    }

    fn push_err(&mut self, x: Result<(), KalmarError>) -> Option<()> {
        if let Err(e) = x {
            self.errors.push(e);
            None
        } else {
            Some(())
        }
    }
}
