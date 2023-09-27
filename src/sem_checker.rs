use crate::{
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
    CaseExpr,
    Empty,
}

pub struct SemChecker<'a> {
    declared_scripts: Vec<&'a String>,
    referenced_scripts: Vec<&'a String>,
    declared_labels: Vec<&'a String>,
    referenced_labels: Vec<&'a String>,
}

impl<'a> SemChecker<'a> {
    pub fn check_stmts(&mut self, stmts: &'a Vec<Stmt>) {
        for stmt in stmts {
            self.check_stmt(stmt);
        }

        self.verify_referenced_identifiers(&self.declared_scripts, &self.referenced_scripts);
        self.verify_referenced_identifiers(&self.declared_labels, &self.referenced_labels);
    }

    fn check_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Script(i, s) => {
                self.check_identifier_uniqueness(i, &self.declared_scripts, || ());
                self.check_stmt(s);
            }
            Stmt::Block(s) => self.check_stmts(s),
            Stmt::Label(l) => self.check_identifier_uniqueness(l, &self.declared_labels, || {
                if self.declared_labels.len() >= 16 {
                    panic!("Cannot have more than 16 labels per script");
                }
            }),
            Stmt::Goto(l) => match l {
                Literal::Identifier(i) => {
                    if !self.referenced_labels.contains(&i) {
                        self.referenced_labels.push(i);
                    }
                }
                _ => panic!(),
            },
            Stmt::Loop(e, s) => {
                self.check_expr(e);
                self.check_stmt(s);
            }
            Stmt::IfElse(i, e) => {
                self.check_stmt(i);
                self.check_stmts(e);
            }
            Stmt::If(e, s) => {
                self.check_expr(e);
                self.check_stmt(s);
            }
            Stmt::Else(i, b) => {
                assert_ne!(i.is_some(), b.is_some());
                if let Some(e) = i {
                    self.check_stmt(e);
                }

                if let Some(e) = b {
                    self.check_stmt(e);
                }
            }
            Stmt::Jump(_) => unimplemented!(),
            Stmt::Thread(s) => self.check_stmt(s),
            Stmt::ChildThread(s) => self.check_stmt(s),
            Stmt::Expr(e) => {
                self.check_expr(e);
            }
            Stmt::Switch(e, s) => self.check_stmt(s),
            Stmt::CaseStmt(e, s) => self.check_stmt(s),
            Stmt::Return | Stmt::BreakCase | Stmt::BreakLoop => (),
        };
    }

    fn check_expr(&self, expr: &Expr) -> Type {
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
                _ => unimplemented!(),
            },
            Expr::Array(_, e) => self.check_expr(e),
            Expr::UnOp(op, expr) => self.check_unop_type(op, expr),
            Expr::BinOp(op, lhs, rhs) => self.check_binop_type(op, lhs, rhs),
            Expr::FuncCall(_, args) => {
                for e in args {
                    self.check_expr(e);
                }
                Type::Empty
            }
            Expr::Default => Type::Empty,
        }
    }

    fn check_unop_type(&self, op: &UnOp, expr: &Expr) -> Type {
        let t = self.check_expr(expr);
        match op {
            UnOp::Minus => {
                matches!(t, Type::Integer | Type::Float);
                t
            }
            UnOp::Bang => {
                assert_eq!(t, Type::Boolean);
                Type::Boolean
            }
            UnOp::Equal => {
                matches!(t, Type::Integer | Type::Float | Type::Var | Type::CaseExpr);
                Type::Empty
            }
            UnOp::NotEqual | UnOp::Greater | UnOp::GreaterEq | UnOp::Less | UnOp::LessEq => {
                matches!(t, Type::Integer | Type::Float | Type::Var);
                Type::Empty
            }
            UnOp::Ampersand => {
                unimplemented!()
            }
            UnOp::New => {
                unimplemented!()
            }
        }
    }

    fn check_binop_type(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Type {
        let l_type = self.check_expr(lhs);
        let r_type = self.check_expr(rhs);
        match op {
            BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Div => {
                assert_eq!(l_type, r_type);
                matches!(l_type, Type::Integer | Type::Float);
                l_type
            }
            BinOp::Mod | BinOp::BitOr | BinOp::BitAnd => {
                assert_eq!(l_type, r_type);
                assert_eq!(l_type, Type::Integer);
                l_type
            }
            BinOp::PlusEq | BinOp::MinusEq | BinOp::StarEq | BinOp::DivEq => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Float | Type::Var);
                Type::Empty
            }
            BinOp::ModEq | BinOp::OrEq | BinOp::AndEq => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Var);
                Type::Empty
            }
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Greater
            | BinOp::GreaterEq
            | BinOp::Less
            | BinOp::LessEq => {
                assert_eq!(l_type, r_type);
                matches!(l_type, Type::Integer | Type::Float);
                Type::Boolean
            }
            BinOp::Assign => {
                assert_eq!(l_type, Type::Var);
                matches!(r_type, Type::Integer | Type::Float | Type::Var);
                Type::Empty
            }
            BinOp::Range => {
                assert_eq!(l_type, Type::Integer);
                assert_eq!(r_type, Type::Integer);
                Type::Range
            }
            BinOp::Or | BinOp::And => {
                matches!(l_type, Type::Integer | Type::Float | Type::CaseExpr);
                matches!(r_type, Type::Integer | Type::Float | Type::CaseExpr);
                Type::CaseExpr
            }
        }
    }

    fn check_identifier_uniqueness<F>(
        &mut self,
        ident: &'a Literal,
        declared: &Vec<&'a String>,
        callback: F,
    ) where
        F: FnOnce() -> (),
    {
        callback();

        let name = match ident {
            Literal::Identifier(i) => i,
            _ => panic!(),
        };

        if declared.contains(&name) {
            panic!("Script {} redeclared", name);
        }

        declared.push(name);
    }

    fn verify_referenced_identifiers(
        &self,
        declared: &Vec<&'a String>,
        referenced: &Vec<&'a String>,
    ) {
        let mut undeclared_references = vec![];
        for ident in referenced {
            if !declared.contains(ident) {
                undeclared_references.push(ident);
            }
        }

        if !undeclared_references.is_empty() {
            panic!("{:?}", undeclared_references);
        }
    }
}
