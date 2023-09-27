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

pub fn check_stmts(stmts: &mut Vec<Stmt>) {
    for stmt in stmts {
        check_stmt(stmt);
    }
}

fn check_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Script(_, s) => check_stmt(s),
        Stmt::Block(s) => check_stmts(s),
        Stmt::Label(_) => unimplemented!(),
        Stmt::Goto(_) => unimplemented!(),
        Stmt::Loop(e, s) => {
            check_expr(e);
            check_stmt(s);
        }
        Stmt::IfElse(i, e) => {
            check_stmt(i);
            check_stmts(e);
        }
        Stmt::If(e, s) => {
            check_expr(e);
            check_stmt(s);
        }
        Stmt::Else(i, b) => {
            assert_ne!(i.is_some(), b.is_some());
            if let Some(e) = i {
                check_stmt(e);
            }

            if let Some(e) = b {
                check_stmt(e);
            }
        }
        Stmt::Jump(_) => unimplemented!(),
        Stmt::Thread(s) => check_stmt(s),
        Stmt::ChildThread(s) => check_stmt(s),
        Stmt::Expr(e) => {
            check_expr(e);
        }
        Stmt::Switch(e, s) => check_stmt(s),
        Stmt::CaseStmt(e, s) => check_stmt(s),
        Stmt::Return | Stmt::BreakCase | Stmt::BreakLoop => (),
    };
}

fn check_expr(expr: &mut Expr) -> Type {
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
        Expr::Array(_, e) => check_expr(e),
        Expr::UnOp(op, expr) => check_unop_type(op, expr),
        Expr::BinOp(op, lhs, rhs) => check_binop_type(op, lhs, rhs),
        Expr::FuncCall(_, args) => {
            for e in args {
                check_expr(e);
            }
            Type::Empty
        }
        Expr::Default => Type::Empty,
    }
}

fn check_unop_type(op: &UnOp, expr: &mut Expr) -> Type {
    let t = check_expr(expr);
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

fn check_binop_type(op: &BinOp, lhs: &mut Expr, rhs: &mut Expr) -> Type {
    let l_type = check_expr(lhs);
    let r_type = check_expr(rhs);
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
