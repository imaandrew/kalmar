use crate::{
    lexer::Literal,
    parser::{BinOp, Expr, Stmt, UnOp},
};

pub fn optimize_stmts(stmts: &mut Vec<Stmt>) {
    for stmt in stmts {
        collapse_stmt(stmt);
    }
}

fn collapse_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Script(_, s) => collapse_stmt(s),
        Stmt::Block(s) => optimize_stmts(s),
        Stmt::Loop(e, s) => {
            collapse_expr(e);
            collapse_stmt(s);
        }
        Stmt::IfElse(i, e) => {
            collapse_stmt(i);
            optimize_stmts(e);
        }
        Stmt::If(e, s) => {
            collapse_expr(e);
            collapse_stmt(s);
        }
        Stmt::Else(Some(i), None) => collapse_stmt(i),
        Stmt::Else(None, Some(s)) => collapse_stmt(s),
        Stmt::Thread(s) => collapse_stmt(s),
        Stmt::ChildThread(s) => collapse_stmt(s),
        Stmt::Expr(e) => collapse_expr(e),
        Stmt::Switch(e, s) => {
            collapse_expr(e);
            collapse_stmt(s);
        }
        Stmt::CaseStmt(e, s) => {
            collapse_expr(e);
            collapse_stmt(s);
        }
        _ => (),
    }
}

macro_rules! generate_binops {
    ($expr:ident, $($binop:pat, $op:tt, $type:ident),*) => {
        match $expr {
            Expr::BinOp(op, l, r) => match op {
                $(
                    $binop => match (l.as_ref(), r.as_ref()) {
                        (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                            *$expr = Expr::Identifier(Literal::$type(*x $op *y));
                        }
                        _ => (),
                    }
                )*
                _ => (),
            },
            _ => (),
        }
    };
}

fn collapse_expr(expr: &mut Expr) {
    generate_binops!(
        expr,
        BinOp::Plus, +, Number,
        BinOp::Minus, -, Number,
        BinOp::Star, *, Number,
        BinOp::Div, /, Number,
        BinOp::Mod, %, Number,
        BinOp::BitAnd, &, Number,
        BinOp::Equal, ==, Boolean,
        BinOp::NotEqual, !=, Boolean,
        BinOp::Greater, >, Boolean,
        BinOp::GreaterEq, >=, Boolean,
        BinOp::Less, <, Boolean,
        BinOp::LessEq, <=, Boolean
    );

    match expr {
        Expr::UnOp(op, e) => match op {
            UnOp::Minus => match &mut **e {
                Expr::Identifier(Literal::Number(n)) => {
                    *expr = Expr::Identifier(Literal::Number(-*n))
                }
                _ => {
                    collapse_expr(e);
                }
            },
            _ => {
                collapse_expr(e);
            }
        },
        Expr::FuncCall(_, a) => {
            for mut arg in a {
                collapse_expr(&mut arg);
            }
        }
        Expr::Array(_, idx) => {
            collapse_expr(idx);
        }
        _ => (),
    };
}
