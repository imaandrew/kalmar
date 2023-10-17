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
            fold_expr_op(e);
            collapse_stmt(s);
        }
        Stmt::IfElse(i, e) => {
            collapse_stmt(i);
            optimize_stmts(e);
        }
        Stmt::If(e, s) => {
            fold_expr_op(e);
            collapse_stmt(s);
        }
        Stmt::Else(Some(i), None) => collapse_stmt(i),
        Stmt::Else(None, Some(s)) => collapse_stmt(s),
        Stmt::Thread(s) => collapse_stmt(s),
        Stmt::ChildThread(s) => collapse_stmt(s),
        Stmt::Expr(e) => fold_expr_op(e),
        Stmt::Switch(e, s) => {
            fold_expr_op(e);
            collapse_stmt(s);
        }
        Stmt::CaseStmt(e, s) => {
            fold_expr_op(e);
            collapse_stmt(s);
        }
        _ => (),
    }
}

macro_rules! generate_binops {
    ($expr:ident, $($binop:ident, $op:tt, $type:ident),*) => {
        match $expr {
            Expr::BinOp(op, l, r) => {
                fold_expr_op(l);
                fold_expr_op(r);
                match op {
                $(
                    BinOp::$binop => match (l.as_ref(), r.as_ref()) {
                        (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                            *$expr = Expr::Identifier(Literal::$type(*x $op *y));
                        }
                        _ => (),
                    }
                )*
                _ => (),
            }},
            _ => (),
        }
    };
}

fn fold_expr_op(expr: &mut Expr) {
    generate_binops!(
        expr,
        Plus, +, Number,
        Minus, -, Number,
        Star, *, Number,
        Div, /, Number,
        Mod, %, Number,
        BitAnd, &, Number,
        Equal, ==, Boolean,
        NotEqual, !=, Boolean,
        Greater, >, Boolean,
        GreaterEq, >=, Boolean,
        Less, <, Boolean,
        LessEq, <=, Boolean
    );

    match expr {
        Expr::UnOp(op, e) => match op {
            UnOp::Minus => match &mut **e {
                Expr::Identifier(Literal::Number(n)) => {
                    *expr = Expr::Identifier(Literal::Number(-*n))
                }
                _ => {
                    fold_expr_op(e);
                }
            },
            _ => {
                fold_expr_op(e);
            }
        },
        Expr::FuncCall(_, a) => {
            for arg in a {
                fold_expr_op(arg);
            }
        }
        Expr::Array(_, idx) => {
            fold_expr_op(idx);
        }
        _ => (),
    };
}
