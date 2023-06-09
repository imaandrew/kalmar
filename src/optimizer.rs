use crate::{
    lexer::Literal,
    parser::{BinOp, Expr, ExprEnum, Stmt, UnOp},
};

pub fn optimize_stmts(stmts: Vec<Stmt>) -> Vec<Stmt> {
    let mut s = vec![];
    for stmt in stmts {
        s.push(collapse_stmt(stmt));
    }
    s
}

fn collapse_stmt(stmt: Stmt) -> Stmt {
    match stmt {
        Stmt::Script(n, s) => Stmt::Script(n, Box::new(collapse_stmt(*s))),
        Stmt::Block(s) => Stmt::Block(optimize_stmts(s)),
        Stmt::Loop(e, s) => Stmt::Loop(collapse_expr(e), Box::new(collapse_stmt(*s))),
        Stmt::IfElse(i, e) => Stmt::IfElse(Box::new(collapse_stmt(*i)), optimize_stmts(e)),
        Stmt::If(e, s) => Stmt::If(collapse_expr(e), Box::new(collapse_stmt(*s))),
        Stmt::Else(Some(i), None) => Stmt::Else(Some(Box::new(collapse_stmt(*i))), None),
        Stmt::Else(None, Some(s)) => Stmt::Else(None, Some(Box::new(collapse_stmt(*s)))),
        Stmt::Thread(s) => Stmt::Thread(Box::new(collapse_stmt(*s))),
        Stmt::ChildThread(s) => Stmt::ChildThread(Box::new(collapse_stmt(*s))),
        Stmt::Expr(e) => Stmt::Expr(collapse_expr(e)),
        Stmt::Switch(e, s) => Stmt::Switch(collapse_expr(e), Box::new(collapse_stmt(*s))),
        Stmt::CaseStmt(e, s) => Stmt::CaseStmt(collapse_expr(e), Box::new(collapse_stmt(*s))),
        s => s,
    }
}

fn collapse_expr(expr: Expr) -> Expr {
    let e = match &expr.expr {
        ExprEnum::UnOp(op, e) => match op {
            UnOp::Minus => match &e.expr {
                ExprEnum::Identifier(Literal::Number(n)) => {
                    ExprEnum::Identifier(Literal::Number(-*n))
                }
                _ => expr.expr,
            },
            UnOp::EqEq
            | UnOp::BangEq
            | UnOp::Greater
            | UnOp::GreaterEq
            | UnOp::Less
            | UnOp::LessEq
            | UnOp::Addr => ExprEnum::UnOp(*op, Box::new(collapse_expr(*e.clone()))),
            UnOp::Bang => expr.expr,
        },
        ExprEnum::BinOp(op, l, r) => match op {
            BinOp::Plus => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x + *y)),
                _ => expr.expr,
            },
            BinOp::Minus => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x - *y)),
                _ => expr.expr,
            },
            BinOp::Star => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x * *y)),
                _ => expr.expr,
            },
            BinOp::Slash => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x / *y)),
                _ => expr.expr,
            },
            BinOp::EqEq => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x == y)),
                _ => expr.expr,
            },
            BinOp::BangEq => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x != y)),
                _ => expr.expr,
            },
            BinOp::Greater => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x > y)),
                _ => expr.expr,
            },
            BinOp::GreaterEq => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x >= y)),
                _ => expr.expr,
            },
            BinOp::Less => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x > y)),
                _ => expr.expr,
            },
            BinOp::LessEq => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Boolean(x <= y)),
                _ => expr.expr,
            },
            BinOp::Percent => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x % *y)),
                _ => expr.expr,
            },
            BinOp::And => match (&l.expr, &r.expr) {
                (
                    ExprEnum::Identifier(Literal::Number(x)),
                    ExprEnum::Identifier(Literal::Number(y)),
                ) => ExprEnum::Identifier(Literal::Number(*x & *y)),
                _ => expr.expr,
            },
            _ => expr.expr,
        },
        ExprEnum::FuncCall(f, a) => {
            let mut args = vec![];

            for arg in a {
                args.push(collapse_expr(arg.clone()));
            }

            ExprEnum::FuncCall(f.clone(), args)
        }
        ExprEnum::Array(ident, index) => {
            ExprEnum::Array(ident.clone(), Box::new(collapse_expr(*index.clone())))
        }
        _ => expr.expr,
    };

    Expr {
        expr: e,
        ty: expr.ty,
    }
}
