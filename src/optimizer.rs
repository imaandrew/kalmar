use crate::{
    lexer::Literal,
    parser::{BinOp, Expr, Stmt, UnOp},
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
    match &expr {
        Expr::UnOp(op, e) => match op {
            UnOp::Minus => match **e {
                Expr::Identifier(Literal::Number(n)) => Expr::Identifier(Literal::Number(-n)),
                _ => Expr::UnOp(*op, Box::new(collapse_expr(**e))),
            },
            _ => Expr::UnOp(*op, Box::new(collapse_expr(**e))),
        },
        Expr::BinOp(op, l, r) => match op {
            BinOp::Plus => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x + *y))
                }
                _ => expr,
            },
            BinOp::Minus => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x - *y))
                }
                _ => expr,
            },
            BinOp::Star => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x * *y))
                }
                _ => expr,
            },
            BinOp::Div => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x / *y))
                }
                _ => expr,
            },
            BinOp::Equal => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x == y))
                }
                _ => expr,
            },
            BinOp::NotEqual => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x != y))
                }
                _ => expr,
            },
            BinOp::Greater => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x > y))
                }
                _ => expr,
            },
            BinOp::GreaterEq => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x >= y))
                }
                _ => expr,
            },
            BinOp::Less => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x > y))
                }
                _ => expr,
            },
            BinOp::LessEq => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Boolean(x <= y))
                }
                _ => expr,
            },
            BinOp::Mod => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x % *y))
                }
                _ => expr,
            },
            BinOp::BitAnd => match (l.as_ref(), r.as_ref()) {
                (Expr::Identifier(Literal::Number(x)), Expr::Identifier(Literal::Number(y))) => {
                    Expr::Identifier(Literal::Number(*x & *y))
                }
                _ => expr,
            },
            _ => expr,
        },
        Expr::FuncCall(f, a) => {
            let mut args = vec![];

            for arg in a {
                args.push(collapse_expr(*arg));
            }

            Expr::FuncCall(f.clone(), args)
        }
        Expr::Var(ident, idx) => {
            Expr::Var(*ident, Box::new(collapse_expr(**idx)))
        }
        _ => expr,
    }
}
