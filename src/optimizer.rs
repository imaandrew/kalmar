use crate::{
    lexer::Literal,
    lexer::{Token, TokenKind},
    parser::{BinOp, Expr, Stmt, UnOp},
};

pub fn optimize_stmts(stmts: &mut [Stmt]) {
    for stmt in stmts {
        collapse_stmt(stmt);
        fold_redundant_blocks(stmt);
    }
}

fn collapse_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Script(_, s) => collapse_stmt(s),
        Stmt::Block(s) => optimize_stmts(s),
        Stmt::Loop(e, s) => {
            if let Some(e) = e {
                fold_expr_op(e);
            }
            collapse_stmt(s);
        }
        Stmt::IfElse(i, e) => {
            collapse_stmt(i);
            if let Some(e) = e {
                collapse_stmt(e);
            }
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
        Stmt::Case(e, s) => {
            fold_expr_op(e);
            collapse_stmt(s);
        }
        _ => (),
    }
}

fn fold_redundant_blocks(stmt: &mut Stmt) {
    match stmt {
        Stmt::Script(_, s)
        | Stmt::Loop(_, s)
        | Stmt::If(_, s)
        | Stmt::Else(Some(s), None)
        | Stmt::Else(None, Some(s))
        | Stmt::Thread(s)
        | Stmt::ChildThread(s)
        | Stmt::Switch(_, s)
        | Stmt::Case(_, s) => fold_redundant_blocks(s),
        Stmt::Block(s) => {
            for i in 0..s.len() {
                let ss = s.get_mut(i).unwrap();
                fold_redundant_blocks(ss);
                if let Stmt::Block(ss) = ss {
                    let mut ss = std::mem::take(ss);
                    s.remove(i);
                    s.append(&mut ss);
                }
            }
        }
        Stmt::IfElse(i, e) => {
            fold_redundant_blocks(i);
            for i in e {
                fold_redundant_blocks(i);
            }
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
                        (Expr::Identifier(Token { val: Some(Literal::Number(x)), line, col, pos: pos1, .. }), Expr::Identifier(Token { val: Some(Literal::Number(y)), pos: pos2, len: len2, .. })) => {
                            let lit = Literal::$type(*x $op *y);
                            let token = Token {
                                kind: TokenKind::Number,
                                val: Some(lit),
                                pos: *pos1,
                                line: *line,
                                col: *col,
                                len: (*len2 + *pos2) - *pos1,
                            };
                            *$expr = Expr::Identifier(token);
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
                Expr::Identifier(Token {
                    val: Some(Literal::Number(n)),
                    pos,
                    col,
                    line,
                    len,
                    ..
                }) => {
                    let lit = Literal::Number(-*n);
                    let token = Token {
                        kind: TokenKind::Number,
                        val: Some(lit),
                        pos: *pos - 1,
                        col: *col,
                        line: *line,
                        len: *len + 1,
                    };
                    *expr = Expr::Identifier(token)
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
