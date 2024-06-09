use crate::{
    lexer::Literal,
    lexer::{Span, Token, TokenKind},
    parser::{BinOp, Expr, ExprKind, Stmt, UnOp},
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
        match &mut $expr.kind {
            ExprKind::BinOp(op, l, r) => {
                fold_expr_op(l);
                fold_expr_op(r);
                match op {
                $(
                    BinOp::$binop => match (&l.kind, &r.kind) {
                        (ExprKind::Identifier(Token { val: Some(Literal::Number(x)), span: Span {line, col, ..}, .. }), ExprKind::Identifier(Token { val: Some(Literal::Number(y)), span: Span {len: len2, col: col2, ..}, .. })) => {
                            let lit = Literal::$type(*x $op *y);
                            let token = Token {
                                kind: TokenKind::Number,
                                val: Some(lit),
                                span: Span::new(*line, *col, col2 + len2 - col)
                            };
                            *$expr = Expr {
                                kind: ExprKind::Identifier(token),
                                span: Span::new(*line, *col, token.span.len)
                            };
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

    match &mut expr.kind {
        ExprKind::UnOp(op, e) => match op {
            UnOp::Minus => match e.kind {
                ExprKind::Identifier(Token {
                    val: Some(Literal::Number(n)),
                    span: Span { len, col, .. },
                    ..
                }) => {
                    let lit = Literal::Number(-n);
                    let token = Token {
                        kind: TokenKind::Number,
                        val: Some(lit),
                        span: Span::new(expr.span.line, expr.span.col, col + len - expr.span.col),
                    };
                    expr.kind = ExprKind::Identifier(token);
                    expr.span.len = token.span.len;
                }
                _ => {
                    fold_expr_op(e);
                }
            },
            _ => {
                fold_expr_op(e);
            }
        },
        ExprKind::FuncCall(_, a) => {
            for arg in a {
                fold_expr_op(arg);
            }
        }
        ExprKind::Array(_, idx) => {
            fold_expr_op(idx);
        }
        _ => (),
    };
}
