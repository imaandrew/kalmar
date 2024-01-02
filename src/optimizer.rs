use crate::{
    lexer::Literal,
    parser::{ASTNode, BinOp, Expr, Stmt, UnOp},
};

use std::rc::Rc;

pub fn optimize_ast(stmts: &mut [ASTNode]) {
    for stmt in stmts {
        collapse_stmt_node(stmt);
        fold_redundant_blocks(stmt);
    }
}

fn collapse_stmt_node(stmt: &mut ASTNode) {
    let stmt = stmt.get_mut_stmt();
    match stmt {
        Stmt::Script(_, s) => collapse_stmt_node(s),
        Stmt::Block(s) => optimize_ast(s),
        Stmt::Loop(e, s) => {
            if let Some(e) = e {
                fold_expr_op(e);
            }
            collapse_stmt_node(s);
        }
        Stmt::IfElse(i, e) => {
            collapse_stmt_node(i);
            optimize_ast(e);

            try_elim_if_else_stmt(stmt);
        }
        Stmt::If(e, s) => {
            fold_expr_op(e);
            collapse_stmt_node(s);
        }
        Stmt::Else(Some(i), None) => collapse_stmt_node(i),
        Stmt::Else(None, Some(s)) => collapse_stmt_node(s),
        Stmt::Thread(s) => collapse_stmt_node(s),
        Stmt::ChildThread(s) => collapse_stmt_node(s),
        Stmt::Expr(e) => fold_expr_op(e),
        Stmt::Switch(e, s) => {
            fold_expr_op(e);
            collapse_stmt_node(s);
        }
        Stmt::Case(e, s) => {
            fold_expr_op(e);
            collapse_stmt_node(s);
        }
        _ => (),
    }
}

fn try_elim_if_else_stmt(stmt: &mut Stmt) {
    let (i, e) = match stmt {
        Stmt::IfElse(i, e) => (i, e),
        _ => unreachable!(),
    };

    fn try_elim_if_stmt(stmt: &mut ASTNode) -> bool {
        match stmt.get_mut_stmt() {
            Stmt::If(e, s) if matches!(e.get_expr().get_literal(), Some(Literal::Boolean(_))) => {
                if let Some(Literal::Boolean(b)) = e.get_expr().get_literal() {
                    if *b {
                        *stmt = std::mem::take(&mut *s);
                        return true;
                    }
                    return false;
                }
                true
            }
            Stmt::If(_, _) => true,
            _ => unreachable!(),
        }
    }

    if try_elim_if_stmt(i) {
        return;
    }

    for stmt in e {
        match stmt.get_mut_stmt() {
            Stmt::Else(Some(s), _) => {
                if try_elim_if_stmt(s) {
                    return;
                }
            }
            Stmt::Else(_, Some(s)) => {
                *stmt = std::mem::take(s);
                return;
            }
            _ => unreachable!(),
        }
    }

    *stmt = Stmt::Empty;
}

fn fold_redundant_blocks(stmt: &mut ASTNode) {
    match stmt.get_mut_stmt() {
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
                if let Stmt::Block(ss) = ss.get_mut_stmt() {
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
        let expr = $expr.get_mut_expr();
        match expr {
            Expr::BinOp(op, l, r) => {
                fold_expr_op(l);
                fold_expr_op(r);
                match op {
                $(
                    BinOp::$binop => match (l.get_expr().get_literal(), r.get_expr().get_literal()) {
                        (Some(Literal::Number(x)), Some(Literal::Number(y))) => {
                            *expr = Expr::Identifier(Rc::new(Literal::$type(*x $op *y)));
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

fn fold_expr_op(expr: &mut ASTNode) {
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

    let expr = expr.get_mut_expr();
    match expr {
        Expr::UnOp(op, e) => match op {
            UnOp::Minus => match e.get_expr().get_literal() {
                Some(Literal::Number(n)) => *expr = Expr::Identifier(Rc::new(Literal::Number(-*n))),
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
