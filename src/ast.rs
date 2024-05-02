use itertools::Itertools;
use std::fmt::{Debug, Error, Formatter};

pub enum Expression {
    Concatenate(Vec<Box<Expression>>),
    Expansion(String),
    Indirection(Box<Expression>),
    PythonExpansion(String),
    Literal(String),
}

impl Debug for Expression {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expression::*;
        match *self {
            Concatenate(ref exprs) => {
                write!(
                    fmt,
                    "Concatenate({})",
                    exprs.into_iter().map(|v| format!("{:?}", v)).join(", ")
                )
            }
            Expansion(ref var) => write!(fmt, "GetVar({})", var),
            Indirection(ref expr) => write!(fmt, "Indirection({:?})", expr),
            PythonExpansion(ref p) => write!(fmt, "Python({})", p),
            Literal(ref s) => write!(fmt, "Literal('{}')", s),
        }
    }
}
//
// impl<'input> Debug for ExprSymbol<'input> {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::ExprSymbol::*;
//         match *self {
//             NumSymbol(n) => write!(fmt, "{:?}", n),
//             Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
//             Error => write!(fmt, "error"),
//         }
//     }
// }
//
// impl Debug for Opcode {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::Opcode::*;
//         match *self {
//             Mul => write!(fmt, "*"),
//             Div => write!(fmt, "/"),
//             Add => write!(fmt, "+"),
//             Sub => write!(fmt, "-"),
//         }
//     }
// }
