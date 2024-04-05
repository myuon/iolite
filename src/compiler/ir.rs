use super::ast::BinOp;

#[derive(Debug, PartialEq)]
pub enum IrOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, PartialEq)]
pub enum IrTerm {
    Nil,
    Integer(i32),
    Ident(String),
    Let {
        name: String,
        value: Box<IrTerm>,
    },
    Op {
        op: IrOp,
        args: Vec<IrTerm>,
    },
    Block {
        terms: Vec<IrTerm>,
    },
    Return(Box<IrTerm>),
    Load(Box<IrTerm>),
    Store(Box<IrTerm>, Box<IrTerm>),
    While {
        cond: Box<IrTerm>,
        body: Box<IrTerm>,
    },
}
