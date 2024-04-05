#[derive(Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Integer(i32),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
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
pub enum Expr {
    Ident(String),
    Lit(Literal),
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Assign(String, Expr),
    While { cond: Expr, body: Block },
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function {
        name: String,
        params: Vec<String>,
        body: Block,
    },
}
