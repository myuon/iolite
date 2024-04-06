#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(i32),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, PartialEq, Clone)]
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
    If {
        cond: Box<Expr>,
        then: Block,
        else_: Block,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Assign(String, Expr),
    While { cond: Expr, body: Block },
    If { cond: Expr, then: Block },
    Block(Block),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub has_value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Function {
        name: String,
        params: Vec<String>,
        body: Block,
    },
}
