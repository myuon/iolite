#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(i32),
    Float(f32),
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
    Match {
        cond: Box<Expr>,
        cases: Vec<Block>,
    },
    New(Box<Expr>),
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    Assign(Expr, Expr),
    While {
        cond: Expr,
        body: Block,
    },
    If {
        cond: Expr,
        then: Block,
        else_: Option<Block>,
    },
    Block(Block),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Function {
        name: String,
        params: Vec<String>,
        body: Block,
    },
    Let {
        name: String,
        value: Expr,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub name: String,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Unknown,
    Nil,
    Bool,
    Int,
    Float,
    Array(Box<Type>),
    Fun(Vec<Type>, Box<Type>),
}
