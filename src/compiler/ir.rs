#[derive(Debug, PartialEq, Clone)]
pub enum IrOp {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    And,
    Or,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    IntToFloat,
    FloatToInt,
    IntToPointer,
    PointerToInt,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrTerm {
    Nil,
    Integer(i32),
    Float(f32),
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
    If {
        cond: Box<IrTerm>,
        then: Box<IrTerm>,
        else_: Box<IrTerm>,
    },
    Call {
        name: String,
        args: Vec<IrTerm>,
    },
    Index {
        ptr: Box<IrTerm>,
        index: Box<IrTerm>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrDecl {
    Fun {
        name: String,
        args: Vec<String>,
        body: Box<IrTerm>,
    },
    Let {
        name: String,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrModule {
    pub name: String,
    pub decls: Vec<IrDecl>,
}
