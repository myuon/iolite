#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub start: Option<usize>,
    pub end: Option<usize>,
}

impl Span {
    pub fn span(start: usize, end: usize) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
        }
    }

    pub fn unknown() -> Self {
        Self {
            start: None,
            end: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Source<T> {
    pub data: T,
    pub span: Span,
}

impl<T: PartialEq> PartialEq for Source<T> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.data, &other.data)
    }
}

impl<T> Source<T> {
    #[allow(dead_code)]
    pub fn unknown(data: T) -> Self {
        Self {
            data,
            span: Span::unknown(),
        }
    }

    pub fn new_span(data: T, start: Option<usize>, end: Option<usize>) -> Self {
        Self {
            data,
            span: Span { start, end },
        }
    }

    pub fn span(data: T, span: Span) -> Self {
        Self { data, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(Source<bool>),
    Integer(Source<i32>),
    Float(Source<f32>),
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
    Ident(Source<String>),
    Lit(Source<Literal>),
    BinOp {
        ty: Type,
        op: Source<BinOp>,
        left: Box<Source<Expr>>,
        right: Box<Source<Expr>>,
    },
    Call {
        name: Source<String>,
        args: Vec<Source<Expr>>,
    },
    Match {
        cond: Box<Source<Expr>>,
        cases: Vec<Source<Expr>>,
    },
    New {
        ty: Source<Type>,
        argument: Box<Source<Expr>>,
    },
    Index {
        ptr: Box<Source<Expr>>,
        index: Box<Source<Expr>>,
    },
    Block(Box<Source<Block>>),
    Struct {
        name: Source<String>,
        fields: Vec<(Source<String>, Source<Expr>)>,
    },
    Project {
        expr_ty: Type,
        expr: Box<Source<Expr>>,
        field: Source<String>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Source<String>, Source<Expr>),
    Return(Source<Expr>),
    Expr(Source<Expr>),
    Assign(Source<Expr>, Source<Expr>),
    While {
        cond: Source<Expr>,
        body: Source<Block>,
    },
    If {
        cond: Source<Expr>,
        then: Source<Block>,
        else_: Option<Source<Block>>,
    },
    Block(Source<Block>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Source<Statement>>,
    pub expr: Option<Source<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Function {
        name: Source<String>,
        params: Vec<(Source<String>, Source<Type>)>,
        body: Source<Block>,
    },
    Let {
        name: Source<String>,
        value: Source<Expr>,
    },
    Struct {
        name: Source<String>,
        fields: Vec<(Source<String>, Source<Type>)>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub name: String,
    pub declarations: Vec<Source<Declaration>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Unknown,
    Nil,
    Bool,
    Int,
    Float,
    Ptr(Box<Type>),
    Array(Box<Type>),
    Fun(Vec<Type>, Box<Type>),
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Ident(String),
}

impl Type {
    pub fn as_struct_fields(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Type::Struct { fields, .. } => Some(fields),
            _ => None,
        }
    }

    pub fn fields_array(item: Box<Type>) -> Vec<(String, Type)> {
        vec![
            ("ptr".to_string(), Type::Ptr(item)),
            ("length".to_string(), Type::Int),
        ]
    }
}
