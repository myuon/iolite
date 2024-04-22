use std::vec;

use super::ir::TypeTag;

#[derive(Debug, Clone)]
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

    pub fn has(&self, pos: usize) -> bool {
        match (self.start, self.end) {
            (Some(start), Some(end)) => start <= pos && pos < end,
            _ => false,
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
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
    Nil,
    Bool(Source<bool>),
    Integer(Source<i32>),
    Float(Source<f32>),
    String(Source<String>),
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
pub enum Conversion {
    Cast(TypeTag),
    IntToFloat,
    FloatToInt,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Ident(Source<String>),
    Lit(Source<Literal>),
    Negate {
        ty: Type,
        expr: Box<Source<Expr>>,
    },
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
    MethodCall {
        expr_ty: Type,
        expr: Box<Source<Expr>>,
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
        ty: Type,
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
    As {
        expr: Box<Source<Expr>>,
        ty: Source<Type>,
        conversion: Option<Conversion>,
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
        ty: Type,
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
    Byte,
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

    pub fn methods_builtin(ty: &Type) -> Vec<(String, Type, String)> {
        match ty {
            Type::Int => vec![(
                "abs".to_string(),
                Type::Fun(vec![], Box::new(Type::Int)),
                "int_abs".to_string(),
            )],
            Type::Float => vec![(
                "abs".to_string(),
                Type::Fun(vec![], Box::new(Type::Int)),
                "float_abs".to_string(),
            )],
            Type::Ptr(item) => vec![(
                "offset".to_string(),
                Type::Fun(vec![Type::Int], Box::new(Type::Ptr(item.clone()))),
                "ptr_offset".to_string(),
            )],
            _ => vec![],
        }
    }

    pub fn sizeof(&self) -> usize {
        match self {
            Type::Bool => 1,
            Type::Byte => 1,
            _ => 4,
        }
    }
}
