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

#[derive(Debug, PartialEq, Clone)]
pub struct AstWalker {
    pub(crate) tokens: Vec<(String, Span)>,
}

pub const AST_WALKER_FUNCTION: &'static str = "FUNCTION";
pub const AST_WALKER_FIELD: &'static str = "FIELD";
pub const AST_WALKER_METHOD: &'static str = "METHOD";
pub const AST_WALKER_TYPE: &'static str = "TYPE";

impl AstWalker {
    pub fn new() -> Self {
        Self { tokens: vec![] }
    }

    pub fn module(&mut self, module: &Module) {
        for decl in &module.declarations {
            self.decl(decl);
        }
    }

    fn decl(&mut self, decl: &Source<Declaration>) {
        match &decl.data {
            Declaration::Function { name, params, body } => {
                self.tokens
                    .push((AST_WALKER_FUNCTION.to_string(), name.span.clone()));
                self.block(body);
            }
            Declaration::Let { name, value, .. } => {
                self.expr(value);
            }
            Declaration::Struct { fields, .. } => {}
        }
    }

    fn block(&mut self, block: &Source<Block>) {
        for stmt in &block.data.statements {
            self.stmt(stmt);
        }
        if let Some(expr) = &block.data.expr {
            self.expr(expr);
        }
    }

    fn stmt(&mut self, stmt: &Source<Statement>) {
        match &stmt.data {
            Statement::Let(_, value) => self.expr(value),
            Statement::Return(expr) => self.expr(expr),
            Statement::Expr(expr) => self.expr(expr),
            Statement::Assign(lhs, rhs) => {
                self.expr(lhs);
                self.expr(rhs);
            }
            Statement::While { cond, body } => {
                self.expr(cond);
                self.block(body);
            }
            Statement::If { cond, then, else_ } => {
                self.expr(cond);
                self.block(then);
                if let Some(else_) = else_ {
                    self.block(else_);
                }
            }
            Statement::Block(block) => self.block(block),
        }
    }

    fn expr(&mut self, expr: &Source<Expr>) {
        match &expr.data {
            Expr::Ident(_) => {}
            Expr::Lit(_) => {}
            Expr::Negate { expr, .. } => self.expr(expr),
            Expr::BinOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            Expr::Call { args, name } => {
                self.tokens
                    .push((AST_WALKER_FUNCTION.to_string(), name.span.clone()));
                for arg in args {
                    self.expr(arg);
                }
            }
            Expr::MethodCall {
                expr, args, name, ..
            } => {
                self.tokens
                    .push((AST_WALKER_METHOD.to_string(), name.span.clone()));
                self.expr(expr);
                for arg in args {
                    self.expr(arg);
                }
            }
            Expr::Match { cond, cases } => {
                self.expr(cond);
                for case in cases {
                    self.expr(case);
                }
            }
            Expr::New { ty, argument, .. } => {
                self.tokens
                    .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));

                self.expr(argument)
            }
            Expr::Index { ptr, index, .. } => {
                self.expr(ptr);
                self.expr(index);
            }
            Expr::Block(block) => self.block(block),
            Expr::Struct { fields, .. } => {
                for (_, expr) in fields {
                    self.expr(expr);
                }
            }
            Expr::Project { expr, field, .. } => {
                self.tokens
                    .push((AST_WALKER_FIELD.to_string(), field.span.clone()));
                self.expr(expr)
            }
            Expr::As { expr, ty, .. } => {
                self.tokens
                    .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                self.expr(expr)
            }
        }
    }
}
