use std::{
    collections::{BTreeMap, HashMap},
    vec,
};

use super::ir::TypeTag;

#[derive(Debug, Clone)]
pub struct Span {
    pub module_name: Option<String>,
    pub start: Option<usize>,
    pub end: Option<usize>,
}

impl Span {
    pub fn span(module_name: String, start: usize, end: usize) -> Self {
        Self {
            module_name: Some(module_name),
            start: Some(start),
            end: Some(end),
        }
    }

    pub fn unknown() -> Self {
        Self {
            module_name: None,
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

    pub fn new_span(
        data: T,
        module_name: String,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Self {
        Self {
            data,
            span: Span {
                module_name: Some(module_name),
                start,
                end,
            },
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
pub enum UniOp {
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
    UniOp {
        ty: Type,
        op: Source<UniOp>,
        expr: Box<Source<Expr>>,
    },
    BinOp {
        ty: Type,
        op: Source<BinOp>,
        left: Box<Source<Expr>>,
        right: Box<Source<Expr>>,
    },
    Call {
        callee: Box<Source<Expr>>,
        args: Vec<Source<Expr>>,
        newtype: Option<String>,
    },
    MethodCall {
        expr_ty: Type,
        expr: Box<Source<Expr>>,
        name: Source<String>,
        args: Vec<Source<Expr>>,
        call_symbol: Option<String>,
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
    Closure {
        params: Vec<(Source<String>, Source<Type>)>,
        result: Source<Type>,
        body: Box<Source<Block>>,
        captured: Vec<String>,
    },
    Qualified {
        module: Source<String>,
        name: Source<String>,
    },
    Unwrap(Box<Source<Expr>>),
    Range {
        start: Box<Source<Expr>>,
        end: Box<Source<Expr>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForMode {
    Range,
    Array(Type),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Source<String>, Source<Expr>),
    Return(Source<Expr>),
    Expr(Source<Expr>),
    Assign(Type, Source<Expr>, Source<Expr>),
    While {
        cond: Source<Expr>,
        body: Source<Block>,
    },
    For {
        mode: ForMode,
        var: Source<String>,
        expr: Source<Expr>,
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
pub enum MetaTag {
    Test,
    BuiltinMethod(Type, String),
    BuiltinMethodGenericsPtr(String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Function {
        name: Source<String>,
        params: Vec<(Source<String>, Source<Type>)>,
        result: Source<Type>,
        body: Source<Block>,
        meta_tags: Vec<MetaTag>,
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
    Newtype {
        name: Source<String>,
        ty: Source<Type>,
    },
    Import(Source<String>),
    DeclareFunction {
        name: Source<String>,
        params: Vec<(Source<String>, Source<Type>)>,
        result: Source<Type>,
    },
    Module(Module),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub name: String,
    pub declarations: Vec<Source<Declaration>>,
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Ord)]
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
    RawPtr,
    Self_,
    Newtype {
        name: String,
        ty: Box<Type>,
    },
    Range(Box<Type>),
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

    pub fn fields_closure() -> Vec<(String, Type)> {
        vec![
            ("ptr".to_string(), Type::RawPtr),
            ("env".to_string(), Type::RawPtr),
        ]
    }

    pub fn methods_builtin(ty: &Type) -> Vec<(String, Type, String)> {
        match ty {
            Type::Ptr(item) => vec![(
                "offset".to_string(),
                Type::Fun(
                    vec![Type::Self_, Type::Int],
                    Box::new(Type::Ptr(item.clone())),
                ),
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

    pub fn to_string(&self) -> String {
        match self {
            Type::Nil => "nil".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Ptr(item) => format!("ptr[{}]", item.to_string()),
            Type::Array(item) => format!("array[{}]", item.to_string()),
            Type::Fun(params, ret) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) => {}", params, ret.to_string())
            }
            Type::Struct { name, .. } => name.clone(),
            Type::Ident(name) => name.clone(),
            Type::Byte => "byte".to_string(),
            Type::RawPtr => "rawptr".to_string(),
            Type::Unknown => "unknown".to_string(),
            Type::Self_ => "self".to_string(),
            Type::Newtype { name, ty: _ } => name.clone(),
            Type::Range(ty) => format!("range[{}]", ty.to_string()),
        }
    }

    pub fn builtin_types() -> HashMap<String, Source<Type>> {
        let mut types = HashMap::new();
        types.insert(
            "abort".to_string(),
            Source::unknown(Type::Fun(vec![], Box::new(Type::Nil))),
        );

        types
    }

    pub fn replace(&self, from: &str, to: &Type) -> Type {
        match self {
            Type::Ptr(item) => Type::Ptr(Box::new(item.replace(from, to))),
            Type::Array(item) => Type::Array(Box::new(item.replace(from, to))),
            Type::Fun(params, ret) => {
                let params = params.iter().map(|p| p.replace(from, to)).collect();
                Type::Fun(params, Box::new(ret.replace(from, to)))
            }
            Type::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.replace(from, to)))
                    .collect();
                Type::Struct {
                    name: name.clone(),
                    fields,
                }
            }
            Type::Ident(name) => {
                if name == from {
                    to.clone()
                } else {
                    self.clone()
                }
            }
            Type::Newtype { name, ty } => {
                let ty = ty.replace(from, to);
                Type::Newtype {
                    name: name.clone(),
                    ty: Box::new(ty),
                }
            }
            Type::Range(ty) => Type::Range(Box::new(ty.replace(from, to))),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum TypeMapKey {
    Ident(String),
    Qualified(String, String),
}

impl TypeMapKey {
    pub fn as_string(&self) -> String {
        match self {
            TypeMapKey::Ident(name) => name.to_string(),
            TypeMapKey::Qualified(module, name) => format!("{}::{}", module, name),
        }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Eq, Ord)]
pub enum AstItemType {
    Variable,
    Function,
    Struct,
    Field,
    Argument,
    DeclareFunction,
    Newtype,
    GlobalVariable,
}

impl AstItemType {
    pub fn is_function_like(&self) -> bool {
        matches!(self, AstItemType::Function | AstItemType::DeclareFunction)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeMap(pub BTreeMap<TypeMapKey, (Source<Type>, AstItemType)>);

impl TypeMap {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn builtin_types() -> Self {
        let mut types = TypeMap::new();

        for (name, ty) in Type::builtin_types() {
            types
                .0
                .insert(TypeMapKey::Ident(name.clone()), (ty, AstItemType::Function));
        }

        types
    }

    pub fn get_ident(&self, name: &str) -> Option<&(Source<Type>, AstItemType)> {
        self.0.get(&TypeMapKey::Ident(name.to_string()))
    }

    pub fn get(&self, key: &TypeMapKey) -> Option<&(Source<Type>, AstItemType)> {
        self.0.get(key)
    }

    pub fn insert_ident(&mut self, name: String, ty: Source<Type>, item_type: AstItemType) {
        self.0.insert(TypeMapKey::Ident(name), (ty, item_type));
    }

    pub fn insert(&mut self, key: TypeMapKey, ty: Source<Type>, item_type: AstItemType) {
        self.0.insert(key, (ty, item_type));
    }

    pub fn contains_ident_key(&self, name: &str) -> bool {
        self.0.contains_key(&TypeMapKey::Ident(name.to_string()))
    }

    pub fn contains_key(&self, key: &TypeMapKey) -> bool {
        self.0.contains_key(key)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstWalkerMode {
    SemanticTokens,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstWalker {
    mode: AstWalkerMode,
    pub(crate) tokens: Vec<(String, Span)>,
    pub(crate) hints: Vec<(Span, Type)>,
}

pub const AST_WALKER_FUNCTION: &'static str = "FUNCTION";
pub const AST_WALKER_FIELD: &'static str = "FIELD";
pub const AST_WALKER_METHOD: &'static str = "METHOD";
pub const AST_WALKER_TYPE: &'static str = "TYPE";
pub const AST_WALKER_NAMESPACE: &'static str = "NAMESPACE";
pub const AST_WALKER_KEYWORD: &'static str = "KEYWORD";

impl AstWalker {
    pub fn new(mode: AstWalkerMode) -> Self {
        Self {
            mode,
            tokens: vec![],
            hints: vec![],
        }
    }

    pub fn module(&mut self, module: &Module) {
        for decl in &module.declarations {
            self.decl(decl);
        }
    }

    fn decl(&mut self, decl: &Source<Declaration>) {
        match &decl.data {
            Declaration::Function {
                name,
                params,
                result: _,
                body,
                ..
            } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_FUNCTION.to_string(), name.span.clone()));
                    for (name, ty) in params {
                        if name.data == "self" {
                            self.tokens
                                .push((AST_WALKER_KEYWORD.to_string(), name.span.clone()));
                        } else {
                            self.tokens
                                .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                        }
                    }
                }

                self.block(body);
            }
            Declaration::Let { value, .. } => {
                self.expr(value, false);
            }
            Declaration::Struct { .. } => {}
            Declaration::Import(_) => {}
            Declaration::DeclareFunction {
                name,
                params,
                result: _,
            } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_FUNCTION.to_string(), name.span.clone()));
                    for (_, ty) in params {
                        self.tokens
                            .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                    }
                }
            }
            Declaration::Module(module) => {
                self.module(module);
            }
            Declaration::Newtype { name: _, ty } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                }
            }
        }
    }

    fn block(&mut self, block: &Source<Block>) {
        for stmt in &block.data.statements {
            self.stmt(stmt);
        }
        if let Some(expr) = &block.data.expr {
            self.expr(expr, false);
        }
    }

    fn stmt(&mut self, stmt: &Source<Statement>) {
        match &stmt.data {
            Statement::Let(_name, value) => self.expr(value, false),
            Statement::Return(expr) => self.expr(expr, false),
            Statement::Expr(expr) => self.expr(expr, false),
            Statement::Assign(_, lhs, rhs) => {
                self.expr(lhs, false);
                self.expr(rhs, false);
            }
            Statement::While { cond, body } => {
                self.expr(cond, false);
                self.block(body);
            }
            Statement::If { cond, then, else_ } => {
                self.expr(cond, false);
                self.block(then);
                if let Some(else_) = else_ {
                    self.block(else_);
                }
            }
            Statement::Block(block) => self.block(block),
            Statement::For { expr, body, .. } => {
                self.expr(expr, false);
                self.block(body);
            }
        }
    }

    fn expr(&mut self, expr: &Source<Expr>, call: bool) {
        match &expr.data {
            Expr::Ident(ident) => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    if call {
                        self.tokens
                            .push((AST_WALKER_FUNCTION.to_string(), ident.span.clone()));
                    }
                }
            }
            Expr::Lit(_) => {}
            Expr::UniOp { expr, .. } => self.expr(expr, false),
            Expr::BinOp { left, right, .. } => {
                self.expr(left, false);
                self.expr(right, false);
            }
            Expr::Call { args, callee, .. } => {
                self.expr(callee, true);

                for arg in args {
                    self.expr(arg, false);
                }
            }
            Expr::MethodCall {
                expr, args, name, ..
            } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_METHOD.to_string(), name.span.clone()));
                }
                self.expr(expr, false);
                for arg in args {
                    self.expr(arg, false);
                }
            }
            Expr::Match { cond, cases } => {
                self.expr(cond, false);
                for case in cases {
                    self.expr(case, false);
                }
            }
            Expr::New { ty, argument, .. } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                }

                self.expr(argument, false)
            }
            Expr::Index { ptr, index, .. } => {
                self.expr(ptr, false);
                self.expr(index, false);
            }
            Expr::Block(block) => self.block(block),
            Expr::Struct { fields, .. } => {
                for (_, expr) in fields {
                    self.expr(expr, false);
                }
            }
            Expr::Project { expr, field, .. } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_FIELD.to_string(), field.span.clone()));
                }

                self.expr(expr, false)
            }
            Expr::As { expr, ty, .. } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                }

                self.expr(expr, false)
            }
            Expr::Closure {
                params,
                result,
                body,
                captured: _,
            } => {
                for (_, ty) in params {
                    self.tokens
                        .push((AST_WALKER_TYPE.to_string(), ty.span.clone()));
                }

                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_TYPE.to_string(), result.span.clone()));
                }

                self.block(body)
            }
            Expr::Qualified { module, name } => {
                if matches!(self.mode, AstWalkerMode::SemanticTokens) {
                    self.tokens
                        .push((AST_WALKER_NAMESPACE.to_string(), module.span.clone()));

                    if call {
                        self.tokens
                            .push((AST_WALKER_FUNCTION.to_string(), name.span.clone()));
                    }
                }
            }
            Expr::Unwrap(expr) => {
                self.expr(expr, false);
            }
            Expr::Range { start, end } => {
                self.expr(start, false);
                self.expr(end, false);
            }
        }
    }
}
