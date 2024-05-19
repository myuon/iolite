use std::collections::HashMap;

use thiserror::Error;

use super::{
    ast::{
        BinOp, Block, Conversion, Declaration, Expr, Literal, Module, Source, Span, Statement, Type,
    },
    ir::TypeTag,
};

#[derive(Debug, Clone, Error)]
pub enum TypecheckerError {
    #[error("Identifier not found: {0:?}")]
    IdentNotFound(Source<String>),
    #[error("Type mismatch: expected {expected:?}, but got {actual:?}")]
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    #[error("Numeric type expected, but got {0:?}")]
    NumericTypeExpected(Type),
    #[error("Argument count mismatch: expected {0}, but got {1}")]
    ArgumentCountMismatch(usize, usize),
    #[error("Function type expected, but got {0:?}")]
    FunctionTypeExpected(Type),
    #[error("Index not supported for type {0:?}")]
    IndexNotSupported(Type),
    #[error("Conversion not supported from {0:?} to {1:?}")]
    ConversionNotSupported(Type, Source<Type>),
}

impl PartialEq for TypecheckerError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypecheckerError::IdentNotFound(a), TypecheckerError::IdentNotFound(b)) => a == b,
            (
                TypecheckerError::TypeMismatch {
                    expected: a_expected,
                    actual: a_actual,
                    span: _,
                },
                TypecheckerError::TypeMismatch {
                    expected: b_expected,
                    actual: b_actual,
                    span: _,
                },
            ) => a_expected == b_expected && a_actual == b_actual,
            (
                TypecheckerError::NumericTypeExpected(a),
                TypecheckerError::NumericTypeExpected(b),
            ) => a == b,
            (
                TypecheckerError::ArgumentCountMismatch(a_expected, a_actual),
                TypecheckerError::ArgumentCountMismatch(b_expected, b_actual),
            ) => a_expected == b_expected && a_actual == b_actual,
            (
                TypecheckerError::FunctionTypeExpected(a),
                TypecheckerError::FunctionTypeExpected(b),
            ) => a == b,
            (TypecheckerError::IndexNotSupported(a), TypecheckerError::IndexNotSupported(b)) => {
                a == b
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct SearchDefinition {
    position: usize,
    found: Option<Span>,
}

#[derive(Debug, Clone)]
struct InferTypeAt {
    position: usize,
    found: Option<Type>,
}

pub struct Typechecker {
    pub types: HashMap<String, Source<Type>>,
    return_ty: Type,
    search_def: Option<SearchDefinition>,
    infer_type_at: Option<InferTypeAt>,
    inlay_hints: Option<Vec<(Span, Type)>>,
    ident_referred: Vec<String>,
    globals: Vec<String>,
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            types: Type::builtin_types(),
            return_ty: Type::Unknown,
            search_def: None,
            infer_type_at: None,
            inlay_hints: None,
            ident_referred: vec![],
            globals: Type::builtin_types().keys().cloned().collect(),
        }
    }

    fn unify(expected: Type, actual: Type, span: Span) -> Result<Type, TypecheckerError> {
        match (&expected, &actual) {
            (Type::Unknown, ty) => Ok(ty.clone()),
            (ty, Type::Unknown) => Ok(ty.clone()),
            (a, b) if a == b => Ok(a.clone()),
            _ => Err(TypecheckerError::TypeMismatch {
                expected,
                actual,
                span,
            }),
        }
    }

    fn get_type(&self, name: &str, span: &Span) -> Result<Source<Type>, TypecheckerError> {
        self.types
            .get(name)
            .cloned()
            .ok_or(TypecheckerError::IdentNotFound(Source::span(
                name.to_string(),
                span.clone(),
            )))
    }

    fn check_search_ident(&mut self, ident: &Source<Expr>) {
        if let Some(search) = &self.search_def {
            if ident.span.has(search.position) {
                match &ident.data {
                    Expr::Ident(ident) => {
                        self.search_def = Some(SearchDefinition {
                            position: search.position,
                            found: Some(
                                self.get_type(&ident.data, &ident.span)
                                    .unwrap()
                                    .span
                                    .clone(),
                            ),
                        });
                    }
                    _ => todo!(),
                }
            }
        }
    }

    fn check_infer_type_at(&mut self, span: &Span, ty: Type) {
        if let Some(infer) = &self.infer_type_at {
            if span.has(infer.position) {
                self.infer_type_at = Some(InferTypeAt {
                    position: infer.position,
                    found: Some(ty),
                });
            }
        }
    }

    fn check_inlay_hints(&mut self, span: &Span, ty: Type) {
        if let Some(inlay_hints) = &mut self.inlay_hints {
            inlay_hints.push((span.clone(), ty));
        }
    }

    fn expr_infer(
        &mut self,
        expr: &mut Source<Expr>,
        expected: Type,
    ) -> Result<Type, TypecheckerError> {
        let span = expr.span.clone();
        let actual = self.expr(expr)?;
        Self::unify(expected, actual, span)
    }

    pub fn expr(&mut self, expr: &mut Source<Expr>) -> Result<Type, TypecheckerError> {
        match &mut expr.data {
            Expr::Ident(i) => {
                self.check_search_ident(&Source::span(Expr::Ident(i.clone()), i.span.clone()));

                let ty = self.get_type(&i.data, &i.span)?.data;
                self.check_infer_type_at(&expr.span, ty.clone());

                self.ident_referred.push(i.data.clone());

                Ok(ty)
            }
            Expr::Lit(lit) => {
                let ty = match lit.data {
                    Literal::Nil => Type::Nil,
                    Literal::Bool(_) => Type::Bool,
                    Literal::Integer(_) => Type::Int,
                    Literal::Float(_) => Type::Float,
                    Literal::String(_) => Type::Array(Box::new(Type::Byte)),
                };
                self.check_infer_type_at(&lit.span, ty.clone());

                Ok(ty)
            }
            Expr::BinOp {
                ty: expr_ty,
                op,
                left,
                right,
            } => {
                let left_ty = self.expr(left)?;
                let right_ty = self.expr(right)?;

                match op.data {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        let ty = Self::unify(Type::Unknown, left_ty, op.span.clone())?;
                        let result = Self::unify(ty, right_ty, op.span.clone())?;
                        match result {
                            Type::Int | Type::Float => {}
                            _ => {
                                return Err(TypecheckerError::NumericTypeExpected(result));
                            }
                        }

                        *expr_ty = result.clone();

                        Ok(result)
                    }
                    BinOp::And | BinOp::Or => {
                        Self::unify(Type::Bool, left_ty, op.span.clone())?;
                        Self::unify(Type::Bool, right_ty, op.span.clone())?;

                        *expr_ty = Type::Bool;

                        Ok(Type::Bool)
                    }
                    BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                        let ty = Self::unify(Type::Unknown, left_ty, op.span.clone())?;
                        let result = Self::unify(ty, right_ty, op.span.clone())?;
                        match result {
                            Type::Int | Type::Float => {}
                            _ => {
                                return Err(TypecheckerError::NumericTypeExpected(result));
                            }
                        }

                        *expr_ty = result;

                        Ok(Type::Bool)
                    }
                }
            }
            Expr::Call { callee, args } => {
                self.check_search_ident(callee);

                let mut arg_types_actual = vec![];

                for arg in args {
                    arg_types_actual.push(self.expr(arg)?);
                }

                let fun_ty = self.expr(callee)?;
                self.check_infer_type_at(&expr.span, fun_ty.clone());

                match fun_ty {
                    Type::Fun(arg_types_expected, ret_ty) => {
                        if arg_types_actual.len() != arg_types_expected.len() {
                            return Err(TypecheckerError::ArgumentCountMismatch(
                                arg_types_expected.len(),
                                arg_types_actual.len(),
                            ));
                        }

                        for (expected, actual) in arg_types_expected.iter().zip(arg_types_actual) {
                            Self::unify(expected.clone(), actual, Span::unknown())?;
                        }

                        Ok(*ret_ty)
                    }
                    _ => {
                        return Err(TypecheckerError::FunctionTypeExpected(fun_ty));
                    }
                }
            }
            Expr::Match { cond, cases } => {
                self.expr_infer(cond, Type::Bool)?;

                let mut result_ty = Type::Unknown;
                for case in cases {
                    result_ty = self.expr_infer(case, result_ty.clone())?;
                }

                Ok(result_ty)
            }
            Expr::New { ty, argument: expr } => {
                self.expr_infer(expr, Type::Int)?;

                Ok(ty.data.clone())
            }
            Expr::Index { ty, ptr, index } => {
                let ptr_ty = self.expr(ptr)?;
                self.expr_infer(index, Type::Int)?;

                *ty = ptr_ty.clone();

                match ptr_ty {
                    Type::Ptr(ty) => Ok(*ty),
                    Type::Array(ty) => Ok(*ty),
                    _ => Err(TypecheckerError::IndexNotSupported(ptr_ty)),
                }
            }
            Expr::Block(block) => self.block(block),
            Expr::Struct { name, fields } => {
                let struct_ty = self.get_type(&name.data, &name.span)?.data;
                match struct_ty.clone() {
                    Type::Struct {
                        fields: field_types,
                        ..
                    } => {
                        for (label, expr) in fields {
                            let field_ty = field_types
                                .iter()
                                .find(|(name, _)| name == &label.data)
                                .map(|(_, ty)| ty.clone())
                                .unwrap();

                            self.expr_infer(expr, field_ty)?;
                        }

                        Ok(struct_ty)
                    }
                    _ => todo!(),
                }
            }
            Expr::Project {
                expr_ty,
                expr,
                field,
            } => {
                let ty = self.expr(expr)?;
                let field_types = match ty.clone() {
                    Type::Struct {
                        fields: field_types,
                        name,
                    } => {
                        *expr_ty = Type::Ident(name.clone());

                        field_types
                    }
                    Type::Array(arr) => {
                        *expr_ty = ty.clone();

                        Type::fields_array(arr)
                    }
                    Type::Fun(_, _) => {
                        *expr_ty = ty.clone();

                        Type::fields_closure()
                    }
                    _ => {
                        return Err(TypecheckerError::IdentNotFound(field.clone()));
                    }
                };

                let field_ty = field_types
                    .iter()
                    .find(|(name, _)| name == &field.data)
                    .map(|(_, ty)| ty.clone())
                    .ok_or(TypecheckerError::IdentNotFound(field.clone()))?;

                self.check_infer_type_at(&field.span, field_ty.clone());

                Ok(field_ty)
            }
            Expr::As {
                expr,
                ty,
                conversion,
            } => {
                let expr_ty = self.expr(expr)?;

                match (expr_ty.clone(), ty.data.clone()) {
                    (Type::Int, Type::Float) => {
                        *conversion = Some(Conversion::IntToFloat);
                    }
                    (Type::Float, Type::Int) => {
                        *conversion = Some(Conversion::FloatToInt);
                    }
                    (Type::Int, Type::Ptr(_)) => {
                        *conversion = Some(Conversion::Cast(TypeTag::Pointer));
                    }
                    (Type::Ptr(_), Type::Int) => {
                        *conversion = Some(Conversion::Cast(TypeTag::Int));
                    }
                    (Type::Int, Type::Byte) => {
                        *conversion = Some(Conversion::Cast(TypeTag::Byte));
                    }
                    (Type::Ptr(_), Type::RawPtr) => {
                        *conversion = Some(Conversion::Cast(TypeTag::None));
                    }
                    (Type::Nil, Type::RawPtr) => {
                        *conversion = Some(Conversion::Cast(TypeTag::None));
                    }
                    _ => {
                        return Err(TypecheckerError::ConversionNotSupported(
                            expr_ty,
                            ty.clone(),
                        ));
                    }
                };

                Ok(ty.data.clone())
            }
            Expr::Negate { expr, ty: expr_ty } => {
                let ty = self.expr(expr)?;

                match ty {
                    Type::Int | Type::Float => {}
                    _ => {
                        return Err(TypecheckerError::NumericTypeExpected(ty));
                    }
                }

                *expr_ty = ty.clone();

                Ok(ty)
            }
            Expr::MethodCall {
                expr_ty,
                expr,
                name,
                args,
            } => {
                let ty = self.expr(expr)?;

                *expr_ty = ty.clone();

                let methods = Type::methods_builtin(&ty);
                let method = methods
                    .iter()
                    .find(|(method_name, _, _)| method_name == &name.data)
                    .ok_or(TypecheckerError::IdentNotFound(name.clone()))?
                    .1
                    .clone();

                let mut arg_types_actual = vec![];
                for arg in args {
                    arg_types_actual.push(self.expr(arg)?);
                }

                match method {
                    Type::Fun(arg_types_expected, ret_ty) => {
                        if arg_types_actual.len() != arg_types_expected.len() {
                            return Err(TypecheckerError::ArgumentCountMismatch(
                                arg_types_expected.len(),
                                arg_types_actual.len(),
                            ));
                        }

                        for (expected, actual) in arg_types_expected.iter().zip(arg_types_actual) {
                            Self::unify(expected.clone(), actual, Span::unknown())?;
                        }

                        Ok(*ret_ty)
                    }
                    _ => {
                        return Err(TypecheckerError::FunctionTypeExpected(method));
                    }
                }
            }
            Expr::Closure {
                params,
                result,
                body,
                captured,
            } => {
                let types_cloned = self.types.clone();
                let mut param_types = vec![];

                for param in params {
                    param_types.push(param.1.data.clone());
                    self.types.insert(
                        param.0.data.clone(),
                        Source::span(param.1.data.clone(), param.0.span.clone()),
                    );
                }

                self.return_ty = result.data.clone();
                self.ident_referred = vec![];

                self.block(body)?;

                for ident in &self.ident_referred {
                    if !types_cloned.contains_key(ident) {
                        // local variable in a closure
                        continue;
                    }
                    if self.globals.contains(ident) {
                        // global variable
                        continue;
                    }

                    captured.push(ident.clone());
                }

                let ty = Type::Fun(param_types.clone(), Box::new(self.return_ty.clone()));

                self.types = types_cloned;

                Ok(ty)
            }
        }
    }

    fn statement(&mut self, stmt: &mut Source<Statement>) -> Result<(), TypecheckerError> {
        match &mut stmt.data {
            Statement::Let(name, value) => {
                let ty = self.expr(value)?;
                self.check_inlay_hints(&name.span, ty.clone());
                self.types
                    .insert(name.data.clone(), Source::span(ty, name.span.clone()));
            }
            Statement::Return(expr) => {
                self.return_ty = self.expr_infer(expr, self.return_ty.clone())?;
            }
            Statement::Expr(expr) => {
                self.expr(expr)?;
            }
            Statement::Assign(assign_ty, left, right) => {
                let left_ty = self.expr(left)?;
                let ty = self.expr_infer(right, left_ty.clone())?;
                *assign_ty = ty.clone();
            }
            Statement::While { cond, body } => {
                self.expr_infer(cond, Type::Bool)?;
                self.block(body)?;
            }
            Statement::If { cond, then, else_ } => {
                self.expr_infer(cond, Type::Bool)?;

                self.block_infer(then, Type::Nil)?;
                if let Some(else_) = else_ {
                    self.block_infer(else_, Type::Nil)?;
                }
            }
            Statement::Block(block) => {
                self.block(block)?;
            }
        }

        Ok(())
    }

    fn block_infer(
        &mut self,
        block: &mut Source<Block>,
        expected: Type,
    ) -> Result<Type, TypecheckerError> {
        let span = block.span.clone();
        let actual = self.block(block)?;
        Self::unify(expected, actual, span)
    }

    pub fn block(&mut self, block: &mut Source<Block>) -> Result<Type, TypecheckerError> {
        for stmt in &mut block.data.statements {
            self.statement(stmt)?;
        }

        if let Some(expr) = &mut block.data.expr {
            Ok(self.expr(expr)?)
        } else {
            Ok(Type::Nil)
        }
    }

    fn decl(&mut self, decl: &mut Source<Declaration>) -> Result<(), TypecheckerError> {
        match &mut decl.data {
            Declaration::Function {
                name,
                params,
                result,
                body,
            } => {
                let types_cloned = self.types.clone();
                let mut param_types = vec![];

                for param in params {
                    param_types.push(param.1.data.clone());
                    self.types.insert(
                        param.0.data.clone(),
                        Source::span(param.1.data.clone(), param.0.span.clone()),
                    );
                }

                self.return_ty = result.data.clone();
                self.types.insert(
                    name.data.clone(),
                    Source::span(
                        Type::Fun(param_types.clone(), Box::new(self.return_ty.clone())),
                        name.span.clone(),
                    ),
                );
                self.globals.push(name.data.clone());

                self.block(body)?;

                self.check_inlay_hints(&result.span, self.return_ty.clone());

                let ty = Type::Fun(param_types, Box::new(self.return_ty.clone()));

                self.types = types_cloned;
                self.types
                    .insert(name.data.clone(), Source::span(ty, name.span.clone()));
            }
            Declaration::Let {
                name,
                ty: let_ty,
                value,
            } => {
                let ty = self.expr(value)?;

                *let_ty = ty.clone();

                self.types
                    .insert(name.data.clone(), Source::span(ty, name.span.clone()));
                self.globals.push(name.data.clone());
            }
            Declaration::Struct { name, fields } => {
                let mut field_types = vec![];

                for field in fields {
                    field_types.push((field.0.data.clone(), field.1.data.clone()));
                }

                self.types.insert(
                    name.data.clone(),
                    Source::span(
                        Type::Struct {
                            name: name.data.clone(),
                            fields: field_types,
                        },
                        name.span.clone(),
                    ),
                );
            }
            Declaration::Import(_) => {}
            Declaration::DeclareFunction {
                name,
                params,
                result,
            } => {
                let mut param_types = vec![];

                for param in params {
                    param_types.push(param.1.data.clone());
                }

                self.types.insert(
                    name.data.clone(),
                    Source::span(
                        Type::Fun(param_types.clone(), Box::new(result.data.clone())),
                        name.span.clone(),
                    ),
                );
                self.globals.push(name.data.clone());
            }
        }

        Ok(())
    }

    pub fn module(&mut self, module: &mut Module) -> Result<(), TypecheckerError> {
        for decl in &mut module.declarations {
            self.decl(decl)?;
        }

        Ok(())
    }

    pub fn search_for_definition(&mut self, module: &mut Module, position: usize) -> Option<Span> {
        self.search_def = Some(SearchDefinition {
            position,
            found: None,
        });

        self.module(module).unwrap();

        self.search_def.clone()?.found
    }

    pub fn infer_type_at(&mut self, module: &mut Module, position: usize) -> Option<Type> {
        self.infer_type_at = Some(InferTypeAt {
            position,
            found: None,
        });

        self.module(module).unwrap();

        self.infer_type_at.clone()?.found
    }

    pub fn inlay_hints(
        &mut self,
        module: &mut Module,
    ) -> Result<Vec<(Span, Type)>, TypecheckerError> {
        self.inlay_hints = Some(vec![]);

        self.module(module)?;

        Ok(self.inlay_hints.clone().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{Compiler, CompilerError};

    use super::*;

    #[test]
    fn test_typecheck_errors() {
        let cases = vec![
            (
                r#"fun main() { x; }"#,
                TypecheckerError::IdentNotFound(Source::unknown("x".to_string())),
            ),
            (
                r#"fun f(a: int) { return a; }
                fun main() { f(true); }"#,
                TypecheckerError::TypeMismatch {
                    expected: Type::Int,
                    actual: Type::Bool,
                    span: Span::unknown(),
                },
            ),
            (
                r#"fun main() { let failing = SomeThing { a: 10 }; }"#,
                TypecheckerError::IdentNotFound(Source::unknown("SomeThing".to_string())),
            ),
            (
                r#"fun main() { let p = new[ptr[int]](1); return p.something; }"#,
                TypecheckerError::IdentNotFound(Source::unknown("something".to_string())),
            ),
            (
                r#"fun main() { return 1 + true; }"#,
                TypecheckerError::TypeMismatch {
                    expected: Type::Int,
                    actual: Type::Bool,
                    span: Span::unknown(),
                },
            ),
        ];

        for (input, error) in cases {
            let result = Compiler::compile_with_input(input.to_string());

            match result.err() {
                Some(CompilerError::TypecheckError(err)) => {
                    assert_eq!(err, error);
                }
                result => {
                    assert!(false, "want {:?}, but got {:?}", error, result);
                }
            }
        }
    }
}
