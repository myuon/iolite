use std::collections::HashSet;

use thiserror::Error;

use super::{
    ast::{
        AstItemType, BinOp, Block, Conversion, Declaration, Expr, ForMode, Literal, Module, Source,
        Span, Statement, Type, TypeMap, TypeMapKey, UniOp,
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
    #[error("Argument count mismatch: expected {1}, but got {2}")]
    ArgumentCountMismatch(Span, usize, usize),
    #[error("Function type expected, but got {0:?}")]
    FunctionTypeExpected(Type),
    #[error("Index not supported for type {0:?}")]
    IndexNotSupported(Type),
    #[error("Conversion not supported from {0:?} to {1:?}")]
    ConversionNotSupported(Type, Source<Type>),
    #[error("Return expected")]
    ReturnExpected,
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
                TypecheckerError::ArgumentCountMismatch(_, a_expected, a_actual),
                TypecheckerError::ArgumentCountMismatch(_, b_expected, b_actual),
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

impl TypecheckerError {
    pub fn as_span(&self) -> Span {
        match self {
            TypecheckerError::IdentNotFound(source) => source.span.clone(),
            TypecheckerError::TypeMismatch { span, .. }
            | TypecheckerError::ArgumentCountMismatch(span, ..) => span.clone(),
            _ => Span::unknown(),
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

#[derive(Debug, Clone)]
struct Completion {
    position: usize,
    found: Option<Vec<(String, Type, AstItemType)>>,
}

pub struct Typechecker {
    pub types: TypeMap,
    return_ty: Type,
    search_def: Option<SearchDefinition>,
    infer_type_at: Option<InferTypeAt>,
    inlay_hints: Option<Vec<(Span, Type)>>,
    ident_referred: Vec<String>,
    globals: Vec<String>,
    current_module: String,
    completion: Option<Completion>,
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            types: TypeMap::builtin_types(),
            return_ty: Type::Unknown,
            search_def: None,
            infer_type_at: None,
            inlay_hints: None,
            ident_referred: vec![],
            globals: Type::builtin_types().keys().cloned().collect(),
            current_module: "".to_string(),
            completion: None,
        }
    }

    fn unify(expected: Type, actual: Type, span: Span) -> Result<Type, TypecheckerError> {
        match (&expected, &actual) {
            (Type::Unknown, ty) => Ok(ty.clone()),
            (ty, Type::Unknown) => Ok(ty.clone()),
            (a, b) if a == b => Ok(a.clone()),
            (
                Type::Ident(a),
                Type::Struct {
                    name: b_name,
                    fields: b_fields,
                },
            ) if a == b_name => Ok(Type::Struct {
                name: b_name.clone(),
                fields: b_fields.clone(),
            }),
            _ => Err(TypecheckerError::TypeMismatch {
                expected,
                actual,
                span,
            }),
        }
    }

    fn get_type(&self, name: &str, span: &Span) -> Result<Source<Type>, TypecheckerError> {
        Ok(self
            .types
            .get_ident(name)
            .cloned()
            .ok_or(TypecheckerError::IdentNotFound(Source::span(
                name.to_string(),
                span.clone(),
            )))?
            .0)
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

    fn check_completion(&mut self, span: &Span, completions: Vec<(String, Type, AstItemType)>) {
        if let Some(completion) = &mut self.completion {
            if span.has(completion.position - 1) {
                let mut found = completion.found.clone().unwrap_or(vec![]);
                found.extend(completions);

                completion.found = Some(found);
            }
        }
    }

    fn find_methods(&self, ty: &Type) -> Vec<(String, Type, String)> {
        match ty {
            Type::Ident(ident) => {
                let mut methods = vec![];

                for (key, (ty, _)) in &self.types.0 {
                    if key.as_string().starts_with(format!("{}::", ident).as_str()) {
                        if let Type::Fun(_, _) = ty.data {
                            methods.push((
                                key.as_string().split("::").last().unwrap().to_string(),
                                ty.data.clone(),
                                key.as_string().to_string(),
                            ));
                        }
                    }
                }

                methods
            }
            Type::Struct {
                name: ident,
                fields: _,
            } => {
                let mut methods = vec![];

                for (key, (ty, _)) in &self.types.0 {
                    if key.as_string().starts_with(format!("{}::", ident).as_str()) {
                        if let Type::Fun(_, _) = ty.data {
                            methods.push((
                                key.as_string().split("::").last().unwrap().to_string(),
                                ty.data.clone(),
                                key.as_string().to_string(),
                            ));
                        }
                    }
                }

                methods
            }
            ty => Type::methods_builtin(&ty),
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

                if let Some(completion) = &self.completion {
                    if i.span.has(completion.position - 1) {
                        // Search similar identifiers
                        let mut completions = vec![];
                        for key in self.types.0.keys() {
                            if key.as_string().contains(&i.data) {
                                let (ty, item_type) = self.types.get(key).unwrap().clone();

                                completions.push((key.as_string(), ty.data.clone(), item_type));
                            }
                        }

                        completions.sort();

                        self.check_completion(&i.span, completions);
                    }
                }

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
            Expr::Call {
                callee,
                args,
                newtype,
            } => {
                let callee_span = callee.span.clone();
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
                                callee_span,
                                arg_types_expected.len(),
                                arg_types_actual.len(),
                            ));
                        }

                        for (expected, actual) in arg_types_expected.iter().zip(arg_types_actual) {
                            Self::unify(expected.clone(), actual, callee.span.clone())?;
                        }

                        Ok(*ret_ty)
                    }
                    Type::Newtype { name, ty } => {
                        if arg_types_actual.len() != 1 {
                            return Err(TypecheckerError::ArgumentCountMismatch(
                                callee_span,
                                1,
                                arg_types_actual.len(),
                            ));
                        }

                        Self::unify(*ty, arg_types_actual[0].clone(), callee.span.clone())?;

                        *newtype = Some(name.clone());

                        Ok(Type::Ident(name))
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
                let mut ty = self.expr(expr)?;
                match ty {
                    Type::Ident(ident) => {
                        ty = self.get_type(&ident, &expr.span)?.data;
                    }
                    _ => (),
                }

                if self.completion.is_some() {
                    // When completion is triggered by a dot, show methods as well

                    let methods = self
                        .find_methods(&ty)
                        .into_iter()
                        .map(|(name, ty, _)| (name, ty, AstItemType::Function))
                        .collect();
                    println!("{:?}", methods);

                    self.check_completion(&field.span, methods);
                }

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

                self.check_completion(
                    &field.span,
                    field_types
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.clone(), AstItemType::Field))
                        .collect::<Vec<_>>(),
                );

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
                    (Type::Byte, Type::Int) => {
                        *conversion = Some(Conversion::Cast(TypeTag::Int));
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
            Expr::UniOp {
                expr,
                op,
                ty: expr_ty,
            } => {
                let ty = self.expr(expr)?;

                match op.data {
                    UniOp::Negate => {
                        match ty {
                            Type::Int | Type::Float => {}
                            _ => {
                                return Err(TypecheckerError::NumericTypeExpected(ty));
                            }
                        }

                        *expr_ty = ty.clone();
                    }
                    UniOp::Not => {
                        *expr_ty = Self::unify(Type::Bool, ty.clone(), op.span.clone())?;
                    }
                }

                Ok(ty)
            }
            Expr::MethodCall {
                expr_ty,
                expr,
                name,
                args,
                call_symbol,
            } => {
                let expr_span = expr.span.clone();
                let ty = self.expr(expr)?;

                *expr_ty = ty.clone();

                let methods = self.find_methods(&ty);
                let (_, method, symbol) = methods
                    .iter()
                    .find(|(method_name, _, _)| method_name == &name.data)
                    .ok_or(TypecheckerError::IdentNotFound(name.clone()))?
                    .clone();
                *call_symbol = Some(symbol);

                self.check_infer_type_at(&name.span, method.clone());

                let mut arg_types_actual = vec![];
                for arg in args {
                    arg_types_actual.push((arg.span.clone(), self.expr(arg)?));
                }

                match method {
                    Type::Fun(arg_types_expected_, ret_ty) => {
                        let arg_types_expected = (&arg_types_expected_[1..]).to_vec();
                        if arg_types_actual.len() != arg_types_expected.len() {
                            return Err(TypecheckerError::ArgumentCountMismatch(
                                expr_span,
                                arg_types_expected.len(),
                                arg_types_actual.len(),
                            ));
                        }

                        for (expected, (actual_span, actual)) in
                            arg_types_expected.iter().zip(arg_types_actual)
                        {
                            Self::unify(expected.clone(), actual, actual_span)?;
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
                    self.types.insert_ident(
                        param.0.data.clone(),
                        Source::span(param.1.data.clone(), param.0.span.clone()),
                        AstItemType::Argument,
                    );
                }

                self.return_ty = result.data.clone();
                self.ident_referred = vec![];

                self.block_with_implicit_return(body, result)?;

                for ident in self.ident_referred.iter().collect::<HashSet<_>>() {
                    if !types_cloned.contains_ident_key(ident) {
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
            Expr::Qualified { module, name } => {
                let ty = self
                    .types
                    .get(&TypeMapKey::Qualified(
                        module.data.clone(),
                        name.data.clone(),
                    ))
                    .ok_or(TypecheckerError::IdentNotFound(Source::span(
                        format!("{}::{}", module.data, name.data),
                        name.span.clone(),
                    )))?
                    .clone()
                    .0;

                Ok(ty.data.clone())
            }
            Expr::Unwrap(expr) => {
                let ty = self.expr(expr)?;

                match ty {
                    Type::Ident(ident) => {
                        let ty = self.types.get_ident(&ident).unwrap().0.data.clone();

                        match ty {
                            Type::Newtype { ty, .. } => Ok(*ty),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            Expr::Range { start, end } => {
                let start_ty = self.expr(start)?;
                let ty = self.expr_infer(end, start_ty)?;

                match ty {
                    Type::Int => Ok(Type::Range(Box::new(Type::Int))),
                    _ => todo!(),
                }
            }
        }
    }

    fn statement(&mut self, stmt: &mut Source<Statement>) -> Result<(), TypecheckerError> {
        match &mut stmt.data {
            Statement::Let(name, value) => {
                let ty = self.expr(value)?;
                self.check_inlay_hints(&name.span, ty.clone());
                self.check_infer_type_at(&name.span, ty.clone());
                self.types.insert_ident(
                    name.data.clone(),
                    Source::span(ty, name.span.clone()),
                    AstItemType::Variable,
                );
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
            Statement::For {
                mode,
                var,
                expr,
                body,
            } => {
                let ty = self.expr(expr)?;
                match ty {
                    Type::Range(ty) => {
                        self.types.insert_ident(
                            var.data.clone(),
                            Source::span(*ty, var.span.clone()),
                            AstItemType::Variable,
                        );
                        self.block(body)?;

                        *mode = ForMode::Range;
                    }
                    Type::Array(ty) => {
                        self.types.insert_ident(
                            var.data.clone(),
                            Source::span(*ty.clone(), var.span.clone()),
                            AstItemType::Variable,
                        );
                        self.block(body)?;

                        *mode = ForMode::Array(*ty);
                    }
                    _ => todo!("{:?}", ty),
                }
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

    pub fn block_with_implicit_return(
        &mut self,
        block: &mut Source<Block>,
        result: &mut Source<Type>,
    ) -> Result<(), TypecheckerError> {
        self.block(block)?;
        if !matches!(
            block.data.statements.last().cloned().map(|t| t.data),
            Some(Statement::Return(_))
        ) {
            if matches!(result.data, Type::Nil) {
                block.data.statements.push(Source::span(
                    Statement::Return(Source::span(
                        Expr::Lit(Source::span(Literal::Nil, Span::unknown())),
                        Span::unknown(),
                    )),
                    Span::unknown(),
                ));
            } else if matches!(result.data, Type::Unknown) {
                result.data = Type::Nil;

                block.data.statements.push(Source::span(
                    Statement::Return(Source::span(
                        Expr::Lit(Source::span(Literal::Nil, Span::unknown())),
                        Span::unknown(),
                    )),
                    Span::unknown(),
                ));
            } else {
                return Err(TypecheckerError::ReturnExpected);
            }
        }

        Ok(())
    }

    fn decl(&mut self, decl: &mut Source<Declaration>) -> Result<(), TypecheckerError> {
        match &mut decl.data {
            Declaration::Function {
                name,
                params,
                result,
                body,
                meta_tags: _,
            } => {
                let types_cloned = self.types.clone();
                let mut param_types = vec![];

                let path = if self.current_module != "" {
                    TypeMapKey::Qualified(self.current_module.clone(), name.data.clone())
                } else {
                    TypeMapKey::Ident(name.data.clone())
                };

                for param in params {
                    let ty = if param.0.data == "self" {
                        Type::Ident(self.current_module.clone())
                    } else {
                        param.1.data.clone()
                    };

                    param_types.push(ty.clone());
                    self.types.insert_ident(
                        param.0.data.clone(),
                        Source::span(ty.clone(), param.0.span.clone()),
                        AstItemType::Argument,
                    );
                }

                self.return_ty = result.data.clone();
                self.types.insert(
                    path.clone(),
                    Source::span(
                        Type::Fun(param_types.clone(), Box::new(self.return_ty.clone())),
                        name.span.clone(),
                    ),
                    AstItemType::Function,
                );
                self.globals.push(path.as_string());

                self.block_with_implicit_return(body, result)?;

                let result_ty_explicitly_written = result.span.start != result.span.end;
                if !result_ty_explicitly_written {
                    self.check_inlay_hints(&result.span, self.return_ty.clone());
                }

                let ty = Type::Fun(param_types, Box::new(self.return_ty.clone()));

                self.types = types_cloned;
                self.types.insert(
                    path.clone(),
                    Source::span(ty, name.span.clone()),
                    AstItemType::Function,
                );
            }
            Declaration::Let {
                name,
                ty: let_ty,
                value,
            } => {
                let ty = self.expr(value)?;

                *let_ty = ty.clone();

                self.types.insert_ident(
                    name.data.clone(),
                    Source::span(ty, name.span.clone()),
                    AstItemType::GlobalVariable,
                );
                self.globals.push(name.data.clone());
            }
            Declaration::Struct { name, fields } => {
                let mut field_types = vec![];

                for field in fields {
                    field_types.push((field.0.data.clone(), field.1.data.clone()));
                }

                self.types.insert_ident(
                    name.data.clone(),
                    Source::span(
                        Type::Struct {
                            name: name.data.clone(),
                            fields: field_types,
                        },
                        name.span.clone(),
                    ),
                    AstItemType::Struct,
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

                self.types.insert_ident(
                    name.data.clone(),
                    Source::span(
                        Type::Fun(param_types.clone(), Box::new(result.data.clone())),
                        name.span.clone(),
                    ),
                    AstItemType::DeclareFunction,
                );
                self.globals.push(name.data.clone());
            }
            Declaration::Module(module) => {
                let current_module = self.current_module.clone();
                self.current_module = module.name.clone();
                self.module(module)?;
                self.current_module = current_module;
            }
            Declaration::Newtype { name, ty } => {
                self.types.insert_ident(
                    name.data.clone(),
                    Source::span(
                        Type::Newtype {
                            name: name.data.clone(),
                            ty: Box::new(ty.data.clone()),
                        },
                        name.span.clone(),
                    ),
                    AstItemType::Newtype,
                );
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

        // Ignore errors
        if let Err(err) = self.module(module) {
            eprintln!("skipping {}", err);
        }

        self.infer_type_at.clone()?.found
    }

    pub fn inlay_hints(
        &mut self,
        module: &mut Module,
    ) -> Result<Vec<(Span, Type)>, TypecheckerError> {
        self.inlay_hints = Some(vec![]);

        // Ignore errors
        if let Err(err) = self.module(module) {
            eprintln!("skipping {}", err);
        };

        Ok(self.inlay_hints.clone().unwrap())
    }

    pub fn completion(
        &mut self,
        module: &mut Module,
        position: usize,
    ) -> Option<Vec<(String, Type, AstItemType)>> {
        self.completion = Some(Completion {
            position,
            found: None,
        });

        // Ignore errors
        if let Err(err) = self.module(module) {
            eprintln!("skipping {}", err);
        }

        self.completion.clone()?.found
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{CompileOptions, Compiler, CompilerError};

    use anyhow::Result;
    use rayon::prelude::*;

    use super::*;

    #[test]
    fn test_typecheck_errors() -> Result<()> {
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

        cases
            .into_par_iter()
            .try_for_each(|(input, error)| -> Result<_> {
                let mut compiler = Compiler::new();

                let result = compiler.compile(
                    None,
                    input.to_string(),
                    CompileOptions {
                        no_emit: true,
                        ..Default::default()
                    },
                );

                match result.err() {
                    Some(CompilerError::TypecheckError(err)) => {
                        assert_eq!(err, error);
                    }
                    result => {
                        assert!(false, "want {:?}, but got {:?}", error, result);
                    }
                }

                Ok(())
            })?;

        Ok(())
    }
}
