use std::collections::HashMap;

use super::ast::{BinOp, Block, Declaration, Expr, Literal, Module, Source, Span, Statement, Type};

#[derive(Debug)]
pub enum TypecheckerError {
    IdentNotFound(Source<String>),
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    NumericTypeExpected(Type),
    ArgumentCountMismatch(usize, usize),
    FunctionTypeExpected(Type),
    ArrayTypeExpected(Type),
}

pub struct Typechecker {
    types: HashMap<String, Type>,
    return_ty: Type,
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            return_ty: Type::Unknown,
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

    fn get_type(&self, name: &Source<String>) -> Result<Type, TypecheckerError> {
        self.types
            .get(&name.data)
            .cloned()
            .ok_or(TypecheckerError::IdentNotFound(name.clone()))
    }

    fn expr_infer(
        &mut self,
        expr: &Source<Expr>,
        expected: Type,
    ) -> Result<Type, TypecheckerError> {
        let span = expr.span.clone();
        let actual = self.expr(expr)?;
        Self::unify(expected, actual, span)
    }

    fn expr(&mut self, expr: &Source<Expr>) -> Result<Type, TypecheckerError> {
        match &expr.data {
            Expr::Ident(i) => Ok(self.get_type(i)?),
            Expr::Lit(lit) => Ok(match lit.data {
                Literal::Bool(_) => Type::Bool,
                Literal::Integer(_) => Type::Int,
                Literal::Float(_) => Type::Float,
            }),
            Expr::BinOp { op, left, right } => {
                let left_ty = self.expr(left)?;
                let right_ty = self.expr(right)?;

                match op.data {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        let ty = Self::unify(Type::Unknown, left_ty, op.span.clone())?;
                        let result = Self::unify(ty, right_ty, op.span.clone())?;
                        match result {
                            Type::Int | Type::Float => {}
                            _ => {
                                return Err(TypecheckerError::NumericTypeExpected(result));
                            }
                        }

                        Ok(result)
                    }
                    BinOp::And | BinOp::Or => {
                        Self::unify(Type::Bool, left_ty, op.span.clone())?;
                        Self::unify(Type::Bool, right_ty, op.span.clone())?;

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

                        Ok(Type::Bool)
                    }
                }
            }
            Expr::Call { name, args } => {
                let mut arg_types_actual = vec![];

                for arg in args {
                    arg_types_actual.push(self.expr(arg)?);
                }

                let fun_ty = self.get_type(name)?;
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
            Expr::New(expr) => {
                self.expr_infer(expr, Type::Int)?;

                Ok(Type::Array(Box::new(Type::Unknown)))
            }
            Expr::Index { array, index } => {
                let array_ty = self.expr(array)?;
                self.expr_infer(index, Type::Int)?;

                match array_ty {
                    Type::Array(ty) => Ok(*ty),
                    _ => Err(TypecheckerError::ArrayTypeExpected(array_ty)),
                }
            }
        }
    }

    fn statement(&mut self, stmt: &Source<Statement>) -> Result<Type, TypecheckerError> {
        match &stmt.data {
            Statement::Let(name, value) => {
                let ty = self.expr(value)?;
                self.types.insert(name.data.clone(), ty);

                Ok(Type::Nil)
            }
            Statement::Return(expr) => {
                self.return_ty = self.expr_infer(expr, self.return_ty.clone())?;

                Ok(Type::Nil)
            }
            Statement::Expr(expr) => {
                self.expr(expr)?;

                Ok(Type::Nil)
            }
            Statement::Assign(left, right) => {
                let left_ty = self.expr(left)?;
                self.expr_infer(right, left_ty.clone())?;

                Ok(Type::Nil)
            }
            Statement::While { cond, body } => {
                self.expr_infer(cond, Type::Bool)?;
                self.block(body)?;

                Ok(Type::Nil)
            }
            Statement::If { cond, then, else_ } => {
                self.expr_infer(cond, Type::Bool)?;

                self.block_infer(then, Type::Nil)?;
                if let Some(else_) = else_ {
                    self.block_infer(else_, Type::Nil)?;
                }

                Ok(Type::Nil)
            }
            Statement::Block(block) => Ok(self.block(block)?),
        }
    }

    fn block_infer(
        &mut self,
        block: &Source<Block>,
        expected: Type,
    ) -> Result<Type, TypecheckerError> {
        let span = block.span.clone();
        let actual = self.block(block)?;
        Self::unify(expected, actual, span)
    }

    fn block(&mut self, block: &Source<Block>) -> Result<Type, TypecheckerError> {
        let mut ty = Type::Unknown;

        for stmt in &block.data.statements {
            ty = self.statement(stmt)?;
        }

        Ok(ty)
    }

    fn decl(&mut self, decl: &Source<Declaration>) -> Result<(), TypecheckerError> {
        match &decl.data {
            Declaration::Function { name, params, body } => {
                let types_cloned = self.types.clone();
                let mut param_types = vec![];

                for param in params {
                    param_types.push(Type::Unknown);
                    self.types.insert(param.data.clone(), Type::Unknown);
                }

                self.return_ty = Type::Unknown;
                self.block(body)?;

                let ty = Type::Fun(param_types, Box::new(self.return_ty.clone()));

                self.types = types_cloned;
                self.types.insert(name.data.clone(), ty);
            }
            Declaration::Let { name, value } => {
                let ty = self.expr(value)?;

                self.types.insert(name.data.clone(), ty);
            }
        }

        Ok(())
    }

    pub fn module(&mut self, module: &Module) -> Result<(), TypecheckerError> {
        for decl in &module.declarations {
            self.decl(decl)?;
        }

        Ok(())
    }
}
