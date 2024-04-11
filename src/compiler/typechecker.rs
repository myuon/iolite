use std::collections::HashMap;

use super::ast::{BinOp, Block, Declaration, Expr, Literal, Module, Statement, Type};

#[derive(Debug)]
pub enum TypecheckerError {
    IdentNotFound(String),
    TypeMismatch(Type, Type),
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

    fn unify(expected: Type, actual: Type) -> Result<Type, TypecheckerError> {
        match (&expected, &actual) {
            (Type::Unknown, ty) => Ok(ty.clone()),
            (ty, Type::Unknown) => Ok(ty.clone()),
            (a, b) if a == b => Ok(a.clone()),
            _ => Err(TypecheckerError::TypeMismatch(expected, actual)),
        }
    }

    fn get_type(&self, name: &str) -> Result<Type, TypecheckerError> {
        self.types
            .get(name)
            .cloned()
            .ok_or(TypecheckerError::IdentNotFound(name.to_string()))
    }

    fn expr(&mut self, expr: &Expr) -> Result<Type, TypecheckerError> {
        match expr {
            Expr::Ident(i) => Ok(self.get_type(i)?),
            Expr::Lit(lit) => Ok(match lit {
                Literal::Bool(_) => Type::Bool,
                Literal::Integer(_) => Type::Int,
                Literal::Float(_) => Type::Float,
                Literal::String(_) => Type::Unknown,
            }),
            Expr::BinOp { op, left, right } => {
                let left_ty = self.expr(left)?;
                let right_ty = self.expr(right)?;

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        let ty = Self::unify(Type::Unknown, left_ty)?;
                        let result = Self::unify(ty, right_ty)?;
                        match result {
                            Type::Int | Type::Float => {}
                            _ => {
                                return Err(TypecheckerError::NumericTypeExpected(result));
                            }
                        }

                        Ok(result)
                    }
                    BinOp::And | BinOp::Or => {
                        Self::unify(Type::Bool, left_ty)?;
                        Self::unify(Type::Bool, right_ty)?;

                        Ok(Type::Bool)
                    }
                    BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                        let ty = Self::unify(Type::Unknown, left_ty)?;
                        let result = Self::unify(ty, right_ty)?;
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
                            Self::unify(expected.clone(), actual)?;
                        }

                        Ok(*ret_ty)
                    }
                    _ => {
                        return Err(TypecheckerError::FunctionTypeExpected(fun_ty));
                    }
                }
            }
            Expr::Match { cond, cases } => {
                let cond_ty = self.expr(cond)?;
                Self::unify(Type::Bool, cond_ty)?;

                let mut result_ty = Type::Unknown;
                for case in cases {
                    result_ty = Self::unify(result_ty, self.block(case)?)?;
                }

                Ok(result_ty)
            }
            Expr::New(expr) => {
                Self::unify(Type::Int, self.expr(expr)?)?;

                Ok(Type::Array(Box::new(Type::Unknown)))
            }
            Expr::Index { array, index } => {
                let array_ty = self.expr(array)?;
                let index_ty = self.expr(index)?;

                Self::unify(Type::Int, index_ty)?;

                match array_ty {
                    Type::Array(ty) => Ok(*ty),
                    _ => Err(TypecheckerError::ArrayTypeExpected(array_ty)),
                }
            }
        }
    }

    fn statement(&mut self, stmt: &Statement) -> Result<Type, TypecheckerError> {
        match stmt {
            Statement::Let(name, value) => {
                let ty = self.expr(value)?;
                self.types.insert(name.clone(), ty);

                Ok(Type::Nil)
            }
            Statement::Return(expr) => {
                let ty = self.expr(expr)?;

                self.return_ty = Self::unify(self.return_ty.clone(), ty)?;

                Ok(self.return_ty.clone())
            }
            Statement::Expr(expr) => {
                self.expr(expr)?;

                Ok(Type::Nil)
            }
            Statement::Assign(left, right) => {
                let left_ty = self.expr(left)?;
                let right_ty = self.expr(right)?;

                Self::unify(left_ty, right_ty)?;

                Ok(Type::Nil)
            }
            Statement::While { cond, body } => {
                let cond_ty = self.expr(cond)?;
                Self::unify(Type::Bool, cond_ty)?;

                self.block(body)?;

                Ok(Type::Nil)
            }
            Statement::If { cond, then, else_ } => {
                let cond_ty = self.expr(cond)?;
                Self::unify(Type::Bool, cond_ty)?;

                Self::unify(Type::Nil, self.block(then)?)?;
                if let Some(else_) = else_ {
                    Self::unify(Type::Nil, self.block(else_)?)?;
                }

                Ok(Type::Nil)
            }
            Statement::Block(block) => Ok(self.block(block)?),
        }
    }

    fn block(&mut self, block: &Block) -> Result<Type, TypecheckerError> {
        let mut ty = Type::Unknown;

        for stmt in &block.statements {
            ty = self.statement(stmt)?;
        }

        Ok(ty)
    }

    fn decl(&mut self, decl: &Declaration) -> Result<(), TypecheckerError> {
        match decl {
            Declaration::Function { name, params, body } => {
                let types_cloned = self.types.clone();
                let mut param_types = vec![];

                for param in params {
                    param_types.push(Type::Unknown);
                    self.types.insert(param.clone(), Type::Unknown);
                }

                self.return_ty = Type::Unknown;
                let result_ty = self.block(body)?;

                let ty = Type::Fun(param_types, Box::new(result_ty));

                self.types = types_cloned;
                self.types.insert(name.clone(), ty);
            }
            Declaration::Let { name, value } => {
                let ty = self.expr(value)?;

                self.types.insert(name.clone(), ty);
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
