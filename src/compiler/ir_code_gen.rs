use std::collections::HashMap;

use nanoid::nanoid;

use super::{
    ast::{BinOp, Block, Conversion, Declaration, Expr, Literal, Module, Source, Statement, Type},
    ir::{IrDecl, IrModule, IrOp, IrTerm, TypeTag, Value},
};

#[derive(Debug, PartialEq, Clone)]
pub enum IrCodeGeneratorError {}

#[derive(Debug)]
pub struct IrCodeGenerator {
    init_function: Vec<IrTerm>,
    globals: Vec<String>,
    types: HashMap<String, Source<Type>>,
}

impl IrCodeGenerator {
    pub fn new() -> Self {
        Self {
            init_function: vec![],
            globals: vec![],
            types: HashMap::new(),
        }
    }

    pub fn set_types(&mut self, types: HashMap<String, Source<Type>>) {
        self.types = types;
    }

    fn allocate(&self, term: IrTerm) -> IrTerm {
        IrTerm::Call {
            name: "alloc".to_string(),
            args: vec![term],
        }
    }

    fn allocate_static(&self, size: usize) -> IrTerm {
        self.allocate(IrTerm::Int(size as i32 * Value::size()))
    }

    fn slice(&self, values: Vec<IrTerm>) -> IrTerm {
        let mut block = vec![];
        let ident_name = format!("slice_{}", nanoid!());

        block.push(IrTerm::Let {
            name: ident_name.clone(),
            value: Box::new(self.allocate_static(values.len())),
        });

        for (index, term) in values.into_iter().enumerate() {
            block.push(IrTerm::Store {
                size: Value::size() as usize,
                address: Box::new(IrTerm::Index {
                    ptr: Box::new(IrTerm::Load {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Ident(ident_name.clone())),
                    }),
                    index: Box::new(IrTerm::Int(index as i32 * Value::size())),
                }),
                value: Box::new(term),
            });
        }

        block.push(IrTerm::Load {
            size: Value::size() as usize,
            address: Box::new(IrTerm::Ident(ident_name)),
        });

        IrTerm::Items(block)
    }

    pub fn module(&mut self, module: Module) -> Result<IrModule, IrCodeGeneratorError> {
        let mut decls = vec![];

        for decl in module.declarations {
            if let Some(term) = self.decl(decl)? {
                decls.push(term);
            }
        }

        // NOTE: update heap_ptr
        self.init_function.push(IrTerm::Store {
            size: Value::size() as usize,
            address: Box::new(IrTerm::Ident("heap_ptr".to_string())),
            value: Box::new(IrTerm::Op {
                op: IrOp::Cast(TypeTag::Pointer),
                args: vec![IrTerm::Int(self.globals.len() as i32 * Value::size())],
            }),
        });

        // NOTE: hoist initial process to the init function
        self.init_function
            .push(IrTerm::Return(Box::new(IrTerm::Nil)));

        decls.push(IrDecl::Fun {
            name: "init".to_string(),
            args: vec![],
            body: Box::new(IrTerm::Items(self.init_function.clone())),
        });

        Ok(IrModule {
            name: module.name,
            decls,
        })
    }

    fn decl(&mut self, decl: Source<Declaration>) -> Result<Option<IrDecl>, IrCodeGeneratorError> {
        match decl.data {
            Declaration::Function { name, params, body } => {
                let body = {
                    let term = self.block(body)?;

                    if name.data == "main" {
                        IrTerm::Items(vec![
                            IrTerm::Call {
                                name: "init".to_string(),
                                args: vec![],
                            },
                            term,
                        ])
                    } else {
                        term
                    }
                };

                Ok(Some(IrDecl::Fun {
                    name: name.data,
                    args: params.into_iter().map(|p| p.0.data).collect(),
                    body: Box::new(body),
                }))
            }
            Declaration::Let { name, ty: _, value } => {
                let value = self.expr(value)?;

                self.init_function.push(IrTerm::Store {
                    size: Value::size() as usize,
                    address: Box::new(IrTerm::Ident(name.data.clone())),
                    value: Box::new(value.clone()),
                });

                self.globals.push(name.data.clone());

                Ok(Some(IrDecl::Let { name: name.data }))
            }
            Declaration::Struct { .. } => Ok(None),
        }
    }

    pub fn expr(&self, expr: Source<Expr>) -> Result<IrTerm, IrCodeGeneratorError> {
        match expr.data {
            Expr::Lit(lit) => match lit.data {
                Literal::Nil => Ok(IrTerm::Nil),
                Literal::Integer(i) => Ok(IrTerm::Int(i.data)),
                Literal::Float(f) => Ok(IrTerm::Float(f.data)),
                Literal::Bool(b) => Ok(IrTerm::Bool(b.data)),
            },
            Expr::BinOp {
                ty,
                op,
                left,
                right,
            } => {
                let left = self.expr(*left)?;
                let right = self.expr(*right)?;
                let op = match (op.data, ty) {
                    (BinOp::Add, Type::Int) => IrOp::AddInt,
                    (BinOp::Add, Type::Float) => IrOp::AddFloat,
                    (BinOp::Add, _) => todo!(),
                    (BinOp::Sub, Type::Int) => IrOp::SubInt,
                    (BinOp::Sub, Type::Float) => IrOp::SubFloat,
                    (BinOp::Sub, _) => todo!(),
                    (BinOp::Mul, Type::Int) => IrOp::MulInt,
                    (BinOp::Mul, Type::Float) => IrOp::MulFloat,
                    (BinOp::Mul, _) => todo!(),
                    (BinOp::Div, Type::Int) => IrOp::DivInt,
                    (BinOp::Div, Type::Float) => IrOp::DivFloat,
                    (BinOp::Div, _) => todo!(),
                    (BinOp::And, Type::Bool) => IrOp::And,
                    (BinOp::And, _) => todo!(),
                    (BinOp::Or, Type::Bool) => IrOp::Or,
                    (BinOp::Or, _) => todo!(),
                    (BinOp::Eq, Type::Int) => IrOp::Eq,
                    (BinOp::Eq, Type::Float) => IrOp::Eq,
                    (BinOp::Eq, _) => todo!(),
                    (BinOp::NotEq, Type::Int) => IrOp::NotEq,
                    (BinOp::NotEq, Type::Float) => IrOp::NotEq,
                    (BinOp::NotEq, _) => todo!(),
                    (BinOp::Lt, Type::Int) => IrOp::Lt,
                    (BinOp::Lt, Type::Float) => IrOp::Lt,
                    (BinOp::Lt, _) => todo!(),
                    (BinOp::Gt, Type::Int) => IrOp::Gt,
                    (BinOp::Gt, Type::Float) => IrOp::Gt,
                    (BinOp::Gt, _) => todo!(),
                    (BinOp::Le, Type::Int) => IrOp::Le,
                    (BinOp::Le, Type::Float) => IrOp::Le,
                    (BinOp::Le, _) => todo!(),
                    (BinOp::Ge, Type::Int) => IrOp::Ge,
                    (BinOp::Ge, Type::Float) => IrOp::Ge,
                    (BinOp::Ge, _) => todo!(),
                };

                Ok(IrTerm::Op {
                    op,
                    args: vec![left, right],
                })
            }
            Expr::Call { name, args } => {
                let mut ir_args = vec![];
                for arg in args {
                    ir_args.push(self.expr(arg)?);
                }

                Ok(IrTerm::Call {
                    name: name.data,
                    args: ir_args,
                })
            }
            Expr::Match { cond, cases } => {
                // currently, cases are `true => cases[0], false => cases[1]`
                let cond = self.expr(*cond)?;

                let then = self.expr(cases[0].clone())?;
                let else_ = self.expr(cases[1].clone())?;

                Ok(IrTerm::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                })
            }
            Expr::New { ty, argument: expr } => {
                let expr = self.expr(*expr)?;

                match ty.data {
                    Type::Ptr(item) => Ok(self.allocate(IrTerm::Op {
                        op: IrOp::MulInt,
                        args: vec![expr, IrTerm::Int(item.sizeof() as i32)],
                    })),
                    Type::Array(item) => {
                        // FIXME: expr will be evaluated twice
                        Ok(self.slice(vec![
                            self.allocate(IrTerm::Op {
                                op: IrOp::MulInt,
                                args: vec![expr.clone(), IrTerm::Int(item.sizeof() as i32)],
                            }),
                            expr,
                        ]))
                    }
                    _ => todo!(),
                }
            }
            Expr::Block(block) => self.block(*block),
            Expr::Struct { name, mut fields } => {
                let struct_ty = self
                    .types
                    .get(&name.data)
                    .unwrap()
                    .data
                    .as_struct_fields()
                    .unwrap();

                fields.sort_by_key(|(name, _)| {
                    struct_ty.iter().position(|(n, _)| n == &name.data).unwrap()
                });

                let mut values = vec![];
                for (_, expr) in fields {
                    values.push(self.expr(expr)?);
                }

                Ok(self.slice(values))
            }
            Expr::As {
                expr,
                ty: _,
                conversion,
            } => {
                let expr = self.expr(*expr)?;

                let term = match conversion.unwrap() {
                    Conversion::Cast(tag) => IrOp::Cast(tag),
                    Conversion::IntToFloat => IrOp::IntToFloat,
                    Conversion::FloatToInt => IrOp::FloatToInt,
                };

                Ok(IrTerm::Op {
                    op: term,
                    args: vec![expr],
                })
            }
            Expr::Negate { expr, ty } => {
                let expr = self.expr(*expr)?;

                match ty {
                    Type::Int => Ok(IrTerm::Op {
                        op: IrOp::NegateInt,
                        args: vec![expr],
                    }),
                    Type::Float => Ok(IrTerm::Op {
                        op: IrOp::NegateFloat,
                        args: vec![expr],
                    }),
                    _ => todo!(),
                }
            }
            Expr::MethodCall {
                expr_ty,
                expr,
                name,
                args,
            } => {
                let mut ir_args = vec![self.expr(*expr)?];
                for arg in args {
                    ir_args.push(self.expr(arg)?);
                }

                let methods = Type::methods_builtin(&expr_ty);

                let name = methods
                    .iter()
                    .find(|(n, _, _)| n == &name.data)
                    .unwrap()
                    .2
                    .clone();

                Ok(IrTerm::Call {
                    name,
                    args: ir_args,
                })
            }
            Expr::Index { ty, ptr, index } => {
                let term = self.expr_left_value(Source::span(
                    Expr::Index {
                        ty: ty.clone(),
                        ptr,
                        index,
                    },
                    expr.span,
                ))?;
                let item_ty = match &ty {
                    Type::Array(item) => item,
                    Type::Ptr(item) => item,
                    _ => todo!(),
                };

                Ok(IrTerm::Op {
                    op: match item_ty.as_ref() {
                        Type::Byte => IrOp::Cast(TypeTag::Byte),
                        _ => IrOp::Cast(TypeTag::Int),
                    },
                    args: vec![IrTerm::Load {
                        size: item_ty.sizeof(),
                        address: Box::new(term),
                    }],
                })
            }
            _ => {
                let term = self.expr_left_value(expr)?;

                Ok(IrTerm::Load {
                    size: Value::size() as usize,
                    address: Box::new(term),
                })
            }
        }
    }

    fn expr_left_value(&self, expr: Source<Expr>) -> Result<IrTerm, IrCodeGeneratorError> {
        match expr.data {
            Expr::Ident(name) => Ok(IrTerm::Ident(name.data)),
            Expr::Index { ty, ptr, index } => {
                let ptr = self.expr(*ptr)?;
                let index = self.expr(*index)?;

                Ok(IrTerm::Index {
                    ptr: Box::new(match &ty {
                        Type::Ptr(_) => ptr,
                        // array.(i) -> array.ptr.(i)
                        Type::Array(_) => IrTerm::Load {
                            size: Value::size() as usize,
                            address: Box::new(IrTerm::Index {
                                ptr: Box::new(ptr),
                                index: Box::new(IrTerm::Int(0)),
                            }),
                        },
                        _ => todo!(),
                    }),
                    index: Box::new(IrTerm::Op {
                        op: IrOp::MulInt,
                        args: vec![
                            index,
                            IrTerm::Int(match &ty {
                                Type::Ptr(item) => item.sizeof(),
                                Type::Array(item) => item.sizeof(),
                                _ => todo!(),
                            } as i32),
                        ],
                    }),
                })
            }
            Expr::Project {
                expr_ty,
                expr,
                field,
            } => {
                let expr = self.expr(*expr)?;

                let struct_ty = match expr_ty {
                    Type::Ident(name) => self
                        .types
                        .get(&name)
                        .unwrap()
                        .data
                        .as_struct_fields()
                        .unwrap()
                        .clone(),
                    Type::Array(arr) => Type::fields_array(arr),
                    _ => todo!(),
                };

                let index = struct_ty
                    .iter()
                    .position(|(name, _)| name == &field.data)
                    .unwrap();

                Ok(IrTerm::Index {
                    ptr: Box::new(expr),
                    index: Box::new(IrTerm::Int(index as i32 * Value::size())),
                })
            }
            _ => todo!(),
        }
    }

    pub fn block(&self, block: Source<Block>) -> Result<IrTerm, IrCodeGeneratorError> {
        let mut terms = vec![];

        for stmt in block.data.statements {
            match stmt.data {
                Statement::Let(name, expr) => {
                    let ir = self.expr(expr)?;

                    terms.push(IrTerm::Let {
                        name: name.data,
                        value: Box::new(ir),
                    });
                }
                Statement::Return(expr) => {
                    let ir = self.expr(expr)?;

                    terms.push(IrTerm::Return(Box::new(ir)));
                }
                Statement::Expr(expr) => {
                    let ir = self.expr(expr)?;

                    terms.push(ir);
                }
                Statement::Assign(lhs, rhs) => {
                    let lhs = self.expr_left_value(lhs)?;
                    let rhs = self.expr(rhs)?;

                    terms.push(IrTerm::Store {
                        size: Value::size() as usize,
                        address: Box::new(lhs),
                        value: Box::new(rhs),
                    });
                }
                Statement::While { cond, body } => {
                    let cond = self.expr(cond)?;
                    let body = self.block(body)?;

                    terms.push(IrTerm::While {
                        cond: Box::new(cond),
                        body: Box::new(body),
                    });
                }
                Statement::If { cond, then, else_ } => {
                    let cond = self.expr(cond)?;
                    let then = self.block(then)?;
                    let else_ = match else_ {
                        Some(block) => self.block(block)?,
                        None => IrTerm::Items(vec![]),
                    };

                    terms.push(IrTerm::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                    });
                }
                Statement::Block(block) => {
                    terms.push(self.block(block)?);
                }
            }
        }

        if let Some(expr) = block.data.expr {
            terms.push(self.expr(expr)?);
        } else {
            terms.push(IrTerm::Nil);
        }

        Ok(IrTerm::Items(terms))
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{lexer::Lexer, parser::Parser, typechecker::Typechecker};

    use super::*;

    #[test]
    fn test_expr() {
        let cases = vec![
            (
                "1 + 3 * 4",
                IrTerm::Op {
                    op: IrOp::AddInt,
                    args: vec![
                        IrTerm::Int(1),
                        IrTerm::Op {
                            op: IrOp::MulInt,
                            args: vec![IrTerm::Int(3), IrTerm::Int(4)],
                        },
                    ],
                },
            ),
            (
                "1 * 3 - 4",
                IrTerm::Op {
                    op: IrOp::SubInt,
                    args: vec![
                        IrTerm::Op {
                            op: IrOp::MulInt,
                            args: vec![IrTerm::Int(1), IrTerm::Int(3)],
                        },
                        IrTerm::Int(4),
                    ],
                },
            ),
            (
                "1 > 2",
                IrTerm::Op {
                    op: IrOp::Gt,
                    args: vec![IrTerm::Int(1), IrTerm::Int(2)],
                },
            ),
        ];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let mut expr = parser.expr(true).unwrap();
            let mut typechecker = Typechecker::new();
            typechecker.expr(&mut expr).unwrap();
            let ir = gen.expr(expr).unwrap();

            assert_eq!(ir, expected);
        }
    }

    #[test]
    fn test_block() {
        let cases = vec![
            (
                "let a = 1; let b = 2; let c = a + b;",
                IrTerm::Items(vec![
                    IrTerm::Let {
                        name: "a".to_string(),
                        value: Box::new(IrTerm::Int(1)),
                    },
                    IrTerm::Let {
                        name: "b".to_string(),
                        value: Box::new(IrTerm::Int(2)),
                    },
                    IrTerm::Let {
                        name: "c".to_string(),
                        value: Box::new(IrTerm::Op {
                            op: IrOp::AddInt,
                            args: vec![
                                IrTerm::Load {
                                    size: Value::size() as usize,
                                    address: Box::new(IrTerm::Ident("a".to_string())),
                                },
                                IrTerm::Load {
                                    size: Value::size() as usize,
                                    address: Box::new(IrTerm::Ident("b".to_string())),
                                },
                            ],
                        }),
                    },
                    IrTerm::Nil,
                ]),
            ),
            (
                "let a = 1; a = 2;",
                IrTerm::Items(vec![
                    IrTerm::Let {
                        name: "a".to_string(),
                        value: Box::new(IrTerm::Int(1)),
                    },
                    IrTerm::Store {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Ident("a".to_string())),
                        value: Box::new(IrTerm::Int(2)),
                    },
                    IrTerm::Nil,
                ]),
            ),
            (
                "let a = 2; { let a = 3; a = 4; }; a",
                IrTerm::Items(vec![
                    IrTerm::Let {
                        name: "a".to_string(),
                        value: Box::new(IrTerm::Int(2)),
                    },
                    IrTerm::Items(vec![
                        IrTerm::Let {
                            name: "a".to_string(),
                            value: Box::new(IrTerm::Int(3)),
                        },
                        IrTerm::Store {
                            size: Value::size() as usize,
                            address: Box::new(IrTerm::Ident("a".to_string())),
                            value: Box::new(IrTerm::Int(4)),
                        },
                        IrTerm::Nil,
                    ]),
                    IrTerm::Load {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Ident("a".to_string())),
                    },
                ]),
            ),
        ];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let mut block = parser.block(None).unwrap();
            let mut typechecker = Typechecker::new();
            typechecker.block(&mut block).unwrap();

            let ir = gen.block(block).unwrap();

            assert_eq!(ir, expected, "input: {}", input);
        }
    }
}
