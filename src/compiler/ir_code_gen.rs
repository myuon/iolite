use super::{
    ast::{BinOp, Block, Declaration, Expr, Literal, Module, Source, Statement, Type},
    ir::{IrDecl, IrModule, IrOp, IrTerm},
};

#[derive(Debug)]
pub enum IrCodeGeneratorError {}

#[derive(Debug)]
pub struct IrCodeGenerator {
    init_function: Vec<IrTerm>,
}

impl IrCodeGenerator {
    pub fn new() -> Self {
        Self {
            init_function: vec![],
        }
    }

    pub fn module(&mut self, module: Module) -> Result<IrModule, IrCodeGeneratorError> {
        let mut decls = vec![];

        for decl in module.declarations {
            decls.push(self.decl(decl)?);
        }

        // NOTE: hoist initial process to the init function
        self.init_function
            .push(IrTerm::Return(Box::new(IrTerm::Nil)));

        decls.push(IrDecl::Fun {
            name: "init".to_string(),
            args: vec![],
            body: Box::new(IrTerm::Block {
                terms: self.init_function.clone(),
            }),
        });

        Ok(IrModule {
            name: module.name,
            decls,
        })
    }

    fn decl(&mut self, decl: Source<Declaration>) -> Result<IrDecl, IrCodeGeneratorError> {
        match decl.data {
            Declaration::Function { name, params, body } => {
                let body = {
                    let term = self.block(body)?;

                    if name.data == "main" {
                        IrTerm::Block {
                            terms: vec![
                                IrTerm::Call {
                                    name: "init".to_string(),
                                    args: vec![],
                                },
                                term,
                            ],
                        }
                    } else {
                        term
                    }
                };

                Ok(IrDecl::Fun {
                    name: name.data,
                    args: params.into_iter().map(|p| p.0.data).collect(),
                    body: Box::new(body),
                })
            }
            Declaration::Let { name, value } => {
                let value = self.expr(value)?;

                self.init_function.push(IrTerm::Store(
                    Box::new(IrTerm::Ident(name.data.clone())),
                    Box::new(value.clone()),
                ));

                Ok(IrDecl::Let { name: name.data })
            }
        }
    }

    pub fn expr(&self, expr: Source<Expr>) -> Result<IrTerm, IrCodeGeneratorError> {
        match expr.data {
            Expr::Lit(lit) => match lit.data {
                Literal::Integer(i) => Ok(IrTerm::Integer(i.data)),
                Literal::Float(f) => Ok(IrTerm::Float(f.data)),
                Literal::Bool(b) => Ok(IrTerm::Integer(if b.data { 1 } else { 0 })),
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
                    (BinOp::Add, p) => todo!("{:?}", p),
                    (BinOp::Sub, Type::Int) => IrOp::SubInt,
                    (BinOp::Sub, Type::Float) => IrOp::SubFloat,
                    (BinOp::Sub, p) => todo!("{:?}", p),
                    (BinOp::Mul, Type::Int) => IrOp::MulInt,
                    (BinOp::Mul, Type::Float) => IrOp::MulFloat,
                    (BinOp::Mul, p) => todo!("{:?}", p),
                    (BinOp::Div, Type::Int) => IrOp::DivInt,
                    (BinOp::Div, Type::Float) => IrOp::DivFloat,
                    (BinOp::Div, p) => todo!("{:?}", p),
                    (BinOp::And, _) => IrOp::And,
                    (BinOp::Or, _) => IrOp::Or,
                    (BinOp::Eq, _) => IrOp::Eq,
                    (BinOp::Lt, _) => IrOp::Lt,
                    (BinOp::Gt, _) => IrOp::Gt,
                    (BinOp::Le, _) => IrOp::Le,
                    (BinOp::Ge, _) => IrOp::Ge,
                    (BinOp::NotEq, _) => IrOp::NotEq,
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
            Expr::New(expr) => {
                let expr = self.expr(*expr)?;

                Ok(IrTerm::Call {
                    name: "alloc".to_string(),
                    args: vec![expr],
                })
            }
            _ => Ok(IrTerm::Load(Box::new(self.expr_left_value(expr)?))),
        }
    }

    fn expr_left_value(&self, expr: Source<Expr>) -> Result<IrTerm, IrCodeGeneratorError> {
        match expr.data {
            Expr::Ident(name) => Ok(IrTerm::Ident(name.data)),
            Expr::Index { array, index } => {
                let array = self.expr(*array)?;
                let index = self.expr(*index)?;

                Ok(IrTerm::Index {
                    array: Box::new(array),
                    index: Box::new(IrTerm::Op {
                        op: IrOp::MulInt,
                        args: vec![index, IrTerm::Integer(4)],
                    }),
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

                    terms.push(IrTerm::Store(Box::new(lhs), Box::new(rhs)));
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
                        None => IrTerm::Block { terms: vec![] },
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

        Ok(IrTerm::Block { terms })
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
                        IrTerm::Integer(1),
                        IrTerm::Op {
                            op: IrOp::MulInt,
                            args: vec![IrTerm::Integer(3), IrTerm::Integer(4)],
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
                            args: vec![IrTerm::Integer(1), IrTerm::Integer(3)],
                        },
                        IrTerm::Integer(4),
                    ],
                },
            ),
            (
                "1 > 2",
                IrTerm::Op {
                    op: IrOp::Gt,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
            ),
        ];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let mut expr = parser.expr().unwrap();
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
                IrTerm::Block {
                    terms: vec![
                        IrTerm::Let {
                            name: "a".to_string(),
                            value: Box::new(IrTerm::Integer(1)),
                        },
                        IrTerm::Let {
                            name: "b".to_string(),
                            value: Box::new(IrTerm::Integer(2)),
                        },
                        IrTerm::Let {
                            name: "c".to_string(),
                            value: Box::new(IrTerm::Op {
                                op: IrOp::AddInt,
                                args: vec![
                                    IrTerm::Load(Box::new(IrTerm::Ident("a".to_string()))),
                                    IrTerm::Load(Box::new(IrTerm::Ident("b".to_string()))),
                                ],
                            }),
                        },
                        IrTerm::Nil,
                    ],
                },
            ),
            (
                "let a = 1; a = 2;",
                IrTerm::Block {
                    terms: vec![
                        IrTerm::Let {
                            name: "a".to_string(),
                            value: Box::new(IrTerm::Integer(1)),
                        },
                        IrTerm::Store(
                            Box::new(IrTerm::Ident("a".to_string())),
                            Box::new(IrTerm::Integer(2)),
                        ),
                        IrTerm::Nil,
                    ],
                },
            ),
            (
                "let a = 2; { let a = 3; a = 4; }; a",
                IrTerm::Block {
                    terms: vec![
                        IrTerm::Let {
                            name: "a".to_string(),
                            value: Box::new(IrTerm::Integer(2)),
                        },
                        IrTerm::Block {
                            terms: vec![
                                IrTerm::Let {
                                    name: "a".to_string(),
                                    value: Box::new(IrTerm::Integer(3)),
                                },
                                IrTerm::Store(
                                    Box::new(IrTerm::Ident("a".to_string())),
                                    Box::new(IrTerm::Integer(4)),
                                ),
                                IrTerm::Nil,
                            ],
                        },
                        IrTerm::Load(Box::new(IrTerm::Ident("a".to_string()))),
                    ],
                },
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
