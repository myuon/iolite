use super::{
    ast::{BinOp, Block, Expr, Literal, Statement},
    ir::{IrOp, IrTerm},
};

#[derive(Debug)]
pub enum IrCodeGeneratorError {}

#[derive(Debug)]
pub struct IrCodeGenerator {}

impl IrCodeGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn expr(&self, expr: Expr) -> Result<IrTerm, IrCodeGeneratorError> {
        match expr {
            Expr::Ident(name) => Ok(IrTerm::Load(Box::new(IrTerm::Ident(name)))),
            Expr::Lit(lit) => match lit {
                Literal::Integer(i) => Ok(IrTerm::Integer(i)),
                Literal::String(s) => todo!(),
            },
            Expr::BinOp { op, left, right } => {
                let left = self.expr(*left)?;
                let right = self.expr(*right)?;

                match op {
                    BinOp::Add => Ok(IrTerm::Op {
                        op: IrOp::Add,
                        args: vec![left, right],
                    }),
                    BinOp::Sub => Ok(IrTerm::Op {
                        op: IrOp::Sub,
                        args: vec![left, right],
                    }),
                    BinOp::Mul => Ok(IrTerm::Op {
                        op: IrOp::Mul,
                        args: vec![left, right],
                    }),
                    BinOp::Div => Ok(IrTerm::Op {
                        op: IrOp::Div,
                        args: vec![left, right],
                    }),
                }
            }
            Expr::Call { name, args } => todo!(),
        }
    }

    pub fn block(&self, block: Block) -> Result<IrTerm, IrCodeGeneratorError> {
        let mut terms = vec![];

        for stmt in block.statements {
            match stmt {
                Statement::Let(name, expr) => {
                    let ir = self.expr(expr)?;

                    terms.push(IrTerm::Let {
                        name: name,
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
            }
        }

        Ok(IrTerm::Block { terms })
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_expr() {
        let cases = vec![
            (
                "1 + 3 * 4",
                IrTerm::Op {
                    op: IrOp::Add,
                    args: vec![
                        IrTerm::Integer(1),
                        IrTerm::Op {
                            op: IrOp::Mul,
                            args: vec![IrTerm::Integer(3), IrTerm::Integer(4)],
                        },
                    ],
                },
            ),
            (
                "1 * 3 - 4",
                IrTerm::Op {
                    op: IrOp::Sub,
                    args: vec![
                        IrTerm::Op {
                            op: IrOp::Mul,
                            args: vec![IrTerm::Integer(1), IrTerm::Integer(3)],
                        },
                        IrTerm::Integer(4),
                    ],
                },
            ),
        ];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let ir = gen.expr(parser.expr().unwrap()).unwrap();

            assert_eq!(ir, expected);
        }
    }

    #[test]
    fn test_block() {
        let cases = vec![(
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
                            op: IrOp::Add,
                            args: vec![
                                IrTerm::Load(Box::new(IrTerm::Ident("a".to_string()))),
                                IrTerm::Load(Box::new(IrTerm::Ident("b".to_string()))),
                            ],
                        }),
                    },
                ],
            },
        )];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let ir = gen.block(parser.block().unwrap()).unwrap();

            assert_eq!(ir, expected, "input: {}", input);
        }
    }
}
