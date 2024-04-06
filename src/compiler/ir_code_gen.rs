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
                Literal::Bool(b) => Ok(IrTerm::Integer(if b { 1 } else { 0 })),
            },
            Expr::BinOp { op, left, right } => {
                let left = self.expr(*left)?;
                let right = self.expr(*right)?;
                let op = match op {
                    BinOp::Add => IrOp::Add,
                    BinOp::Sub => IrOp::Sub,
                    BinOp::Mul => IrOp::Mul,
                    BinOp::Div => IrOp::Div,
                    BinOp::And => IrOp::And,
                    BinOp::Or => IrOp::Or,
                    BinOp::Eq => IrOp::Eq,
                    BinOp::Lt => IrOp::Lt,
                    BinOp::Gt => IrOp::Gt,
                    BinOp::Le => IrOp::Le,
                    BinOp::Ge => IrOp::Ge,
                    BinOp::NotEq => IrOp::NotEq,
                };

                Ok(IrTerm::Op {
                    op,
                    args: vec![left, right],
                })
            }
            Expr::Call { name, args } => todo!(),
            Expr::If { cond, then, else_ } => {
                let cond = self.expr(*cond)?;
                let then = self.block(then)?;
                let else_ = self.block(else_)?;

                Ok(IrTerm::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                })
            }
        }
    }

    pub fn block(&self, block: Block) -> Result<IrTerm, IrCodeGeneratorError> {
        let mut terms = vec![];

        let n_statements = block.statements.len();
        for (index, stmt) in block.statements.into_iter().enumerate() {
            let is_last_statement = index == n_statements - 1;

            match stmt {
                Statement::Let(name, expr) => {
                    let ir = self.expr(expr)?;

                    terms.push(IrTerm::Let {
                        name,
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

                    if !is_last_statement || !block.has_value {
                        terms.push(IrTerm::Pop);
                    }
                }
                Statement::Assign(var, expr) => {
                    let term = self.expr(expr)?;

                    terms.push(IrTerm::Store(Box::new(IrTerm::Ident(var)), Box::new(term)));
                }
                Statement::While { cond, body } => {
                    let cond = self.expr(cond)?;
                    let body = self.block(body)?;

                    terms.push(IrTerm::While {
                        cond: Box::new(cond),
                        body: Box::new(body),
                    });
                }
                Statement::If { cond, then } => {
                    let cond = self.expr(cond)?;
                    let then = self.block(then)?;

                    terms.push(IrTerm::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(IrTerm::Block { terms: vec![] }),
                    });
                }
                Statement::Block(block) => {
                    let ir = self.block(block)?;

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
            let ir = gen.expr(parser.expr().unwrap()).unwrap();

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
                                op: IrOp::Add,
                                args: vec![
                                    IrTerm::Load(Box::new(IrTerm::Ident("a".to_string()))),
                                    IrTerm::Load(Box::new(IrTerm::Ident("b".to_string()))),
                                ],
                            }),
                        },
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
                    ],
                },
            ),
        ];

        let gen = IrCodeGenerator::new();

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer.run().unwrap());
            let ir = gen.block(parser.block(None).unwrap()).unwrap();

            assert_eq!(ir, expected, "input: {}", input);
        }
    }
}
