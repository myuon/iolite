use super::{
    ast::{BinOp, Expr, Literal},
    lexer::Lexeme,
};

#[derive(Debug, PartialEq)]
pub struct Parser {
    tokens: Vec<Lexeme>,
    position: usize,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEos,
}

impl Parser {
    pub fn new(tokens: Vec<Lexeme>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Result<&Lexeme, ParseError> {
        self.tokens
            .get(self.position)
            .ok_or(ParseError::UnexpectedEos)
    }

    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        self.expr_2()
    }

    pub fn expr_2(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_1()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::Plus => {
                    self.position += 1;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Add,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::Minus => {
                    self.position += 1;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Sub,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_1(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_0()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::Star => {
                    self.position += 1;
                    let right = self.expr_0()?;

                    current = Expr::BinOp {
                        op: BinOp::Mul,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::Slash => {
                    self.position += 1;
                    let right = self.expr_0()?;

                    current = Expr::BinOp {
                        op: BinOp::Div,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_0(&mut self) -> Result<Expr, ParseError> {
        match self.tokens[self.position].clone() {
            Lexeme::Integer(i) => {
                self.position += 1;

                Ok(Expr::Lit(Literal::Integer(i.clone())))
            }
            Lexeme::String(s) => {
                self.position += 1;

                Ok(Expr::Lit(Literal::String(s.clone())))
            }
            Lexeme::Ident(i) => {
                self.position += 1;

                let current = Expr::Ident(i.clone());
                if let Ok(token) = self.peek() {
                    if matches!(token, Lexeme::LParen) {
                        self.position += 1;

                        let mut args = vec![];
                        while let Ok(token) = self.peek() {
                            if matches!(token, Lexeme::RParen) {
                                self.position += 1;
                                break;
                            }

                            let arg = self.expr()?;
                            args.push(arg);

                            if let Ok(token) = self.peek() {
                                if matches!(token, Lexeme::Comma) {
                                    self.position += 1;
                                }
                            }
                        }

                        return Ok(Expr::Call {
                            name: i.clone(),
                            args,
                        });
                    }
                }

                Ok(current)
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::ast::BinOp;

    use super::*;

    #[test]
    fn test_expr() {
        let cases = vec![
            (
                "1 + 3 * 4",
                Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Lit(Literal::Integer(1))),
                    right: Box::new(Expr::BinOp {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Lit(Literal::Integer(3))),
                        right: Box::new(Expr::Lit(Literal::Integer(4))),
                    }),
                },
            ),
            (
                "1 * 3 - 4",
                Expr::BinOp {
                    op: BinOp::Sub,
                    left: Box::new(Expr::BinOp {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Lit(Literal::Integer(1))),
                        right: Box::new(Expr::Lit(Literal::Integer(3))),
                    }),
                    right: Box::new(Expr::Lit(Literal::Integer(4))),
                },
            ),
            (
                "4 * f(1, 2, 4 * 3) - 2",
                Expr::BinOp {
                    op: BinOp::Sub,
                    left: Box::new(Expr::BinOp {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Lit(Literal::Integer(4))),
                        right: Box::new(Expr::Call {
                            name: "f".to_string(),
                            args: vec![
                                Expr::Lit(Literal::Integer(1)),
                                Expr::Lit(Literal::Integer(2)),
                                Expr::BinOp {
                                    op: BinOp::Mul,
                                    left: Box::new(Expr::Lit(Literal::Integer(4))),
                                    right: Box::new(Expr::Lit(Literal::Integer(3))),
                                },
                            ],
                        }),
                    }),
                    right: Box::new(Expr::Lit(Literal::Integer(2))),
                },
            ),
        ];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.expr().unwrap();

            assert_eq!(got, expected);
        }
    }
}
