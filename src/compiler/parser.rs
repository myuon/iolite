use super::{
    ast::{BinOp, Block, Expr, Literal, Statement},
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
    UnexpectedToken { expected: Lexeme, got: Lexeme },
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

    fn consume(&mut self) -> Result<Lexeme, ParseError> {
        let token = self.peek()?.clone();
        self.position += 1;

        Ok(token)
    }

    fn ident(&mut self) -> Result<String, ParseError> {
        let token = self.consume()?;
        match token {
            Lexeme::Ident(i) => Ok(i),
            _ => Err(ParseError::UnexpectedToken {
                expected: Lexeme::Ident("".to_string()),
                got: token,
            }),
        }
    }

    fn expect(&mut self, lexeme: Lexeme) -> Result<(), ParseError> {
        let token = self.peek()?;
        if token == &lexeme {
            self.consume()?;
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: lexeme,
                got: token.clone(),
            })
        }
    }

    pub fn block(&mut self) -> Result<Block, ParseError> {
        let mut block = vec![];

        while self.position < self.tokens.len() {
            block.push(self.statement()?);
            self.expect(Lexeme::Semicolon)?;
        }

        Ok(Block { statements: block })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek()? {
            Lexeme::Let => {
                self.consume()?;

                let ident = self.ident()?;

                self.expect(Lexeme::Equal)?;

                let expr = self.expr()?;

                Ok(Statement::Let(ident, expr))
            }
            Lexeme::Return => {
                self.consume()?;

                let expr = self.expr()?;

                Ok(Statement::Return(expr))
            }
            _ => {
                let expr = self.expr()?;

                if let Ok(token) = self.peek() {
                    match token {
                        Lexeme::Equal => {
                            if let Expr::Ident(ident) = expr {
                                self.consume()?;
                                let right = self.expr()?;
                                return Ok(Statement::Assign(ident, right));
                            }
                        }
                        _ => (),
                    }
                }

                Ok(Statement::Expr(expr))
            }
        }
    }

    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        self.expr_5()
    }

    fn expr_5(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_4()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::DoubleOr => {
                    self.consume()?;
                    let right = self.expr_4()?;

                    current = Expr::BinOp {
                        op: BinOp::Or,
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

    fn expr_4(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_3()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::DoubleAnd => {
                    self.consume()?;
                    let right = self.expr_3()?;

                    current = Expr::BinOp {
                        op: BinOp::And,
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

    fn expr_3(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_2()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::Le => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Le,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::Ge => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Ge,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::LAngle => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Lt,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::GAngle => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Gt,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::DoubleEqual => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Eq,
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

    fn expr_2(&mut self) -> Result<Expr, ParseError> {
        let mut current = self.expr_1()?;

        while self.position < self.tokens.len() {
            match &self.tokens[self.position] {
                Lexeme::Plus => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::Add,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::Minus => {
                    self.consume()?;
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
                    self.consume()?;
                    let right = self.expr_0()?;

                    current = Expr::BinOp {
                        op: BinOp::Mul,
                        left: Box::new(current),
                        right: Box::new(right),
                    };
                }
                Lexeme::Slash => {
                    self.consume()?;
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
            Lexeme::LParen => {
                self.position += 1;

                let current = self.expr()?;

                self.expect(Lexeme::RParen)?;

                Ok(current)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: Lexeme::Integer(0),
                got: self.tokens[self.position].clone(),
            }),
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
            (
                "1 <= 2 || 3 >= 4 && 1 + 2 == 4",
                Expr::BinOp {
                    op: BinOp::Or,
                    left: Box::new(Expr::BinOp {
                        op: BinOp::Le,
                        left: Box::new(Expr::Lit(Literal::Integer(1))),
                        right: Box::new(Expr::Lit(Literal::Integer(2))),
                    }),
                    right: Box::new(Expr::BinOp {
                        op: BinOp::And,
                        left: Box::new(Expr::BinOp {
                            op: BinOp::Ge,
                            left: Box::new(Expr::Lit(Literal::Integer(3))),
                            right: Box::new(Expr::Lit(Literal::Integer(4))),
                        }),
                        right: Box::new(Expr::BinOp {
                            op: BinOp::Eq,
                            left: Box::new(Expr::BinOp {
                                op: BinOp::Add,
                                left: Box::new(Expr::Lit(Literal::Integer(1))),
                                right: Box::new(Expr::Lit(Literal::Integer(2))),
                            }),
                            right: Box::new(Expr::Lit(Literal::Integer(4))),
                        }),
                    }),
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

    #[test]
    fn test_statement() {
        let cases = vec![
            (
                "let x = 1 + 2",
                Statement::Let(
                    "x".to_string(),
                    Expr::BinOp {
                        op: BinOp::Add,
                        left: Box::new(Expr::Lit(Literal::Integer(1))),
                        right: Box::new(Expr::Lit(Literal::Integer(2))),
                    },
                ),
            ),
            (
                "return 1 + 2",
                Statement::Return(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Lit(Literal::Integer(1))),
                    right: Box::new(Expr::Lit(Literal::Integer(2))),
                }),
            ),
            (
                "a = b;",
                Statement::Assign("a".to_string(), Expr::Ident("b".to_string())),
            ),
        ];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.statement().unwrap();

            assert_eq!(got, expected);
        }
    }

    #[test]
    fn test_block() {
        let cases = vec![
            (
                "let x = 1 + 2; let y = 3 * 4; let z = x + y;",
                Block {
                    statements: vec![
                        Statement::Let(
                            "x".to_string(),
                            Expr::BinOp {
                                op: BinOp::Add,
                                left: Box::new(Expr::Lit(Literal::Integer(1))),
                                right: Box::new(Expr::Lit(Literal::Integer(2))),
                            },
                        ),
                        Statement::Let(
                            "y".to_string(),
                            Expr::BinOp {
                                op: BinOp::Mul,
                                left: Box::new(Expr::Lit(Literal::Integer(3))),
                                right: Box::new(Expr::Lit(Literal::Integer(4))),
                            },
                        ),
                        Statement::Let(
                            "z".to_string(),
                            Expr::BinOp {
                                op: BinOp::Add,
                                left: Box::new(Expr::Ident("x".to_string())),
                                right: Box::new(Expr::Ident("y".to_string())),
                            },
                        ),
                    ],
                },
            ),
            (
                "return 1 + 2; return 3 * 4;",
                Block {
                    statements: vec![
                        Statement::Return(Expr::BinOp {
                            op: BinOp::Add,
                            left: Box::new(Expr::Lit(Literal::Integer(1))),
                            right: Box::new(Expr::Lit(Literal::Integer(2))),
                        }),
                        Statement::Return(Expr::BinOp {
                            op: BinOp::Mul,
                            left: Box::new(Expr::Lit(Literal::Integer(3))),
                            right: Box::new(Expr::Lit(Literal::Integer(4))),
                        }),
                    ],
                },
            ),
        ];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.block().unwrap();

            assert_eq!(got, expected);
        }
    }
}
