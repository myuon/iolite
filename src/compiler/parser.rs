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
    ExpressionExpected { got: Statement },
    ElseExpected { cond: Expr, then: Block },
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

    pub fn block(&mut self, end_token: Option<Lexeme>) -> Result<Block, ParseError> {
        let mut block = vec![];
        let mut has_value = false;

        while self.position < self.tokens.len() {
            if let Some(end_token) = &end_token {
                if self.peek()? == end_token {
                    break;
                }
            }

            let statement = self.statement()?;
            let needs_semilon = match &statement {
                Statement::While { .. } | Statement::If { .. } | Statement::Block(_) => false,
                _ => true,
            };

            block.push(statement.clone());
            if needs_semilon {
                // NOTE: allows eos here
                if matches!(self.peek(), Ok(Lexeme::Semicolon)) {
                    self.consume()?;
                } else {
                    if !matches!(statement, Statement::Expr(_)) {
                        return Err(ParseError::ExpressionExpected { got: statement });
                    }

                    has_value = true;

                    break;
                }
            } else {
                if let Ok(token) = self.peek() {
                    if matches!(token, Lexeme::Semicolon) {
                        self.consume()?;
                    }
                }
            }
        }

        Ok(Block {
            statements: block,
            has_value,
        })
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
            Lexeme::While => {
                self.consume()?;

                self.expect(Lexeme::LParen)?;
                let expr = self.expr()?;
                self.expect(Lexeme::RParen)?;

                self.expect(Lexeme::LBrace)?;
                let block = self.block(Some(Lexeme::RBrace))?;
                self.expect(Lexeme::RBrace)?;

                Ok(Statement::While {
                    cond: expr,
                    body: block,
                })
            }
            Lexeme::LBrace => {
                self.consume()?;

                let block = self.block(Some(Lexeme::RBrace))?;

                self.expect(Lexeme::RBrace)?;

                Ok(Statement::Block(block))
            }
            _ => {
                let expr = match self.expr() {
                    Ok(expr) => expr,
                    Err(ParseError::ElseExpected { cond, then }) => {
                        return Ok(Statement::If { cond, then });
                    }
                    Err(e) => return Err(e),
                };

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
        let token = self.peek()?;
        match token {
            Lexeme::If => {
                self.consume()?;
                let cond = self.expr()?;

                self.expect(Lexeme::LBrace)?;
                let then_block = self.block(Some(Lexeme::RBrace))?;
                self.expect(Lexeme::RBrace)?;

                self.expect(Lexeme::Else)
                    .map_err(|_| ParseError::ElseExpected {
                        cond: cond.clone(),
                        then: then_block.clone(),
                    })?;

                if matches!(self.peek(), Ok(Lexeme::If)) {
                    let next_if = self.expr()?;

                    Ok(Expr::If {
                        cond: Box::new(cond),
                        then: then_block,
                        else_: Block {
                            statements: vec![Statement::Expr(next_if)],
                            has_value: true,
                        },
                    })
                } else {
                    self.expect(Lexeme::LBrace)?;
                    let else_block = self.block(Some(Lexeme::RBrace))?;
                    self.expect(Lexeme::RBrace)?;

                    Ok(Expr::If {
                        cond: Box::new(cond),
                        then: then_block,
                        else_: else_block,
                    })
                }
            }
            _ => self.expr_5(),
        }
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
                Lexeme::NotEqual => {
                    self.consume()?;
                    let right = self.expr_2()?;

                    current = Expr::BinOp {
                        op: BinOp::NotEq,
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
                self.consume()?;

                Ok(Expr::Lit(Literal::Integer(i.clone())))
            }
            Lexeme::String(s) => {
                self.consume()?;

                Ok(Expr::Lit(Literal::String(s.clone())))
            }
            Lexeme::True => {
                self.consume()?;

                Ok(Expr::Lit(Literal::Bool(true)))
            }
            Lexeme::False => {
                self.consume()?;

                Ok(Expr::Lit(Literal::Bool(false)))
            }
            Lexeme::Ident(i) => {
                self.consume()?;

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
                expected: Lexeme::String("".to_string()),
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
                    has_value: false,
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
                    has_value: false,
                },
            ),
            (
                "while (true) { let a = 1; let b = 2; }",
                Block {
                    statements: vec![Statement::While {
                        cond: Expr::Lit(Literal::Bool(true)),
                        body: Block {
                            statements: vec![
                                Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(1))),
                                Statement::Let("b".to_string(), Expr::Lit(Literal::Integer(2))),
                            ],
                            has_value: false,
                        },
                    }],
                    has_value: false,
                },
            ),
            (
                "let a = 1; 10",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(1))),
                        Statement::Expr(Expr::Lit(Literal::Integer(10))),
                    ],
                    has_value: true,
                },
            ),
            (
                "let a = 1; 10;",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(1))),
                        Statement::Expr(Expr::Lit(Literal::Integer(10))),
                    ],
                    has_value: false,
                },
            ),
            (
                "let a = 2; { let a = 3; a = 4; }; a",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(2))),
                        Statement::Block(Block {
                            statements: vec![
                                Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(3))),
                                Statement::Assign("a".to_string(), Expr::Lit(Literal::Integer(4))),
                            ],
                            has_value: false,
                        }),
                        Statement::Expr(Expr::Ident("a".to_string())),
                    ],
                    has_value: true,
                },
            ),
            (
                "let a = 2; { let a = 3; a = 4; } a",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(2))),
                        Statement::Block(Block {
                            statements: vec![
                                Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(3))),
                                Statement::Assign("a".to_string(), Expr::Lit(Literal::Integer(4))),
                            ],
                            has_value: false,
                        }),
                        Statement::Expr(Expr::Ident("a".to_string())),
                    ],
                    has_value: true,
                },
            ),
        ];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.block(None).unwrap();

            assert_eq!(got, expected);
        }
    }
}
