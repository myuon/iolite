use super::{
    ast::{BinOp, Block, Declaration, Expr, Literal, Source, Span, Statement, Type},
    lexer::{Lexeme, Token},
};

#[derive(Debug, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedEos,
    UnexpectedToken {
        expected: Option<Lexeme>,
        got: Token,
    },
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Result<&Token, ParseError> {
        self.tokens
            .get(self.position)
            .ok_or(ParseError::UnexpectedEos)
    }

    fn is_next_token(&self, lexeme: Lexeme) -> bool {
        if let Ok(token) = self.peek() {
            return token.lexeme == lexeme;
        }

        false
    }

    fn consume(&mut self) -> Result<Token, ParseError> {
        let token = self.peek()?.clone();
        self.position += 1;

        Ok(token)
    }

    fn is_ident(&mut self) -> bool {
        if let Ok(token) = self.peek() {
            return matches!(token.lexeme, Lexeme::Ident(_));
        }

        false
    }

    fn ident(&mut self) -> Result<Source<String>, ParseError> {
        let token = self.consume()?;
        match token.lexeme {
            Lexeme::Ident(i) => Ok(Source::span(i, token.span)),
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                got: token,
            }),
        }
    }

    fn expect(&mut self, lexeme: Lexeme) -> Result<Token, ParseError> {
        let token = self.peek()?;
        if token.lexeme == lexeme {
            Ok(self.consume()?)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: Some(lexeme),
                got: token.clone(),
            })
        }
    }

    pub fn decls(&mut self) -> Result<Vec<Source<Declaration>>, ParseError> {
        let mut decls = vec![];

        while self.peek().is_ok() {
            let decl = self.decl()?;

            decls.push(decl);
        }

        Ok(decls)
    }

    fn ty(&mut self) -> Result<Source<Type>, ParseError> {
        let token = self.consume()?;
        match token.lexeme {
            Lexeme::Ident(i) if i == "int".to_string() => Ok(Source::span(Type::Int, token.span)),
            Lexeme::Ident(i) if i == "bool".to_string() => Ok(Source::span(Type::Bool, token.span)),
            Lexeme::Ident(i) if i == "byte".to_string() => Ok(Source::span(Type::Byte, token.span)),
            Lexeme::Ident(i) if i == "float".to_string() => {
                Ok(Source::span(Type::Float, token.span))
            }
            Lexeme::Ident(i) if i == "ptr".to_string() => {
                self.expect(Lexeme::LBracket)?;
                let t = self.ty()?;
                self.expect(Lexeme::RBracket)?;

                Ok(Source::span(Type::Ptr(Box::new(t.data)), token.span))
            }
            Lexeme::Ident(i) if i == "array".to_string() => {
                self.expect(Lexeme::LBracket)?;
                let t = self.ty()?;
                self.expect(Lexeme::RBracket)?;

                Ok(Source::span(Type::Array(Box::new(t.data)), token.span))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                got: token,
            }),
        }
    }

    fn arity_decl(&mut self) -> Result<Vec<(Source<String>, Source<Type>)>, ParseError> {
        let mut args = vec![];

        while let Ok(token) = self.peek() {
            if matches!(token.lexeme, Lexeme::RParen) {
                break;
            }

            let arg = self.ident()?;
            self.expect(Lexeme::Colon)?;
            let ty = self.ty()?;
            args.push((arg, ty));

            if matches!(self.peek().map(|t| &t.lexeme), Ok(&Lexeme::Comma)) {
                self.consume()?;
            } else {
                break;
            }
        }

        Ok(args)
    }

    pub fn decl(&mut self) -> Result<Source<Declaration>, ParseError> {
        let token = self.peek()?;
        match token.lexeme {
            Lexeme::Fun => {
                let start_token = self.consume()?;

                let name = self.ident()?;
                self.expect(Lexeme::LParen)?;
                let params = self.arity_decl()?;
                self.expect(Lexeme::RParen)?;

                self.expect(Lexeme::LBrace)?;
                let block = self.block(Some(Lexeme::RBrace))?;
                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Declaration::Function {
                        name,
                        params,
                        body: block,
                    },
                    start_token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::Let => {
                let start_token = self.consume()?;

                let name = self.ident()?;
                self.expect(Lexeme::Equal)?;
                let expr = self.expr(true)?;

                let end_token = self.expect(Lexeme::Semicolon)?;

                Ok(Source::new_span(
                    Declaration::Let { name, value: expr },
                    start_token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::Struct => {
                let start_token = self.consume()?;

                let name = self.ident()?;
                self.expect(Lexeme::LBrace)?;

                let mut fields = vec![];
                while let Ok(token) = self.peek() {
                    if matches!(token.lexeme, Lexeme::RBrace) {
                        break;
                    }

                    let field = self.ident()?;
                    self.expect(Lexeme::Colon)?;
                    let ty = self.ty()?;
                    fields.push((field, ty));

                    if matches!(self.peek().map(|t| &t.lexeme), Ok(&Lexeme::Comma)) {
                        self.consume()?;
                    } else {
                        break;
                    }
                }

                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Declaration::Struct { name, fields },
                    start_token.span.start,
                    end_token.span.end,
                ))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: Some(Lexeme::Fun),
                got: token.clone(),
            }),
        }
    }

    pub fn block(&mut self, end_token: Option<Lexeme>) -> Result<Source<Block>, ParseError> {
        let mut block = vec![];
        let mut span = Span::unknown();
        let mut last_expr = None;

        while self.position < self.tokens.len() {
            if let Some(end_token) = &end_token {
                if &self.peek()?.lexeme == end_token {
                    break;
                }
            }

            let statement = self.statement()?;
            if span.start.is_none() {
                span.start = statement.span.start;
            }
            // NOTE: is this correct?
            span.end = statement.span.end;

            match statement.data {
                Statement::While { .. } | Statement::If { .. } | Statement::Block(_) => {
                    if self.is_next_token(Lexeme::Semicolon) {
                        self.expect(Lexeme::Semicolon)?;
                    }

                    block.push(statement);
                    continue;
                }
                Statement::Expr(expr) => {
                    if self.is_next_token(Lexeme::Semicolon) {
                        self.expect(Lexeme::Semicolon)?;
                        block.push(Source::span(Statement::Expr(expr), statement.span));
                        continue;
                    }

                    last_expr = Some(expr);
                    break;
                }
                _ => {
                    self.expect(Lexeme::Semicolon)?;
                    block.push(statement.clone());
                    continue;
                }
            };
        }

        Ok(Source::span(
            Block {
                statements: block,
                expr: last_expr,
            },
            span,
        ))
    }

    fn statement(&mut self) -> Result<Source<Statement>, ParseError> {
        let token = self.peek()?.clone();
        match token.lexeme {
            Lexeme::Let => {
                self.consume()?;

                let ident = self.ident()?;

                self.expect(Lexeme::Equal)?;

                let expr = self.expr(true)?;
                let end = expr.span.end;

                Ok(Source::new_span(
                    Statement::Let(ident, expr),
                    token.span.start,
                    end,
                ))
            }
            Lexeme::Return => {
                self.consume()?;

                let expr = self.expr(true)?;
                let end = expr.span.end;

                Ok(Source::new_span(
                    Statement::Return(expr),
                    token.span.start,
                    end,
                ))
            }
            Lexeme::While => {
                self.consume()?;

                self.expect(Lexeme::LParen)?;
                let expr = self.expr(false)?;
                self.expect(Lexeme::RParen)?;

                self.expect(Lexeme::LBrace)?;
                let block = self.block(Some(Lexeme::RBrace))?;
                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Statement::While {
                        cond: expr,
                        body: block,
                    },
                    token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::LBrace => {
                self.consume()?;

                let block = self.block(Some(Lexeme::RBrace))?;

                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Statement::Block(block),
                    token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::If => {
                self.consume()?;
                let cond = self.expr(false)?;

                self.expect(Lexeme::LBrace)?;
                let then_block = self.block(Some(Lexeme::RBrace))?;
                self.expect(Lexeme::RBrace)?;

                if !matches!(self.peek().map(|t| &t.lexeme), Ok(Lexeme::Else)) {
                    let end = then_block.span.end;
                    let s = Statement::If {
                        cond,
                        then: then_block,
                        else_: None,
                    };

                    return Ok(Source::new_span(s, token.span.start, end));
                }

                self.expect(Lexeme::Else)?;

                if matches!(self.peek().map(|t| &t.lexeme), Ok(Lexeme::If)) {
                    let next_if = self.statement()?;
                    let span = next_if.span.clone();
                    let end = span.end;

                    let s = Statement::If {
                        cond,
                        then: then_block,
                        else_: Some(Source::span(
                            Block {
                                statements: vec![next_if],
                                expr: None,
                            },
                            span,
                        )),
                    };

                    Ok(Source::new_span(s, token.span.start, end))
                } else {
                    self.expect(Lexeme::LBrace)?;
                    let else_block = self.block(Some(Lexeme::RBrace))?;
                    self.expect(Lexeme::RBrace)?;

                    let end = else_block.span.end;
                    let s = Statement::If {
                        cond,
                        then: then_block,
                        else_: Some(else_block),
                    };

                    Ok(Source::new_span(s, token.span.start, end))
                }
            }
            _ => {
                let expr = self.expr(true)?;
                let span = expr.span.clone();

                if let Ok(token) = self.peek() {
                    match token.lexeme {
                        Lexeme::Equal => {
                            self.consume()?;
                            let right = self.expr(true)?;

                            let start = expr.span.start;
                            let end = right.span.end;
                            return Ok(Source::new_span(
                                Statement::Assign(expr, right),
                                start,
                                end,
                            ));
                        }
                        _ => (),
                    }
                }

                Ok(Source::span(Statement::Expr(expr), span))
            }
        }
    }

    pub fn expr(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let token = self.peek()?;
        match token.lexeme {
            Lexeme::Match => {
                let start_token = self.consume()?;
                let cond = self.expr(with_struct)?;

                self.expect(Lexeme::LBrace)?;

                self.expect(Lexeme::True)?;
                self.expect(Lexeme::Arrow)?;
                let true_case = self.expr(with_struct)?;
                self.expect(Lexeme::Comma)?;

                self.expect(Lexeme::False)?;
                self.expect(Lexeme::Arrow)?;
                let false_case = self.expr(with_struct)?;

                if matches!(self.peek().map(|t| &t.lexeme), Ok(Lexeme::Comma)) {
                    self.consume()?;
                }

                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Expr::Match {
                        cond: Box::new(cond),
                        cases: vec![true_case, false_case],
                    },
                    start_token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::New => {
                self.consume()?;
                self.expect(Lexeme::LBracket)?;
                let ty = self.ty()?;
                self.expect(Lexeme::RBracket)?;

                self.expect(Lexeme::LParen)?;
                let expr = self.expr(with_struct)?;
                self.expect(Lexeme::RParen)?;

                let span = expr.span.clone();

                Ok(Source::span(
                    Expr::New {
                        ty,
                        argument: Box::new(expr),
                    },
                    span,
                ))
            }
            _ => Ok(self.expr_5(with_struct)?),
        }
    }

    fn expr_5(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_4(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::DoubleOr => {
                    self.consume()?;
                    let right = self.expr_4(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Or, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_4(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_3(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::DoubleAnd => {
                    self.consume()?;
                    let right = self.expr_3(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::And, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_3(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_2(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::Le => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Le, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::Ge => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Ge, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::LAngle => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Lt, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::GAngle => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Gt, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::DoubleEqual => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Eq, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::NotEqual => {
                    self.consume()?;
                    let right = self.expr_2(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::NotEq, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_2(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_1(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::Plus => {
                    self.consume()?;
                    let right = self.expr_1(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Add, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::Minus => {
                    self.consume()?;
                    let right = self.expr_1(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Sub, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_1(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_0_5(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::Star => {
                    self.consume()?;
                    let right = self.expr_0_5(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Mul, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                Lexeme::Slash => {
                    self.consume()?;
                    let right = self.expr_0_5(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Div, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        start,
                        end,
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_0_5(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_0(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::As => {
                    self.consume()?;

                    let ty = self.ty()?;

                    let start = current.span.start;
                    let end = ty.span.end;

                    current = Source::new_span(
                        Expr::As {
                            expr: Box::new(current),
                            ty,
                            conversion: None,
                        },
                        start,
                        end,
                    );
                }
                Lexeme::LParen => {
                    self.position += 1;
                    let mut end = None;

                    let mut args = vec![];
                    while let Ok(token) = self.peek() {
                        if matches!(token.lexeme, Lexeme::RParen) {
                            end = self.consume()?.span.end;

                            break;
                        }

                        let arg = self.expr(with_struct)?;
                        args.push(arg);

                        if let Ok(token) = self.peek() {
                            if matches!(token.lexeme, Lexeme::Comma) {
                                self.position += 1;
                            }
                        }
                    }

                    let start = current.span.start;

                    match current.data {
                        Expr::Project {
                            expr_ty: _,
                            expr,
                            field,
                        } => {
                            current = Source::new_span(
                                Expr::MethodCall {
                                    expr_ty: Type::Unknown,
                                    expr,
                                    name: field,
                                    args,
                                },
                                start,
                                end,
                            );
                        }
                        Expr::Ident(name) => {
                            current = Source::new_span(Expr::Call { name, args }, start, end);
                        }
                        _ => todo!(),
                    }
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_0(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_base(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::Dot => {
                    self.consume()?;

                    if self.is_next_token(Lexeme::LParen) {
                        self.expect(Lexeme::LParen)?;
                        let index = self.expr(with_struct)?;
                        let start = current.span.start;
                        let end_token = self.expect(Lexeme::RParen)?;

                        current = Source::new_span(
                            Expr::Index {
                                ptr: Box::new(current),
                                index: Box::new(index),
                            },
                            start,
                            end_token.span.end,
                        );
                    } else if self.is_ident() {
                        let field = self.ident()?;
                        let start = current.span.start;
                        let end = field.span.end;

                        current = Source::new_span(
                            Expr::Project {
                                expr_ty: Type::Unknown,
                                expr: Box::new(current),
                                field,
                            },
                            start,
                            end,
                        );
                    }
                }
                _ => {
                    break;
                }
            }
        }

        Ok(current)
    }

    fn expr_base(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let token = self.peek()?.clone();
        match token.lexeme {
            Lexeme::Integer(i) => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(
                        Literal::Integer(Source::span(i.clone(), token.span.clone())),
                        token.span.clone(),
                    )),
                    token.span,
                ))
            }
            Lexeme::Float(f) => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(
                        Literal::Float(Source::span(f.clone(), token.span.clone())),
                        token.span.clone(),
                    )),
                    token.span,
                ))
            }
            Lexeme::String(_) => {
                todo!()
            }
            Lexeme::True => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(
                        Literal::Bool(Source::span(true, token.span.clone())),
                        token.span.clone(),
                    )),
                    token.span,
                ))
            }
            Lexeme::False => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(
                        Literal::Bool(Source::span(false, token.span.clone())),
                        token.span.clone(),
                    )),
                    token.span,
                ))
            }
            Lexeme::Nil => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(Literal::Nil, token.span.clone())),
                    token.span,
                ))
            }
            Lexeme::Ident(i) => {
                self.consume()?;

                let ident = Source::span(i.clone(), token.span.clone());
                let current = Source::span(Expr::Ident(ident.clone()), token.span);
                if with_struct && self.is_next_token(Lexeme::LBrace) {
                    self.expect(Lexeme::LBrace)?;

                    let mut fields = vec![];

                    while !self.is_next_token(Lexeme::RBrace) {
                        let field = self.ident()?;
                        self.expect(Lexeme::Colon)?;
                        let value = self.expr(with_struct)?;

                        fields.push((field, value));

                        if self.is_next_token(Lexeme::Comma) {
                            self.expect(Lexeme::Comma)?;
                            continue;
                        } else {
                            break;
                        }
                    }

                    let end = self.expect(Lexeme::RBrace)?;

                    return Ok(Source::new_span(
                        Expr::Struct {
                            name: ident,
                            fields,
                        },
                        current.span.start,
                        end.span.end,
                    ));
                }

                Ok(current)
            }
            Lexeme::LParen => {
                self.position += 1;

                let current = self.expr(with_struct)?;

                self.expect(Lexeme::RParen)?;

                Ok(current)
            }
            Lexeme::LBrace => {
                self.position += 1;

                let block = self.block(Some(Lexeme::RBrace))?;
                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Expr::Block(Box::new(block)),
                    token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::Minus => {
                self.expect(Lexeme::Minus)?;

                let expr = self.expr(false)?;
                let start = token.span.start;
                let end = expr.span.end;

                Ok(Source::new_span(
                    Expr::Negate {
                        ty: Type::Unknown,
                        expr: Box::new(expr),
                    },
                    start,
                    end,
                ))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
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
                Source::unknown(Expr::BinOp {
                    ty: Type::Unknown,
                    op: Source::unknown(BinOp::Add),
                    left: Box::new(Source::unknown(Expr::Lit(Source::unknown(
                        Literal::Integer(Source::unknown(1)),
                    )))),
                    right: Box::new(Source::unknown(Expr::BinOp {
                        ty: Type::Unknown,
                        op: Source::unknown(BinOp::Mul),
                        left: Box::new(Source::unknown(Expr::Lit(Source::unknown(
                            Literal::Integer(Source::unknown(3)),
                        )))),
                        right: Box::new(Source::unknown(Expr::Lit(Source::unknown(
                            Literal::Integer(Source::unknown(4)),
                        )))),
                    })),
                }),
            ),
            /*
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
             */
        ];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.expr(true).unwrap();

            assert_eq!(got, expected);
        }
    }

    /*
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
                Statement::Assign(Expr::Ident("a".to_string()), Expr::Ident("b".to_string())),
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
                        },
                    }],
                },
            ),
            (
                "let a = 1; 10",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(1))),
                        Statement::Expr(Expr::Lit(Literal::Integer(10))),
                    ],
                },
            ),
            (
                "let a = 1; 10;",
                Block {
                    statements: vec![
                        Statement::Let("a".to_string(), Expr::Lit(Literal::Integer(1))),
                        Statement::Expr(Expr::Lit(Literal::Integer(10))),
                    ],
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
                                Statement::Assign(
                                    Expr::Ident("a".to_string()),
                                    Expr::Lit(Literal::Integer(4)),
                                ),
                            ],
                        }),
                        Statement::Expr(Expr::Ident("a".to_string())),
                    ],
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
                                Statement::Assign(
                                    Expr::Ident("a".to_string()),
                                    Expr::Lit(Literal::Integer(4)),
                                ),
                            ],
                        }),
                        Statement::Expr(Expr::Ident("a".to_string())),
                    ],
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

    #[test]
    fn test_decls() {
        let cases = vec![(
            r#"fun main() {
    let x = 1;
    let y = 2;

    return x + y;
                }"#,
            vec![Declaration::Function {
                name: "main".to_string(),
                params: vec![],
                body: Block {
                    statements: vec![
                        Statement::Let("x".to_string(), Expr::Lit(Literal::Integer(1))),
                        Statement::Let("y".to_string(), Expr::Lit(Literal::Integer(2))),
                        Statement::Return(Expr::BinOp {
                            op: BinOp::Add,
                            left: Box::new(Expr::Ident("x".to_string())),
                            right: Box::new(Expr::Ident("y".to_string())),
                        }),
                    ],
                },
            }],
        )];

        for (input, expected) in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new(tokens);
            let got = parser.decls().unwrap();

            assert_eq!(expected, got);
        }
    }
    */
}
