use thiserror::Error;

use super::{
    ast::{BinOp, Block, Declaration, Expr, Literal, Source, Span, Statement, Type},
    lexer::{Lexeme, Token},
};

#[derive(Debug, PartialEq)]
pub struct Parser {
    module_name: String,
    tokens: Vec<Token>,
    pub(crate) position: usize,
    pub(crate) imports: Vec<String>,
}

#[derive(Debug, PartialEq, Clone, Error)]
pub enum ParseError {
    #[error("unexpected end of stream")]
    UnexpectedEos,
    #[error("unexpected token: expected {expected:?}, got {got:?}")]
    UnexpectedToken {
        expected: Option<Lexeme>,
        got: Token,
    },
}

impl Parser {
    pub fn new(module_name: String, tokens: Vec<Token>) -> Self {
        Self {
            module_name,
            tokens: tokens
                .into_iter()
                .filter(|t| !matches!(t.lexeme, Lexeme::Comment(_)))
                .collect(),
            position: 0,
            imports: vec![],
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
        let token = self.peek()?.clone();
        match token.lexeme {
            Lexeme::Fun => {
                let start_token = self.consume()?;

                let name = self.ident()?;
                self.expect(Lexeme::LParen)?;
                let params = self.arity_decl()?;
                let result_position = self.expect(Lexeme::RParen)?;

                self.expect(Lexeme::LBrace)?;
                let block = self.block(Some(Lexeme::RBrace))?;
                let end_token = self.expect(Lexeme::RBrace)?;

                Ok(Source::new_span(
                    Declaration::Function {
                        name,
                        params,
                        result: Source::new_span(
                            Type::Unknown,
                            self.module_name.clone(),
                            Some(result_position.span.end.unwrap()),
                            Some(result_position.span.end.unwrap()),
                        ),
                        body: block,
                    },
                    self.module_name.clone(),
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
                    Declaration::Let {
                        name,
                        ty: Type::Unknown,
                        value: expr,
                    },
                    self.module_name.clone(),
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
                    self.module_name.clone(),
                    start_token.span.start,
                    end_token.span.end,
                ))
            }
            Lexeme::Import => {
                let start_token = self.consume()?;

                let module_name = self.ident()?;
                self.expect(Lexeme::Semicolon)?;

                self.imports.push(module_name.data.clone());

                Ok(Source::new_span(
                    Declaration::Import(module_name.clone()),
                    self.module_name.clone(),
                    start_token.span.start,
                    module_name.span.end,
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
                    self.module_name.clone(),
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
                    self.module_name.clone(),
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
                    self.module_name.clone(),
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
                    self.module_name.clone(),
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

                    return Ok(Source::new_span(
                        s,
                        self.module_name.clone(),
                        token.span.start,
                        end,
                    ));
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

                    Ok(Source::new_span(
                        s,
                        self.module_name.clone(),
                        token.span.start,
                        end,
                    ))
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

                    Ok(Source::new_span(
                        s,
                        self.module_name.clone(),
                        token.span.start,
                        end,
                    ))
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
                                self.module_name.clone(),
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
                    self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
                        self.module_name.clone(),
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
        let mut current = self.expr_0(with_struct)?;

        while self.position < self.tokens.len() {
            let token = self.peek()?.clone();
            match token.lexeme {
                Lexeme::Star => {
                    self.consume()?;
                    let right = self.expr_0(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Mul, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        self.module_name.clone(),
                        start,
                        end,
                    );
                }
                Lexeme::Slash => {
                    self.consume()?;
                    let right = self.expr_0(with_struct)?;
                    let start = current.span.start;
                    let end = right.span.end;

                    current = Source::new_span(
                        Expr::BinOp {
                            ty: Type::Unknown,
                            op: Source::span(BinOp::Div, token.span),
                            left: Box::new(current),
                            right: Box::new(right),
                        },
                        self.module_name.clone(),
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

    fn expr_0(&mut self, with_struct: bool) -> Result<Source<Expr>, ParseError> {
        let mut current = self.expr_base(with_struct)?;

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
                        self.module_name.clone(),
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
                                self.module_name.clone(),
                                start,
                                end,
                            );
                        }
                        Expr::Ident(name) => {
                            current = Source::new_span(
                                Expr::Call { name, args },
                                self.module_name.clone(),
                                start,
                                end,
                            );
                        }
                        _ => todo!(),
                    }
                }
                Lexeme::Dot => {
                    self.consume()?;

                    if self.is_next_token(Lexeme::LParen) {
                        self.expect(Lexeme::LParen)?;
                        let index = self.expr(with_struct)?;
                        let start = current.span.start;
                        let end_token = self.expect(Lexeme::RParen)?;

                        current = Source::new_span(
                            Expr::Index {
                                ty: Type::Unknown,
                                ptr: Box::new(current),
                                index: Box::new(index),
                            },
                            self.module_name.clone(),
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
                            self.module_name.clone(),
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
            Lexeme::String(s) => {
                self.consume()?;

                Ok(Source::span(
                    Expr::Lit(Source::span(
                        Literal::String(Source::span(s.clone(), token.span.clone())),
                        token.span.clone(),
                    )),
                    token.span,
                ))
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
                        self.module_name.clone(),
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
                    self.module_name.clone(),
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
                    self.module_name.clone(),
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
    use super::*;

    #[test]
    fn test_expr_success() {
        let cases = vec![
            "1 + 3 * 4",
            "1 * 3 - 4",
            "4 * f(1, 2, 4 * 3) - 2",
            "1 <= 2 || 3 >= 4 && 1 + 2 == 4",
            "f.method(1,2,3)",
            "f().length",
        ];

        for input in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new("".to_string(), tokens);
            parser.expr(true).unwrap();

            assert_eq!(parser.position, parser.tokens.len(), "{}", input);
        }
    }

    #[test]
    fn test_statement() {
        let cases = vec!["let x = 1 + 2", "return 1 + 2", "a = b;"];

        for input in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new("".to_string(), tokens);
            parser.statement().unwrap();
        }
    }

    #[test]
    fn test_block() {
        let cases = vec![
            "let x = 1 + 2; let y = 3 * 4; let z = x + y;",
            "return 1 + 2; return 3 * 4;",
            "while (true) { let a = 1; let b = 2; }",
            "let a = 1; 10",
            "let a = 1; 10;",
            "let a = 2; { let a = 3; a = 4; }; a",
            "let a = 2; { let a = 3; a = 4; } a",
        ];

        for input in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new("".to_string(), tokens);
            parser.block(None).unwrap();
        }
    }

    #[test]
    fn test_decls() {
        let cases = vec![
            r#"fun main() {
    let x = 1;
    let y = 2;

    return x + y;
                }"#,
        ];

        for input in cases {
            let mut lexer = crate::compiler::lexer::Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();
            let mut parser = Parser::new("".to_string(), tokens);
            parser.decls().unwrap();
        }
    }
}
