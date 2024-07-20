use once_cell::sync::Lazy;

use regex::Regex;
use thiserror::Error;

use super::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Lexeme {
    Let,
    Fun,
    Return,
    While,
    True,
    False,
    If,
    Else,
    Match,
    New,
    Struct,
    As,
    Nil,
    Import,
    Declare,
    Module,
    In,
    For,
    DoubleEqual,
    NotEqual,
    Le,
    Ge,
    DoubleColon,
    DoubleAnd,
    DoubleOr,
    DoubleDot,
    Arrow,
    LAngle,
    GAngle,
    Equal,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Comma,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exclamation,
    At,
    Ident(String),
    String(String),
    Integer(i32),
    Float(f32),
    Comment(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub lexeme: Lexeme,
    pub position: usize,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Error)]
pub enum LexerError {
    #[error("invalid character: {0}")]
    InvalidCharacter(char, usize),
}

pub struct Lexer {
    module_name: String,
    input: String,
    position: usize,
}

const SPACES: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\s+").unwrap());
const IDENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap());
const STRING: Lazy<Regex> = Lazy::new(|| Regex::new(r#"^"[^"]*""#).unwrap());
const FLOAT: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[0-9]+\.[0-9]+").unwrap());
const INTEGER: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[0-9]+").unwrap());
const COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"^//.*").unwrap());

impl Lexer {
    pub fn new(module_name: String, input: String) -> Self {
        Self {
            module_name,
            input,
            position: 0,
        }
    }

    fn new_token(&self, lexeme: Lexeme, length: usize) -> Token {
        Token {
            lexeme,
            position: self.position,
            span: Span::span(
                self.module_name.clone(),
                self.position,
                self.position + length,
            ),
        }
    }

    fn consume_line_comment(&mut self, chars: &Vec<char>) -> Option<String> {
        if chars[self.position..].starts_with(&"//".chars().collect::<Vec<_>>()) {
            let start_position = self.position;
            self.position += 2;

            while self.position < chars.len() && chars[self.position] != '\n' {
                self.position += 1;
            }

            let end_position = self.position;

            Some(self.input[start_position..end_position].to_string())
        } else {
            None
        }
    }

    fn consume_string(&mut self, chars: &Vec<char>) -> Option<String> {
        if chars[self.position] == '"' {
            let start_position = self.position;
            self.position += 1;

            while self.position < chars.len() && chars[self.position] != '"' {
                self.position += 1;
            }

            let end_position = self.position;

            if chars[self.position] == '"' {
                self.position += 1;
                Some(self.input[start_position..end_position].to_string())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn run(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = vec![];
        let chars = self.input.chars().collect::<Vec<char>>();

        while self.position < chars.len() {
            if chars[self.position].is_whitespace() {
                self.position += 1;
                continue;
            }

            if let Some(comment) = self.consume_line_comment(&chars) {
                tokens.push(self.new_token(Lexeme::Comment(comment.clone()), comment.len()));
                continue;
            }

            if let Some((lexeme, length)) = self.matches() {
                tokens.push(self.new_token(lexeme, length));
                self.position += length;
                continue;
            }

            if let Some(string) = self.consume_string(&chars) {
                let lexeme =
                    Lexeme::String(string.trim_matches('"').replace("\\n", "\n").to_string());
                tokens.push(self.new_token(lexeme, self.position));
                continue;
            }

            if let Some(m) = FLOAT.find(&self.input[self.position..]) {
                let lexeme = Lexeme::Float(m.as_str().parse().unwrap());
                tokens.push(self.new_token(lexeme, m.end()));
                self.position += m.end();
                continue;
            }

            if let Some(m) = INTEGER.find(&self.input[self.position..]) {
                let lexeme = Lexeme::Integer(m.as_str().parse().unwrap());
                tokens.push(self.new_token(lexeme, m.end()));
                self.position += m.end();
                continue;
            }

            if let Some(m) = IDENT.find(&self.input[self.position..]) {
                let lexeme = Lexeme::Ident(m.as_str().to_string());
                tokens.push(self.new_token(lexeme, m.end()));
                self.position += m.end();
                continue;
            }

            return Err(LexerError::InvalidCharacter(
                self.input.chars().nth(self.position).unwrap(),
                self.position,
            ));
        }

        Ok(tokens)
    }

    fn matches(&mut self) -> Option<(Lexeme, usize)> {
        let keywords = vec![
            ("let", Lexeme::Let),
            ("fun", Lexeme::Fun),
            ("return", Lexeme::Return),
            ("while", Lexeme::While),
            ("true", Lexeme::True),
            ("false", Lexeme::False),
            ("if", Lexeme::If),
            ("else", Lexeme::Else),
            ("match", Lexeme::Match),
            ("new", Lexeme::New),
            ("struct", Lexeme::Struct),
            ("as", Lexeme::As),
            ("nil", Lexeme::Nil),
            ("import", Lexeme::Import),
            ("declare", Lexeme::Declare),
            ("module", Lexeme::Module),
            ("in", Lexeme::In),
            ("for", Lexeme::For),
        ];

        for (keyword, lexeme) in keywords.iter() {
            if self.input[self.position..].starts_with(keyword) {
                if let Some(c) = self.input[self.position + keyword.len()..].chars().nth(0) {
                    if !c.is_alphanumeric() && c != '_' {
                        return Some((lexeme.clone(), keyword.len()));
                    }
                }
            }
        }

        let keywords = vec![
            ("=>", Lexeme::Arrow),
            ("<=", Lexeme::Le),
            (">=", Lexeme::Ge),
            ("==", Lexeme::DoubleEqual),
            ("!=", Lexeme::NotEqual),
            ("&&", Lexeme::DoubleAnd),
            ("||", Lexeme::DoubleOr),
            ("::", Lexeme::DoubleColon),
            ("..", Lexeme::DoubleDot),
            ("<", Lexeme::LAngle),
            (">", Lexeme::GAngle),
            ("=", Lexeme::Equal),
            (";", Lexeme::Semicolon),
            ("(", Lexeme::LParen),
            (")", Lexeme::RParen),
            ("{", Lexeme::LBrace),
            ("}", Lexeme::RBrace),
            ("[", Lexeme::LBracket),
            ("]", Lexeme::RBracket),
            (".", Lexeme::Dot),
            (",", Lexeme::Comma),
            (":", Lexeme::Colon),
            ("+", Lexeme::Plus),
            ("-", Lexeme::Minus),
            ("*", Lexeme::Star),
            ("/", Lexeme::Slash),
            ("%", Lexeme::Percent),
            ("!", Lexeme::Exclamation),
            ("@", Lexeme::At),
        ];

        for (keyword, lexeme) in keywords.iter() {
            if self.input[self.position..].starts_with(keyword) {
                return Some((lexeme.clone(), keyword.len()));
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let cases = vec![
            (
                "let x = 10;",
                vec![
                    Lexeme::Let,
                    Lexeme::Ident("x".to_string()),
                    Lexeme::Equal,
                    Lexeme::Integer(10),
                    Lexeme::Semicolon,
                ],
            ),
            (
                "fun add(a, b) { return a + b; }",
                vec![
                    Lexeme::Fun,
                    Lexeme::Ident("add".to_string()),
                    Lexeme::LParen,
                    Lexeme::Ident("a".to_string()),
                    Lexeme::Comma,
                    Lexeme::Ident("b".to_string()),
                    Lexeme::RParen,
                    Lexeme::LBrace,
                    Lexeme::Return,
                    Lexeme::Ident("a".to_string()),
                    Lexeme::Plus,
                    Lexeme::Ident("b".to_string()),
                    Lexeme::Semicolon,
                    Lexeme::RBrace,
                ],
            ),
            (
                r#""A string""#,
                vec![Lexeme::String("A string".to_string())],
            ),
            (
                "\"A string\\nnewline\"",
                vec![Lexeme::String("A string\nnewline".to_string())],
            ),
        ];

        for (input, expected) in cases {
            let mut lexer = Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();
            assert_eq!(
                tokens.into_iter().map(|t| t.lexeme).collect::<Vec<_>>(),
                expected
            );
        }
    }
}
