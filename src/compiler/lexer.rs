use once_cell::sync::Lazy;

use regex::Regex;

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
    DoubleEqual,
    NotEqual,
    Le,
    Ge,
    DoubleAnd,
    DoubleOr,
    Arrow,
    LAngle,
    GAngle,
    Equal,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Dot,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Ident(String),
    String(String),
    Integer(i32),
}

#[derive(Debug)]
pub enum LexerError {
    InvalidCharacter(char),
}

pub struct Lexer {
    input: String,
    position: usize,
}

const SPACES: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\s+").unwrap());
const IDENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap());
const STRING: Lazy<Regex> = Lazy::new(|| Regex::new(r#"^"[^"]*""#).unwrap());
const INTEGER: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[0-9]+").unwrap());

impl Lexer {
    pub fn new(input: String) -> Self {
        Self { input, position: 0 }
    }

    pub fn run(&mut self) -> Result<Vec<Lexeme>, LexerError> {
        let mut tokens = vec![];

        while self.position < self.input.len() {
            if let Some(m) = SPACES.find(&self.input[self.position..]) {
                self.position += m.end();
                continue;
            }

            if let Some((lexeme, length)) = self.matches() {
                tokens.push(lexeme);
                self.position += length;
                continue;
            }

            if let Some(m) = STRING.find(&self.input[self.position..]) {
                let lexeme = Lexeme::String(m.as_str().to_string());
                tokens.push(lexeme);
                self.position += m.end();
                continue;
            }

            if let Some(m) = INTEGER.find(&self.input[self.position..]) {
                let lexeme = Lexeme::Integer(m.as_str().parse().unwrap());
                tokens.push(lexeme);
                self.position += m.end();
                continue;
            }

            if let Some(m) = IDENT.find(&self.input[self.position..]) {
                let lexeme = Lexeme::Ident(m.as_str().to_string());
                tokens.push(lexeme);
                self.position += m.end();
                continue;
            }

            return Err(LexerError::InvalidCharacter(
                self.input.chars().nth(self.position).unwrap(),
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
            ("=>", Lexeme::Arrow),
            ("<=", Lexeme::Le),
            (">=", Lexeme::Ge),
            ("==", Lexeme::DoubleEqual),
            ("!=", Lexeme::NotEqual),
            ("&&", Lexeme::DoubleAnd),
            ("||", Lexeme::DoubleOr),
            ("<", Lexeme::LAngle),
            (">", Lexeme::GAngle),
            ("=", Lexeme::Equal),
            (";", Lexeme::Semicolon),
            ("(", Lexeme::LParen),
            (")", Lexeme::RParen),
            ("{", Lexeme::LBrace),
            ("}", Lexeme::RBrace),
            (".", Lexeme::Dot),
            (",", Lexeme::Comma),
            ("+", Lexeme::Plus),
            ("-", Lexeme::Minus),
            ("*", Lexeme::Star),
            ("/", Lexeme::Slash),
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
        ];

        for (input, expected) in cases {
            let mut lexer = Lexer::new(input.to_string());
            let tokens = lexer.run().unwrap();
            assert_eq!(tokens, expected);
        }
    }
}
