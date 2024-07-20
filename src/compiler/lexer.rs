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

enum Numeric {
    Integer(i32),
    Float(f32),
}

enum Matcher {
    Exact(&'static str),
    Until(char),
    WhileDigit,
}

impl Lexer {
    pub fn new(module_name: String, input: String) -> Self {
        Self {
            module_name,
            input,
            position: 0,
        }
    }

    fn new_token(&self, lexeme: Lexeme, start_position: usize) -> Token {
        Token {
            lexeme,
            span: Span::span(self.module_name.clone(), start_position, self.position),
        }
    }

    fn consumes(chars: &[char], matcher: Matcher) -> usize {
        match matcher {
            Matcher::Exact(token) => {
                if chars.starts_with(&token.chars().collect::<Vec<_>>()) {
                    token.len()
                } else {
                    0
                }
            }
            Matcher::Until(c) => {
                let mut count = 0;
                while count < chars.len() && chars[count] != c {
                    count += 1;
                }

                count
            }
            Matcher::WhileDigit => {
                let mut count = 0;
                while count < chars.len() && chars[count].is_ascii_digit() {
                    count += 1;
                }

                count
            }
        }
    }

    fn consume_line_comment(&mut self, chars: &Vec<char>) -> Option<String> {
        let start = self.position;

        let c = Self::consumes(&chars[self.position..], Matcher::Exact("//"));
        self.position += c;
        if c == 0 {
            return None;
        }

        let c = Self::consumes(&chars[self.position..], Matcher::Until('\n'));
        self.position += c;

        let end = self.position;

        if end > start {
            Some(self.input[start..end].to_string())
        } else {
            None
        }
    }

    fn consume_string(&mut self, chars: &Vec<char>) -> Option<String> {
        let c = Self::consumes(&chars[self.position..], Matcher::Exact("\""));
        self.position += c;
        if c == 0 {
            return None;
        }

        let start = self.position;

        let c = Self::consumes(&chars[self.position..], Matcher::Until('"'));
        self.position += c;

        let c = Self::consumes(&chars[self.position..], Matcher::Exact("\""));
        self.position += c;

        let end = self.position;

        Some(self.input[start..end].to_string())
    }

    fn consume_numeric(&mut self, chars: &Vec<char>) -> Option<Numeric> {
        let start = self.position;

        let c = Self::consumes(&chars[self.position..], Matcher::WhileDigit);
        self.position += c;
        if c == 0 {
            return None;
        }

        let dot = Self::consumes(&chars[self.position..], Matcher::Exact("."));
        self.position += dot;
        if dot == 0 {
            Some(Numeric::Integer(
                self.input[start..self.position].parse().unwrap(),
            ))
        } else {
            let c = Self::consumes(&chars[self.position..], Matcher::WhileDigit);
            self.position += c;
            // If the number ends with a dot, it's not a valid float
            // e.g. 0..n should be parsed as 0, .., n
            if c == 0 {
                self.position -= 1;

                return Some(Numeric::Integer(
                    self.input[start..self.position].parse().unwrap(),
                ));
            }

            let end = self.position;
            Some(Numeric::Float(self.input[start..end].parse().unwrap()))
        }
    }

    fn consume_ident(&mut self, chars: &Vec<char>) -> Option<String> {
        let mut s = String::new();
        if !chars[self.position].is_alphabetic() {
            return None;
        }

        while self.position < chars.len()
            && (chars[self.position].is_alphanumeric() || "_".contains(chars[self.position]))
        {
            s.push(chars[self.position]);
            self.position += 1;
        }

        Some(s)
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

            if let Some((lexeme, length)) = self.consume_keywords() {
                tokens.push(self.new_token(lexeme, length));
                continue;
            }

            let position = self.position;
            if let Some(string) = self.consume_string(&chars) {
                let lexeme =
                    Lexeme::String(string.trim_matches('"').replace("\\n", "\n").to_string());
                tokens.push(self.new_token(lexeme, position));
                continue;
            }

            let position = self.position;
            if let Some(num) = self.consume_numeric(&chars) {
                match num {
                    Numeric::Integer(val) => {
                        let lexeme = Lexeme::Integer(val);
                        tokens.push(self.new_token(lexeme, position));
                        continue;
                    }
                    Numeric::Float(val) => {
                        let lexeme = Lexeme::Float(val);
                        tokens.push(self.new_token(lexeme, position));
                        continue;
                    }
                }
            }

            let position = self.position;
            if let Some(ident) = self.consume_ident(&chars) {
                let lexeme = Lexeme::Ident(ident);
                tokens.push(self.new_token(lexeme, position));
                continue;
            }

            return Err(LexerError::InvalidCharacter(
                chars[self.position],
                self.position,
            ));
        }

        Ok(tokens)
    }

    fn consume_keywords(&mut self) -> Option<(Lexeme, usize)> {
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
                        let position = self.position;
                        self.position += keyword.len();
                        return Some((lexeme.clone(), position));
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
                let position = self.position;
                self.position += keyword.len();
                return Some((lexeme.clone(), position));
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

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
            ("3.14", vec![Lexeme::Float(3.14)]),
            ("128", vec![Lexeme::Integer(128)]),
            (
                "0..n",
                vec![
                    Lexeme::Integer(0),
                    Lexeme::DoubleDot,
                    Lexeme::Ident("n".to_string()),
                ],
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

    #[test]
    fn test_lexer_with_position() {
        let cases = vec![(
            "let hoge = 10;",
            vec![
                Token {
                    lexeme: Lexeme::Let,
                    span: Span::span("".to_string(), 0, 3),
                },
                Token {
                    lexeme: Lexeme::Ident("hoge".to_string()),
                    span: Span::span("".to_string(), 4, 8),
                },
                Token {
                    lexeme: Lexeme::Equal,
                    span: Span::span("".to_string(), 9, 10),
                },
                Token {
                    lexeme: Lexeme::Integer(10),
                    span: Span::span("".to_string(), 11, 13),
                },
                Token {
                    lexeme: Lexeme::Semicolon,
                    span: Span::span("".to_string(), 13, 14),
                },
            ],
        )];

        for (input, expected) in cases {
            let mut lexer = Lexer::new("".to_string(), input.to_string());
            let tokens = lexer.run().unwrap();

            for (token, expected) in tokens.iter().zip(expected.iter()) {
                assert_eq!(token.lexeme, expected.lexeme);
                assert_eq!(token.span.start, expected.span.start);
                assert_eq!(token.span.end, expected.span.end);
            }
        }
    }
}
