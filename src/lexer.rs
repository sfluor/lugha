use std::{collections::HashMap, iter::Iterator, num::ParseIntError, thread};

pub static KEYWORDS: [(&str, KeywordType); 20] = [
    ("or", KeywordType::Or),
    ("and", KeywordType::And),
    ("for", KeywordType::For),
    ("while", KeywordType::While),
    ("const", KeywordType::Const),
    ("var", KeywordType::Var),
    ("true", KeywordType::True),
    ("false", KeywordType::False),
    ("if", KeywordType::If),
    ("print", KeywordType::Print),
    ("println", KeywordType::Println),
    ("else", KeywordType::Else),
    ("elif", KeywordType::Elif),
    ("break", KeywordType::Break),
    ("return", KeywordType::Return),
    ("func", KeywordType::Func),
    ("int", KeywordType::Int),
    ("string", KeywordType::String),
    ("float", KeywordType::Float),
    ("bool", KeywordType::Bool),
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub typ: TokenType,
    start: usize,
    end: usize,
    line: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeywordType {
    And,
    Break,
    Return,
    Const,
    Else,
    Elif,
    False,
    For,
    Func,
    Int,
    String,
    Float,
    Bool,
    If,
    Or,
    Print,
    Println,
    True,
    Var,
    While,
}
impl KeywordType {
    fn len(&self) -> usize {
        match self {
            KeywordType::If | KeywordType::Or => 2,
            KeywordType::Int | KeywordType::And | KeywordType::For | KeywordType::Var => 3,
            KeywordType::Bool
            | KeywordType::Elif
            | KeywordType::Else
            | KeywordType::True
            | KeywordType::Func => 4,
            KeywordType::Float
            | KeywordType::Break
            | KeywordType::False
            | KeywordType::Const
            | KeywordType::Print
            | KeywordType::While => 5,
            KeywordType::Return | KeywordType::String => 6,
            KeywordType::Println => 7,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolType {
    Lparen,
    Rparen,
    Comma,
    LBracket,
    RBracket,
    Plus,
    Minus,
    Star,
    Slash,
    SemiColon,
    GT,
    GTEQ,
    LT,
    LTEQ,
    Bang,
    BangEq,
    Eq,
    EqEq,
    Percentage,
    Colon,
    Arrow,
}

impl SymbolType {
    fn len(&self) -> usize {
        match self {
            SymbolType::Plus
            | SymbolType::Comma
            | SymbolType::Minus
            | SymbolType::LBracket
            | SymbolType::RBracket
            | SymbolType::Lparen
            | SymbolType::Rparen
            | SymbolType::Star
            | SymbolType::Slash
            | SymbolType::Percentage
            | SymbolType::Colon
            | SymbolType::SemiColon
            | SymbolType::Eq
            | SymbolType::GT
            | SymbolType::LT
            | SymbolType::Bang => 1,
            SymbolType::Arrow
            | SymbolType::LTEQ
            | SymbolType::BangEq
            | SymbolType::EqEq
            | SymbolType::GTEQ => 2,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Float(pub f64);

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        // TODO: epsilon ?
        self.0 == other.0
    }
}

impl Eq for Float {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Keyword(KeywordType),
    Identifier(String),
    StringLiteral(String),
    IntLiteral(String, i64),
    FloatLiteral(String, Float),
    Symbol(SymbolType),
}

impl TokenType {
    fn len(&self) -> usize {
        match self {
            TokenType::Keyword(k) => k.len(),
            TokenType::Identifier(id) => id.len(),
            TokenType::StringLiteral(lit) => lit.len() + 2, // + 2 for the quotes
            TokenType::IntLiteral(lit, _) => lit.len(),
            TokenType::FloatLiteral(lit, _) => lit.len(),
            TokenType::Symbol(s) => s.len(),
        }
    }

    pub fn precedence(&self, unary: bool) -> u8 {
        match self {
            TokenType::IntLiteral(_, _)
            | TokenType::Identifier(_)
            | TokenType::StringLiteral(_)
            | TokenType::FloatLiteral(_, _) => 0,
            TokenType::Keyword(keyword) => match keyword {
                KeywordType::Or => 2,
                KeywordType::And => 3,
                _ => 0,
            },
            TokenType::Symbol(symbol) => match symbol {
                SymbolType::Lparen => {
                    if unary {
                        // Grouping
                        0
                    } else {
                        // Func call
                        9
                    }
                }
                SymbolType::SemiColon
                | SymbolType::Arrow
                | SymbolType::Comma
                | SymbolType::Rparen
                | SymbolType::LBracket
                | SymbolType::RBracket => 0,
                SymbolType::Eq => 1,
                SymbolType::EqEq | SymbolType::BangEq => 4,
                SymbolType::GT | SymbolType::GTEQ | SymbolType::LT | SymbolType::LTEQ => 5,
                SymbolType::Plus => 6,
                SymbolType::Minus => {
                    if unary {
                        8
                    } else {
                        6
                    }
                }
                SymbolType::Star | SymbolType::Slash => 7,
                SymbolType::Bang => 8,
                SymbolType::Percentage => todo!(),
                SymbolType::Colon => todo!(),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    Generic(&'static str),
    UnexpectedChar(char, usize, i32),
    InvalidNumber(String, usize, i32),
    UnknownSingleSymbol(char, usize, i32),
    UnknownMultiSymbol(char, usize, i32),
}

pub struct Lexer {
    // TODO: flux of chars instead of raw string
    in_decimal: bool,
    chars: Vec<char>,
    pos_in_line: usize,
    line: i32,
    pos: usize,
    keywords: HashMap<&'static str, KeywordType>,
    error: bool,
}

fn valid_identifier_char(c: &char) -> bool {
    // TODO support more
    c.is_ascii_alphanumeric()
}

impl Lexer {
    pub fn new(inp: &str) -> Lexer {
        Lexer {
            in_decimal: false,
            error: false,
            chars: inp.chars().into_iter().collect(),
            pos_in_line: 0,
            line: 0,
            pos: 0,
            // TODO: could be a static variable
            keywords: HashMap::from_iter(KEYWORDS.clone()),
        }
    }

    pub fn reset(&mut self) {
        self.pos_in_line = 0;
        self.pos = 0;
        self.line = 0;
    }

    fn until<F>(&mut self, cond: F) -> Result<String, LexerError>
    where
        F: Fn(&char) -> bool,
    {
        let start = self.pos;
        let mut end = start + 1;
        while let Some(ch) = self.chars.get(end) {
            if cond(ch) {
                break;
            }
            end += 1;
        }

        return Ok(String::from_iter(self.chars[start..end].iter()));
    }

    fn symbol(&mut self, single: bool) -> Result<TokenType, LexerError> {
        let Some(ch) = self.chars.get(self.pos) else {
            unreachable!() // We already checked the character before this shouldn't happen
        };

        let sym = if single {
            match ch {
                '+' => SymbolType::Plus,
                ',' => SymbolType::Comma,
                '/' => SymbolType::Slash,
                '*' => SymbolType::Star,
                ':' => SymbolType::Colon,
                ';' => SymbolType::SemiColon,
                '%' => SymbolType::Percentage,
                '(' => SymbolType::Lparen,
                ')' => SymbolType::Rparen,
                '{' => SymbolType::LBracket,
                '}' => SymbolType::RBracket,
                _ => {
                    return Err(LexerError::UnknownSingleSymbol(
                        *ch,
                        self.pos_in_line,
                        self.line,
                    ))
                }
            }
        } else {
            match ch {
                '-' => match self.peek() {
                    Some('>') => SymbolType::Arrow,
                    _ => SymbolType::Minus,
                },
                '=' => match self.peek() {
                    Some('=') => SymbolType::EqEq,
                    _ => SymbolType::Eq,
                },
                '>' => match self.peek() {
                    Some('=') => SymbolType::GTEQ,
                    _ => SymbolType::GT,
                },
                '<' => match self.peek() {
                    Some('=') => SymbolType::LTEQ,
                    _ => SymbolType::LT,
                },
                '!' => match self.peek() {
                    Some('=') => SymbolType::BangEq,
                    _ => SymbolType::Bang,
                },
                _ => {
                    return Err(LexerError::UnknownMultiSymbol(
                        *ch,
                        self.pos_in_line,
                        self.line,
                    ))
                }
            }
        };

        Ok(TokenType::Symbol(sym))
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.get(self.pos + 1)
    }

    fn number(&mut self) -> Result<TokenType, LexerError> {
        self.in_decimal = false;
        self.until(|&c| {
            // We will validate numbers later on in the parser.
            !c.is_numeric() && c != '_' && c != '.'
        })
        .and_then(|nb| match nb.chars().filter(|&c| c == '.').count() {
            0 => nb
                .replace("_", "")
                .parse::<i64>()
                .map(|n| TokenType::IntLiteral(nb.clone(), n))
                .map_err(|_| LexerError::InvalidNumber(nb, self.pos_in_line, self.line)),
            1 => nb
                .replace("_", "")
                .parse::<f64>()
                .map(|n| TokenType::FloatLiteral(nb.clone(), Float(n)))
                .map_err(|_| LexerError::InvalidNumber(nb, self.pos_in_line, self.line)),
            _ => Err(LexerError::InvalidNumber(nb, self.pos_in_line, self.line)),
        })
    }

    fn string(&mut self) -> Result<TokenType, LexerError> {
        self.until(|&c| c == '"').map(|mut content| {
            content.remove(0);
            // content will include an extra '"'
            TokenType::StringLiteral(content)
        })
    }

    fn identifier(&mut self) -> Result<TokenType, LexerError> {
        self.until(|c| !valid_identifier_char(c)).map(|content| {
            match self.keywords.get(&&content[..]) {
                Some(keyword) => TokenType::Keyword(keyword.clone()),
                None => TokenType::Identifier(content),
            }
        })
    }

    fn skip_newlines_and_whitespace(&mut self) {
        while let Some(ch) = self.chars.get(self.pos) {
            match ch {
                ' ' | '\t' => self.pos += 1,
                '\n' => {
                    self.pos += 1;
                    self.line += 1;
                }
                _ => return,
            }
        }
    }

    fn next_token(&mut self) -> Option<Result<Token, LexerError>> {
        self.skip_newlines_and_whitespace();

        let Some(ch) = self.chars.get(self.pos) else {
            return None;
        };

        let ttype = Some(match ch {
            '+' | '*' | '/' | ':' | ';' | '(' | ')' | '{' | '}' | ',' => self.symbol(true),
            '=' | '-' | '!' | '>' | '<' => self.symbol(false),
            'A'..='Z' | 'a'..='z' => self.identifier(),
            '"' => self.string(),
            '0'..='9' => self.number(),
            _ => Err(LexerError::UnexpectedChar(*ch, self.pos_in_line, self.line)),
        });

        ttype.map(|typ_or_err| {
            typ_or_err.map(|typ| {
                let len = typ.len();
                let tok = Token {
                    typ,
                    start: self.pos_in_line,
                    end: self.pos_in_line + len,
                    line: self.line,
                };

                self.pos += len;
                self.pos_in_line += len;
                tok
            })
        })
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.error {
            None
        } else {
            self.next_token().inspect(|r| self.error = r.is_err())
        }
    }
}

mod tests {
    use super::*;
    #[test]
    fn test_lex() -> Result<(), LexerError> {
        let lexer = Lexer::new(
            r#"const x: int = 5 + 10_11_2;
    var y: string = "abc";
    x + y;
    if 5 > 3 {
        if a > b {
            1 + 2;
            3 + 4;
        } elif a > c {
            4 + 7;
            x = 1;
        } else {
            y = 2;
        }
    }"#,
        );

        let expected = vec![
            Token {
                typ: TokenType::Keyword(KeywordType::Const),
                start: 0,
                end: 5,
                line: 0,
            },
            Token {
                typ: TokenType::Identifier("x".to_string()),
                start: 5,
                end: 6,
                line: 0,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Colon),
                start: 6,
                end: 7,
                line: 0,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::Int),
                start: 7,
                end: 10,
                line: 0,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Eq),
                start: 10,
                end: 11,
                line: 0,
            },
            Token {
                typ: TokenType::IntLiteral("5".to_string(), 5),
                start: 11,
                end: 12,
                line: 0,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 12,
                end: 13,
                line: 0,
            },
            Token {
                typ: TokenType::IntLiteral("10_11_2".to_string(), 10112),
                start: 13,
                end: 20,
                line: 0,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 20,
                end: 21,
                line: 0,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::Var),
                start: 21,
                end: 24,
                line: 1,
            },
            Token {
                typ: TokenType::Identifier("y".to_string()),
                start: 24,
                end: 25,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Colon),
                start: 25,
                end: 26,
                line: 1,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::String),
                start: 26,
                end: 32,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Eq),
                start: 32,
                end: 33,
                line: 1,
            },
            Token {
                typ: TokenType::StringLiteral("abc".to_string()),
                start: 33,
                end: 38,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 38,
                end: 39,
                line: 1,
            },
            Token {
                typ: TokenType::Identifier("x".to_string()),
                start: 39,
                end: 40,
                line: 2,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 40,
                end: 41,
                line: 2,
            },
            Token {
                typ: TokenType::Identifier("y".to_string()),
                start: 41,
                end: 42,
                line: 2,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 42,
                end: 43,
                line: 2,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::If),
                start: 43,
                end: 45,
                line: 3,
            },
            Token {
                typ: TokenType::IntLiteral("5".to_owned(), 5),
                start: 45,
                end: 46,
                line: 3,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::GT),
                start: 46,
                end: 47,
                line: 3,
            },
            Token {
                typ: TokenType::IntLiteral("3".to_owned(), 3),
                start: 47,
                end: 48,
                line: 3,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::LBracket),
                start: 48,
                end: 49,
                line: 3,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::If),
                start: 49,
                end: 51,
                line: 4,
            },
            Token {
                typ: TokenType::Identifier("a".to_owned()),
                start: 51,
                end: 52,
                line: 4,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::GT),
                start: 52,
                end: 53,
                line: 4,
            },
            Token {
                typ: TokenType::Identifier("b".to_owned()),
                start: 53,
                end: 54,
                line: 4,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::LBracket),
                start: 54,
                end: 55,
                line: 4,
            },
            Token {
                typ: TokenType::IntLiteral("1".to_owned(), 1),
                start: 55,
                end: 56,
                line: 5,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 56,
                end: 57,
                line: 5,
            },
            Token {
                typ: TokenType::IntLiteral("2".to_owned(), 2),
                start: 57,
                end: 58,
                line: 5,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 58,
                end: 59,
                line: 5,
            },
            Token {
                typ: TokenType::IntLiteral("3".to_owned(), 3),
                start: 59,
                end: 60,
                line: 6,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 60,
                end: 61,
                line: 6,
            },
            Token {
                typ: TokenType::IntLiteral("4".to_owned(), 4),
                start: 61,
                end: 62,
                line: 6,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 62,
                end: 63,
                line: 6,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::RBracket),
                start: 63,
                end: 64,
                line: 7,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::Elif),
                start: 64,
                end: 68,
                line: 7,
            },
            Token {
                typ: TokenType::Identifier("a".to_owned()),
                start: 68,
                end: 69,
                line: 7,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::GT),
                start: 69,
                end: 70,
                line: 7,
            },
            Token {
                typ: TokenType::Identifier("c".to_owned()),
                start: 70,
                end: 71,
                line: 7,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::LBracket),
                start: 71,
                end: 72,
                line: 7,
            },
            Token {
                typ: TokenType::IntLiteral("4".to_owned(), 4),
                start: 72,
                end: 73,
                line: 8,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 73,
                end: 74,
                line: 8,
            },
            Token {
                typ: TokenType::IntLiteral("7".to_owned(), 7),
                start: 74,
                end: 75,
                line: 8,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 75,
                end: 76,
                line: 8,
            },
            Token {
                typ: TokenType::Identifier("x".to_owned()),
                start: 76,
                end: 77,
                line: 9,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Eq),
                start: 77,
                end: 78,
                line: 9,
            },
            Token {
                typ: TokenType::IntLiteral("1".to_owned(), 1),
                start: 78,
                end: 79,
                line: 9,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 79,
                end: 80,
                line: 9,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::RBracket),
                start: 80,
                end: 81,
                line: 10,
            },
            Token {
                typ: TokenType::Keyword(KeywordType::Else),
                start: 81,
                end: 85,
                line: 10,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::LBracket),
                start: 85,
                end: 86,
                line: 10,
            },
            Token {
                typ: TokenType::Identifier("y".to_owned()),
                start: 86,
                end: 87,
                line: 11,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Eq),
                start: 87,
                end: 88,
                line: 11,
            },
            Token {
                typ: TokenType::IntLiteral("2".to_owned(), 2),
                start: 88,
                end: 89,
                line: 11,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 89,
                end: 90,
                line: 11,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::RBracket),
                start: 90,
                end: 91,
                line: 12,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::RBracket),
                start: 91,
                end: 92,
                line: 13,
            },
        ];

        let tokens: Vec<Result<Token, LexerError>> = lexer.collect();

        for (i, expected) in expected.iter().enumerate() {
            let token = tokens.get(i);
            assert!(token.is_some());
            assert_eq!(token.unwrap().to_owned()?, *expected);
        }

        assert_eq!(tokens.len(), expected.len(), "got {tokens:?}");

        Ok(())
    }

    #[test]
    fn test_lex_number_multiple_dots() {
        let inputs = vec!["0.9.1", "00.11.22", "44......"];

        for input in inputs {
            let mut lexer = Lexer::new(input);
            assert_eq!(
                lexer.next(),
                Some(Err(LexerError::InvalidNumber(input.to_string(), 0, 0)))
            );
        }
    }
}
