use std::{collections::HashSet, iter::Iterator, num::ParseIntError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub typ: TokenType,
    start: usize,
    end: usize,
    line: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolType {
    Lparen,
    Rparen,
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
}

impl SymbolType {
    fn len(&self) -> usize {
        match self {
            SymbolType::Plus
            | SymbolType::Minus
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
            SymbolType::LTEQ | SymbolType::BangEq | SymbolType::EqEq | SymbolType::GTEQ => 2,
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
    Keyword(String),
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
            TokenType::Keyword(keyword) => match keyword.as_ref() {
                "or" => 2,
                "and" => 3,
                _ => 0,
            },
            TokenType::Symbol(symbol) => match symbol {
                SymbolType::SemiColon | SymbolType::Lparen | SymbolType::Rparen => 0,
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
    keywords: HashSet<&'static str>,
}

fn valid_identifier_char(c: &char) -> bool {
    // TODO support more
    c.is_ascii_alphanumeric()
}

impl Lexer {
    pub fn new(inp: &str) -> Lexer {
        Lexer {
            in_decimal: false,
            chars: inp.chars().into_iter().collect(),
            pos_in_line: 0,
            line: 0,
            pos: 0,
            // TODO: could be a static variable
            keywords: HashSet::from_iter(vec![
                "for", "while", "const", "var", "true", "false", "if", "else", "break", "func",
            ]),
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
                '-' => SymbolType::Minus,
                '/' => SymbolType::Slash,
                '*' => SymbolType::Star,
                ':' => SymbolType::Colon,
                ';' => SymbolType::SemiColon,
                '%' => SymbolType::Percentage,
                '(' => SymbolType::Lparen,
                ')' => SymbolType::Rparen,
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
            if self.keywords.contains(&&content[..]) {
                TokenType::Keyword(content)
            } else {
                TokenType::Identifier(content)
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
}

impl Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.skip_newlines_and_whitespace();

        let Some(ch) = self.chars.get(self.pos) else {
            return None;
        };

        let ttype = Some(match ch {
            '+' | '-' | '*' | '/' | ':' | ';' | '(' | ')' => self.symbol(true),
            '=' | '!' | '>' | '<' => self.symbol(false),
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

mod tests {
    use super::*;
    #[test]
    fn test_lex() -> Result<(), LexerError> {
        let lexer = Lexer::new(
            r#"const x: int = 5 + 10_11_2;
    var y: string = "abc";
    x + y;"#,
        );

        let expected = vec![
            Token {
                typ: TokenType::Keyword("const".to_string()),
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
                typ: TokenType::Identifier("int".to_string()),
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
                typ: TokenType::Keyword("var".to_string()),
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
                typ: TokenType::Identifier("string".to_string()),
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
        ];

        for (token, expected) in lexer.zip(expected.iter()) {
            assert_eq!(token?, *expected);
        }

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
