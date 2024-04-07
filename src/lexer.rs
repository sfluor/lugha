use std::iter::Iterator;

#[derive(Debug, PartialEq, Eq)]
struct Token {
    typ: TokenType,
    start: usize,
    end: usize,
    line: i32,
}

#[derive(Debug, PartialEq, Eq)]
enum SymbolType {
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

#[derive(Debug, PartialEq, Eq)]
enum TokenType {
    Keyword(String),
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    Symbol(SymbolType),
}

impl TokenType {
    fn len(&self) -> usize {
        match self {
            TokenType::Keyword(k) => k.len(),
            TokenType::Identifier(id) => id.len(),
            TokenType::StringLiteral(lit) => lit.len() + 2, // + 2 for the quotes
            TokenType::NumberLiteral(lit) => lit.len(),
            TokenType::Symbol(s) => s.len(),
        }
    }
}

#[derive(Debug)]
enum LexerError {
    Generic(&'static str),
    UnexpectedChar(char, usize, i32),
    UnknownSingleSymbol(char, usize, i32),
    UnknownMultiSymbol(char, usize, i32),
}

struct Lexer {
    // TODO: flux of chars instead of raw string
    in_decimal: bool,
    chars: Vec<char>,
    pos_in_line: usize,
    line: i32,
    pos: usize,
    buffer: Vec<char>,
}

fn valid_identifier_char(c: &char) -> bool {
    // TODO support more
    c.is_ascii_alphanumeric()
}

impl Lexer {
    pub fn new(inp: String) -> Lexer {
        Lexer {
            in_decimal: false,
            chars: inp.chars().into_iter().collect(),
            pos_in_line: 0,
            line: 0,
            pos: 0,
            buffer: Vec::new(),
        }
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
        .map(TokenType::NumberLiteral)
    }

    fn string(&mut self) -> Result<TokenType, LexerError> {
        self.until(|&c| c == '"').map(|mut content| {
            content.remove(0);
            // content will include an extra '"'
            TokenType::StringLiteral(content)
        })
    }

    fn identifier(&mut self) -> Result<TokenType, LexerError> {
        self.until(|c| !valid_identifier_char(c))
            .map(TokenType::Identifier)
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
            '+' | '-' | '*' | '/' | ':' | ';' => self.symbol(true),
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
    const y: string = "abc";
    x + y;"#
                .to_string(),
        );

        let expected = vec![
            Token {
                typ: TokenType::Identifier("const".to_string()),
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
                typ: TokenType::NumberLiteral("5".to_string()),
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
                typ: TokenType::NumberLiteral("10_11_2".to_string()),
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
                typ: TokenType::Identifier("const".to_string()),
                start: 21,
                end: 26,
                line: 1,
            },
            Token {
                typ: TokenType::Identifier("y".to_string()),
                start: 26,
                end: 27,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Colon),
                start: 27,
                end: 28,
                line: 1,
            },
            Token {
                typ: TokenType::Identifier("string".to_string()),
                start: 28,
                end: 34,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Eq),
                start: 34,
                end: 35,
                line: 1,
            },
            Token {
                typ: TokenType::StringLiteral("abc".to_string()),
                start: 35,
                end: 40,
                line: 1,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 40,
                end: 41,
                line: 1,
            },
            Token {
                typ: TokenType::Identifier("x".to_string()),
                start: 41,
                end: 42,
                line: 2,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::Plus),
                start: 42,
                end: 43,
                line: 2,
            },
            Token {
                typ: TokenType::Identifier("y".to_string()),
                start: 43,
                end: 44,
                line: 2,
            },
            Token {
                typ: TokenType::Symbol(SymbolType::SemiColon),
                start: 44,
                end: 45,
                line: 2,
            },
        ];

        for (token, expected) in lexer.zip(expected.iter()) {
            assert_eq!(token?, *expected);
        }

        Ok(())
    }

    // TODO number with multiple .
}
