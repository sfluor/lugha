use crate::lexer::{Float, Lexer, LexerError, SymbolType, Token, TokenType};
use core::panic;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    GT,
    LT,
    GTEQ,
    LTEQ,
    EQ,
    NEQ,
}

impl BinOp {
    fn of(self, left: Node, right: Node) -> Node {
        Node::Binary(Box::new(left), self, Box::new(right))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum UnaOp {
    Neg,
    Not,
}

impl UnaOp {
    fn of(self, node: Node) -> Node {
        Node::Unary(self, Box::new(node))
    }
}

type BNode = Box<Node>;

#[derive(Debug, PartialEq, Eq)]
enum Node {
    Binary(BNode, BinOp, BNode),
    Unary(UnaOp, BNode),
    Literal(Value),
}

enum Type {
    String,
    Float,
    Integer,
    // TODO: functions and custom types
}

#[derive(Debug, PartialEq, Eq)]
enum ParserError {
    Lexing(LexerError),
    ConsumeError {
        actual: Option<Token>,
        expected: TokenType,
    },
    InvalidInfix(Token),
    InvalidPrefix(Token),
    UnterminatedUnary(Token),
    UnterminatedGrouping(Token),
}

#[derive(Debug, PartialEq, Eq)]
enum Value {
    String(String),
    Float(Float),
    Integer(i64),
}

trait Eval {
    fn etype(&self) -> Type;
    fn eval(&self) -> Value;
}

struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    fn from_str(str: String) -> Parser {
        Self::new(Lexer::new(str))
    }

    fn next_infix_precedence(&mut self) -> Option<u8> {
        self.lexer
            .peek()?
            .as_ref()
            .map(|t| t.typ.precedence(false))
            .map(Some)
            .unwrap_or(None)
    }

    fn consume(&mut self, expected: TokenType) -> Option<ParserError> {
        let Some(res) = self.lexer.next() else {
            return Some(ParserError::ConsumeError { actual: None, expected: expected });
        };

        let Ok(token) = res else {
            return Some(ParserError::Lexing(res.unwrap_err()));
        };

        if token.typ != expected {
            Some(ParserError::ConsumeError {
                actual: Some(token),
                expected: expected,
            })
        } else {
            None
        }
    }

    fn parse_precedence(&mut self, precedence: u8) -> Option<Result<Node, ParserError>> {
        let Some(res) = self.lexer.next() else {
            return None;
        };

        let Ok(token) = res else {
            return Some(Err(ParserError::Lexing(res.unwrap_err())));
        };

        let parsed = self.parse_prefix(token);
        let Ok(mut node) = parsed else {
            return Some(parsed);
        };

        while let Some(next_precedence) = self.next_infix_precedence() {
            if precedence >= next_precedence {
                break;
            }

            let next_token: Token = self
                .lexer
                .next()
                .expect("Expected next token to exist since precedence existed")
                .expect("Expected no error");

            let TokenType::Symbol(symbol) = next_token.typ else {
 return Some(Err(ParserError::InvalidInfix(next_token)));
            };

            let next_node_res: Result<Node, ParserError> =
                self.parse_precedence(next_precedence)?;
            let Ok(next_node) = next_node_res else {
                return Some(next_node_res);
            };

            node = match symbol {
                SymbolType::Plus => Node::Binary(Box::new(node), BinOp::Add, Box::new(next_node)),
                SymbolType::Minus => todo!(),
                SymbolType::Rparen | SymbolType::Lparen => todo!(),
                SymbolType::Slash => Node::Binary(Box::new(node), BinOp::Div, Box::new(next_node)),
                SymbolType::Star => Node::Binary(Box::new(node), BinOp::Mult, Box::new(next_node)),
                SymbolType::GTEQ => Node::Binary(Box::new(node), BinOp::GTEQ, Box::new(next_node)),
                SymbolType::SemiColon => todo!(),
                SymbolType::GT => todo!(),
                SymbolType::LT => todo!(),
                SymbolType::LTEQ => todo!(),
                SymbolType::Bang => todo!(),
                SymbolType::BangEq => todo!(),
                SymbolType::Eq => todo!(),
                SymbolType::EqEq => todo!(),
                SymbolType::Percentage => todo!(),
                SymbolType::Colon => todo!(),
            }
        }

        Some(Ok(node))
    }

    fn unary(&mut self, token: Token, op: UnaOp) -> Result<Node, ParserError> {
        let Some(result) = self.parse_precedence(token.typ.precedence(true)) else {
            return Err(ParserError::UnterminatedUnary(token.clone()));
        };

        result.map(|n| op.of(n))
    }

    fn grouping(&mut self, token: Token) -> Result<Node, ParserError> {
        let Some(result) = self.parse_precedence(token.typ.precedence(false)) else {
            return Err(ParserError::UnterminatedGrouping(token.clone()));
        };

        if let Some(err) = self.consume(TokenType::Symbol(SymbolType::Rparen)) {
            return Err(err);
        };

        result
    }

    fn parse_prefix(&mut self, token: Token) -> Result<Node, ParserError> {
        match &token.typ {
            TokenType::IntLiteral(_, val) => Ok(Node::Literal(Value::Integer(*val))),
            TokenType::Symbol(symbol) => match symbol {
                SymbolType::Bang => self.unary(token, UnaOp::Not),
                SymbolType::Lparen => self.grouping(token),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            _ => Err(ParserError::InvalidPrefix(token)),
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn test_compare() {
        let mut parser = Parser::from_str("!(5 * 4 >= 2 + 1 / 6)".to_string());

        let node = parser
            .parse_precedence(0)
            .expect("expected node")
            .expect("expected no error");
        assert_eq!(
            node,
            UnaOp::Not.of(BinOp::GTEQ.of(
                BinOp::Mult.of(
                    Node::Literal(Value::Integer(5)),
                    Node::Literal(Value::Integer(4))
                ),
                BinOp::Add.of(
                    Node::Literal(Value::Integer(2)),
                    BinOp::Div.of(
                        Node::Literal(Value::Integer(1)),
                        Node::Literal(Value::Integer(6))
                    )
                )
            ))
        );

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }

    #[test]
    fn test_arithmetic() {
        let mut parser = Parser::from_str("5 * 4 + 2".to_string());

        let node = parser
            .parse_precedence(0)
            .expect("expected node")
            .expect("expected no error");
        assert_eq!(
            node,
            BinOp::Add.of(
                BinOp::Mult.of(
                    Node::Literal(Value::Integer(5)),
                    Node::Literal(Value::Integer(4))
                ),
                Node::Literal(Value::Integer(2))
            )
        );

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }
}
