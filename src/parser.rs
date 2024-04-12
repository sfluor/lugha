use crate::lexer::{Float, Lexer, LexerError, SymbolType, Token, TokenType};
use core::panic;
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Boolean,
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
enum EvalError {
    BinTypeMismatch(Value, Value),
    UnimplementedBinaryOperator(Value, BinOp),
    UnimplementedUnaryOperator(Value, UnaOp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Value {
    String(String),
    Float(Float),
    Integer(i64),
    Bool(bool),
}

impl Value {
    pub fn unary(self, op: &UnaOp) -> Result<Value, EvalError> {
        match op {
            UnaOp::Neg => match self {
                Value::Float(Float(f)) => Ok(Value::Float(Float(-f))),
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::String(_) | Value::Bool(_) => {
                    Err(EvalError::UnimplementedUnaryOperator(self, op.clone()))
                }
            },
            UnaOp::Not => match self {
                Value::String(_) | Value::Float(_) | Value::Integer(_) => {
                    Err(EvalError::UnimplementedUnaryOperator(self, op.clone()))
                }
                Value::Bool(v) => Ok(Value::Bool(!v)),
            },
        }
    }

    pub fn bin(self, op: &BinOp, other: &Value) -> Result<Value, EvalError> {
        match self {
            Value::String(ref left) => {
                let Value::String(right) = other else {
                    return Err(EvalError::BinTypeMismatch(self, other.clone()));
                };

                return Ok(match op {
                    BinOp::Add => Value::String(left.clone() + &right[..]),
                    BinOp::GT => Value::Bool(*left > *right),
                    BinOp::LT => Value::Bool(*left < *right),
                    BinOp::GTEQ => Value::Bool(*left >= *right),
                    BinOp::LTEQ => Value::Bool(*left <= *right),
                    BinOp::EQ => Value::Bool(*left == *right),
                    BinOp::NEQ => Value::Bool(*left != *right),
                    BinOp::Sub | BinOp::Mult | BinOp::Div => {
                        return Err(EvalError::UnimplementedBinaryOperator(self, op.clone()));
                    }
                });
            }
            Value::Integer(left) => {
                let Value::Integer(right) = other else {
                    return Err(EvalError::BinTypeMismatch(self, other.clone()));
                };

                return Ok(match op {
                    BinOp::Add => Value::Integer(left + right),
                    BinOp::GT => Value::Bool(left > *right),
                    BinOp::LT => Value::Bool(left < *right),
                    BinOp::GTEQ => Value::Bool(left >= *right),
                    BinOp::LTEQ => Value::Bool(left <= *right),
                    BinOp::EQ => Value::Bool(left == *right),
                    BinOp::NEQ => Value::Bool(left != *right),
                    BinOp::Sub => Value::Integer(left - right),
                    BinOp::Mult => Value::Integer(left * right),
                    BinOp::Div => Value::Integer(left / right),
                });
            }
            Value::Float(Float(left)) => {
                let Value::Float(Float(right)) = other else {
                    return Err(EvalError::BinTypeMismatch(self, other.clone()));
                };

                return Ok(match op {
                    BinOp::Add => Value::Float(Float(left + right)),
                    BinOp::GT => Value::Bool(left > *right),
                    BinOp::LT => Value::Bool(left < *right),
                    BinOp::GTEQ => Value::Bool(left >= *right),
                    BinOp::LTEQ => Value::Bool(left <= *right),
                    BinOp::EQ => Value::Bool(left == *right),
                    BinOp::NEQ => Value::Bool(left != *right),
                    BinOp::Sub => Value::Float(Float(left - right)),
                    BinOp::Mult => Value::Float(Float(left * right)),
                    BinOp::Div => Value::Float(Float(left / right)),
                });
            }
            Value::Bool(left) => match op {
                BinOp::Add | BinOp::Sub | BinOp::GT => todo!(),
                BinOp::Sub => todo!(),
                BinOp::Mult => todo!(),
                BinOp::Div => todo!(),
                BinOp::GT => todo!(),
                BinOp::LT => todo!(),
                BinOp::GTEQ => todo!(),
                BinOp::LTEQ => todo!(),
                BinOp::EQ => todo!(),
                BinOp::NEQ => todo!(),
            },
        }
    }
}

trait Eval {
    fn etype(&self) -> Type;
    fn eval(&self, scope: &mut Scope) -> Result<Value, EvalError>;
}

struct Scope;

impl Eval for Node {
    fn etype(&self) -> Type {
        todo!()
    }

    fn eval(&self, scope: &mut Scope) -> Result<Value, EvalError> {
        match self {
            Node::Binary(left, op, right) => left.eval(scope)?.bin(op, &right.eval(scope)?),
            Node::Unary(op, node) => node.eval(scope)?.unary(op),
            Node::Literal(value) => Ok(value.clone()),
        }
    }
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

    fn from_str(str: &str) -> Parser {
        Self::new(Lexer::new(str))
    }

    fn next_infix_precedence(&mut self) -> Option<Result<u8, ParserError>> {
        Some(
            self.lexer
                .peek()?
                .as_ref()
                .map(|t| t.typ.precedence(false))
                .map_err(|err| ParserError::Lexing(err.clone())),
        )
    }

    fn consume(&mut self, expected: TokenType) -> Option<ParserError> {
        let Some(res) = self.lexer.next() else {
            return Some(ParserError::ConsumeError {
                actual: None,
                expected: expected,
            });
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

        loop {
            let Some(res_precedence) = self.next_infix_precedence() else {
                break;
            };

            let Ok(next_precedence) = res_precedence else {
                return Some(Err(res_precedence.unwrap_err()));
            };

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

            let op = match symbol {
                SymbolType::Plus => BinOp::Add,
                SymbolType::Minus => BinOp::Sub,
                SymbolType::Slash => BinOp::Div,
                SymbolType::Star => BinOp::Mult,
                SymbolType::GTEQ => BinOp::GTEQ,
                SymbolType::GT => BinOp::GT,
                SymbolType::LT => BinOp::LT,
                SymbolType::LTEQ => BinOp::LTEQ,
                SymbolType::EqEq => BinOp::EQ,
                SymbolType::BangEq => BinOp::NEQ,
                SymbolType::Bang => todo!(),
                SymbolType::Eq => todo!(),
                SymbolType::Percentage => todo!(),
                SymbolType::Colon => todo!(),
                SymbolType::SemiColon => todo!(),
                SymbolType::Rparen | SymbolType::Lparen => todo!(),
            };

            node = op.of(node, next_node)
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
            TokenType::FloatLiteral(_, Float(f)) => Ok(Node::Literal(Value::Float(Float(*f)))),
            TokenType::StringLiteral(val) => Ok(Node::Literal(Value::String(val.clone()))),
            TokenType::Symbol(symbol) => match symbol {
                SymbolType::Bang => self.unary(token, UnaOp::Not),
                SymbolType::Minus => self.unary(token, UnaOp::Neg),
                SymbolType::Lparen => self.grouping(token),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            TokenType::Keyword(val) => match val.as_str() {
                "false" => Ok(Node::Literal(Value::Bool(false))),
                "true" => Ok(Node::Literal(Value::Bool(true))),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            _ => Err(ParserError::InvalidPrefix(token)),
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn should_be_true() {
        let exprs = vec![
            "5 > 4",
            "4 < 5",
            "5 >= 4",
            "4 <= 5",
            "4/8 <= 1",
            "-1 < 1",
            "-1.2 < 1.3",
            "true",
            "!false",
            r#""a" != "ab""#,
            r#""a" != "b""#,
            r#""a" == "a""#,
            r#""" == """#,
        ];

        for expr in exprs {
            let mut node = Parser::from_str(expr)
                .parse_precedence(0)
                .expect("expect node")
                .expect("no error");

            let mut scope = Scope {};
            assert_eq!(
                node.eval(&mut scope),
                Ok(Value::Bool(true)),
                "Evaluating {}, got {:?}",
                expr,
                node,
            );
        }
    }

    #[test]
    fn should_be_false() {
        let exprs = vec![
            "5 < 4",
            "4 > 5",
            "5 <= 4",
            "4 >= 5",
            "4/8 >= 1",
            "-1 > 1",
            "-1.2 > 1.3",
            "false",
            "!true",
            r#""a" == "ab""#,
            r#""a" == "b""#,
        ];

        for expr in exprs {
            let mut node = Parser::from_str(expr)
                .parse_precedence(0)
                .expect("expect node")
                .expect("no error");

            let mut scope = Scope {};
            assert_eq!(
                node.eval(&mut scope),
                Ok(Value::Bool(false)),
                "Evaluating {}, got {:?}",
                expr,
                node,
            );
        }
    }

    #[test]
    fn test_compare() {
        let mut parser = Parser::from_str("!(5 * 4 >= 2 + 1 / 6)");

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

        let mut scope = Scope {};
        assert_eq!(node.eval(&mut scope), Ok(Value::Bool(false)));

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }

    #[test]
    fn test_arithmetic() {
        let mut parser = Parser::from_str("5 * 4 + 2");

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

        let mut scope = Scope {};
        assert_eq!(node.eval(&mut scope), Ok(Value::Integer(22)));

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }
}
