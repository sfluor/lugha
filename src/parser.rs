use crate::lexer::{Float, KeywordType, Lexer, LexerError, SymbolType, Token, TokenType};
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

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
    fn of(self, left: Expr, right: Expr) -> Expr {
        Expr::Binary(Box::new(left), self, Box::new(right))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnaOp {
    Neg,
    Not,
}

impl UnaOp {
    fn of(self, node: Expr) -> Expr {
        Expr::Unary(self, Box::new(node))
    }
}

type BExpr = Box<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Expr {
    Binary(BExpr, BinOp, BExpr),
    Unary(UnaOp, BExpr),
    Literal(Value),
    Reference(String),
}

#[derive(Debug, PartialEq, Eq)]
enum AssignType {
    CreateConst,
    CreateVar,
    Assign,
}

#[derive(Debug, PartialEq, Eq)]
enum Statement {
    Expr(Expr),
    Assign {
        identifier: String,
        expr: Expr,
        typ: AssignType,
    },
}

impl Statement {
    fn eval(&self, scope: &mut Scope) -> Option<EvalError> {
        match self {
            Statement::Expr(expr) => expr.eval(scope).err(),
            _ => todo!(),
        }
    }

    fn type_check(&self, scope: &mut ParserScope) -> Result<Type, ParserError> {
        match self {
            Statement::Expr(expr) => expr.type_check(scope),
            // TODO: also check that we don't override constants
            Statement::Assign {
                identifier,
                expr,
                typ: assignType,
            } => {
                let typ = expr.type_check(scope)?;
                scope.mark(
                    identifier.clone(),
                    typ.clone(),
                    *assignType == AssignType::CreateConst,
                )?;
                Ok(typ)
            }
        }
    }
}

impl Expr {
    fn statement(self) -> Statement {
        Statement::Expr(self)
    }

    fn type_check(&self, scope: &ParserScope) -> Result<Type, ParserError> {
        match self {
            Expr::Binary(left, op, right) => {
                let ltype = left.type_check(scope)?;
                let rtype = right.type_check(scope)?;

                if ltype != rtype {
                    return Err(ParserError::BinTypeError {
                        left: ltype,
                        right: rtype,
                        expr: self.clone(),
                    });
                }

                match op {
                    BinOp::Add => {
                        // TODO Avoid computing the type multiple times.
                        Type::or3(Type::Integer, Type::Float, Type::String, left, ltype)?;

                        Type::or3(Type::Integer, Type::Float, Type::String, right, rtype)
                    }
                    BinOp::Sub | BinOp::Mult | BinOp::Div => {
                        // We only check left since we already checked that left and right were equal
                        Type::or2(Type::Integer, Type::Float, left, ltype)
                    }
                    BinOp::GT | BinOp::LT | BinOp::GTEQ | BinOp::LTEQ | BinOp::EQ | BinOp::NEQ => {
                        // As long as the two types are equal we're good.
                        Ok(Type::Boolean)
                    }
                }
            }
            Expr::Unary(op, expr) => {
                let typ = expr.type_check(scope)?;
                match op {
                    UnaOp::Neg => Type::or2(Type::Integer, Type::Float, expr, typ),
                    UnaOp::Not => Type::Boolean.is(expr, typ),
                }
            }
            Expr::Literal(l) => Ok(match l {
                Value::String(_) => Type::String,
                Value::Float(_) => Type::Float,
                Value::Integer(_) => Type::Integer,
                Value::Bool(_) => Type::Boolean,
            }),
            Expr::Reference(ident) => scope.get(ident),
        }
    }

    fn eval(&self, scope: &mut Scope) -> Result<Value, EvalError> {
        match self {
            Expr::Binary(left, op, right) => left.eval(scope)?.bin(op, &right.eval(scope)?),
            Expr::Unary(op, node) => node.eval(scope)?.unary(op),
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Reference(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    String,
    Float,
    Integer,
    Boolean,
    // TODO: functions and custom types
}

impl Type {
    // TODO: Could be a macro
    fn or3(
        typ1: Type,
        typ2: Type,
        typ3: Type,
        expr: &Expr,
        actual: Type,
    ) -> Result<Type, ParserError> {
        if actual != typ1 && actual != typ2 && actual != typ3 {
            Err(ParserError::TypesError {
                expected: vec![typ1, typ2, typ3],
                actual,
                expr: expr.clone(),
            })
        } else {
            Ok(actual)
        }
    }

    fn or2(typ1: Type, typ2: Type, expr: &Expr, actual: Type) -> Result<Type, ParserError> {
        if actual != typ1 && actual != typ2 {
            Err(ParserError::TypesError {
                expected: vec![typ1, typ2],
                actual,
                expr: expr.clone(),
            })
        } else {
            Ok(actual)
        }
    }

    fn is(self, expr: &Expr, actual: Type) -> Result<Type, ParserError> {
        if actual != self {
            Err(ParserError::TypeError {
                expected: self,
                actual,
                expr: expr.clone(),
            })
        } else {
            Ok(actual)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ParserError {
    Lexing(LexerError),
    ConsumeError {
        actual: Option<Token>,
        expected: TokenType,
    },
    TypesError {
        expected: Vec<Type>,
        actual: Type,
        expr: Expr,
    },
    BinTypeError {
        left: Type,
        right: Type,
        expr: Expr,
    },
    TypeError {
        expected: Type,
        actual: Type,
        expr: Expr,
    },
    InvalidInfix(Token),
    InvalidPrefix(Token),
    UnterminatedAssignment(Token),
    UnterminatedUnary(Token),
    UnterminatedGrouping(Token),
    DuplicateVariableType(String, Type, Type),
    UnknownVariable(String),
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
    pub fn ex(self) -> Expr {
        Expr::Literal(self)
    }

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

struct Variable {
    val: Value,
    constant: bool,
}

struct ParserScope<'a> {
    parent: Option<Box<&'a ParserScope<'a>>>,
    var_types: HashMap<String, Type>,
    constants: HashSet<String>,
}

impl<'a> ParserScope<'a> {
    fn new() -> ParserScope<'a> {
        ParserScope {
            parent: None,
            var_types: HashMap::new(),
            constants: HashSet::new(),
        }
    }
    fn child(&self) -> ParserScope {
        let mut ch = ParserScope::new();
        ch.parent = Some(Box::new(self));

        ch
    }

    fn mark(&mut self, name: String, typ: Type, constant: bool) -> Result<(), ParserError> {
        match self.var_types.insert(name.clone(), typ.clone()) {
            None => {
                self.constants.insert(name);
                Ok(())
            }
            Some(val) => Err(ParserError::DuplicateVariableType(name, val, typ)),
        }
    }

    // TODO: don't use hash map to index variables
    fn get(&self, name: &str) -> Result<Type, ParserError> {
        if let Some(val) = self.var_types.get(name) {
            return Ok(val.clone());
        };

        if let Some(ref par) = self.parent {
            return par.get(name);
        };

        Err(ParserError::UnknownVariable(name.to_string()))
    }
}

struct Scope<'a> {
    parent: Option<Box<&'a Scope<'a>>>,
    locals: HashMap<&'a str, Value>,
    constants: HashSet<&'a str>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            parent: None,
            locals: HashMap::new(),
            constants: HashSet::new(),
        }
    }

    fn child(&self) -> Scope {
        let mut ch = Scope::new();
        ch.parent = Some(Box::new(self));

        ch
    }

    fn set(&mut self, name: &'a str, val: Value) {
        self.locals.insert(name, val);
    }

    // TODO: don't use hash map to index variables
    fn get(&self, name: &str) -> Option<&Value> {
        if let Some(val) = self.locals.get(name) {
            return Some(val);
        };

        if let Some(ref par) = self.parent {
            return par.get(name);
        };

        None
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Statement, ParserError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        self.parse_statement()
    }
}

struct Parser<'a> {
    lexer: Peekable<Lexer>,
    scope: ParserScope<'a>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser<'a> {
        Parser {
            lexer: lexer.peekable(),
            scope: ParserScope::new(),
        }
    }

    fn from_str(str: &'a str) -> Parser {
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

    fn consume(&mut self, expected: TokenType) -> Result<Token, ParserError> {
        let Some(res) = self.lexer.next() else {
            return Err(ParserError::ConsumeError {
                actual: None,
                expected: expected,
            });
        };

        let Ok(token) = res else {
            return Err(ParserError::Lexing(res.unwrap_err()));
        };

        if token.typ != expected {
            Err(ParserError::ConsumeError {
                actual: Some(token),
                expected: expected,
            })
        } else {
            Ok(token)
        }
    }

    // Parses const x = ... / var x = ...
    fn parse_declaration(&mut self, constant: bool) -> Option<Result<Statement, ParserError>> {
        let token = self.consume(TokenType::Keyword(if constant {
            KeywordType::Const
        } else {
            KeywordType::Var
        }));
        if let Err(err) = token {
            return Some(Err(err));
        }

        let token = token.expect("Checked before");

        let identifier = match self.lexer.next() {
            None => return Some(Err(ParserError::UnterminatedAssignment(token))),
            Some(res) => match res {
                Err(err) => return Some(Err(ParserError::Lexing(err))),
                Ok(tok) => match tok.typ {
                    TokenType::Identifier(id) => id,
                    _ => {
                        return Some(Err(ParserError::ConsumeError {
                            actual: Some(tok),
                            expected: TokenType::Identifier("any".to_string()),
                        }))
                    }
                },
            },
        };

        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::Eq)) {
            return Some(Err(err));
        }

        let val = match self.parse_precedence(0) {
            Some(res) => match res {
                Ok(val) => val,
                Err(err) => return Some(Err(err)),
            },
            None => {
                return Some(Err(ParserError::UnterminatedAssignment(token)));
            }
        };

        Some(Ok(Statement::Assign {
            identifier,
            expr: val,
            typ: if constant {
                AssignType::CreateConst
            } else {
                AssignType::CreateVar
            },
        }))
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, ParserError>> {
        let result: Option<Result<Statement, ParserError>> = match self.lexer.peek() {
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Const) => {
                self.parse_declaration(true)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Var) => {
                self.parse_declaration(false)
            }
            None => {
                return None;
            }
            _ => self.parse_precedence(0).map(|r| r.map(Statement::Expr)),
        };

        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::SemiColon)) {
            return Some(Err(err));
        }

        if let Some(Ok(ref stmt)) = result {
            if let Err(err) = stmt.type_check(&mut self.scope) {
                return Some(Err(err));
            }
        }

        result
    }

    fn parse_precedence(&mut self, precedence: u8) -> Option<Result<Expr, ParserError>> {
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

            let next_node_res: Result<Expr, ParserError> =
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

    fn unary(&mut self, token: Token, op: UnaOp) -> Result<Expr, ParserError> {
        let Some(result) = self.parse_precedence(token.typ.precedence(true)) else {
            return Err(ParserError::UnterminatedUnary(token.clone()));
        };

        result.map(|n| op.of(n))
    }

    fn grouping(&mut self, token: Token) -> Result<Expr, ParserError> {
        let Some(result) = self.parse_precedence(token.typ.precedence(false)) else {
            return Err(ParserError::UnterminatedGrouping(token.clone()));
        };

        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::Rparen)) {
            return Err(err);
        };

        result
    }

    fn parse_prefix(&mut self, token: Token) -> Result<Expr, ParserError> {
        match token.typ {
            TokenType::IntLiteral(_, ref val) => Ok(Expr::Literal(Value::Integer(*val))),
            TokenType::FloatLiteral(_, Float(ref f)) => Ok(Expr::Literal(Value::Float(Float(*f)))),
            TokenType::StringLiteral(val) => Ok(Expr::Literal(Value::String(val.clone()))),
            TokenType::Symbol(ref symbol) => match symbol {
                SymbolType::Bang => self.unary(token, UnaOp::Not),
                SymbolType::Minus => self.unary(token, UnaOp::Neg),
                SymbolType::Lparen => self.grouping(token),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            TokenType::Keyword(ref val) => match val {
                KeywordType::False => Ok(Expr::Literal(Value::Bool(false))),
                KeywordType::True => Ok(Expr::Literal(Value::Bool(true))),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            TokenType::Identifier(identifier) => Ok(Expr::Reference(identifier)),
            _ => Err(ParserError::InvalidPrefix(token)),
        }
    }
}

mod test {
    use core::panic;

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
            r#""a" <= "b""#,
            r#""a" < "b""#,
            r#""zba" > "zb""#,
            r#""" == """#,
            r#""abc" == "a" + "b" + "c""#,
        ];

        for expr in exprs {
            let mut node = Parser::from_str(expr)
                .parse_precedence(0)
                .expect("expect node")
                .expect("no error");

            let mut scope = Scope::new();
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

            let mut scope = Scope::new();
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
    fn test_statements() -> Result<(), ParserError> {
        let mut parser = Parser::from_str(
            r#"5 + 3 * -1;
            const x =  5.1;
            10 / 2;
            "ab" + "cd";
            1 == 6;"#,
        );

        let expected_nodes = vec![
            BinOp::Add
                .of(
                    Value::Integer(5).ex(),
                    BinOp::Mult.of(
                        Value::Integer(3).ex(),
                        UnaOp::Neg.of(Value::Integer(1).ex()),
                    ),
                )
                .statement(),
            Statement::Assign {
                identifier: "x".to_string(),
                expr: Value::Float(Float(5.1)).ex(),
                typ: AssignType::CreateConst,
            },
            BinOp::Div
                .of(Value::Integer(10).ex(), Value::Integer(2).ex())
                .statement(),
            BinOp::Add
                .of(
                    Value::String("ab".to_string()).ex(),
                    Value::String("cd".to_string()).ex(),
                )
                .statement(),
            BinOp::EQ
                .of(Value::Integer(1).ex(), Value::Integer(6).ex())
                .statement(),
        ];
        let expected_evals = vec![
            Some(Value::Integer(2)),
            None,
            Some(Value::Integer(5)),
            Some(Value::String("abcd".to_string())),
            Some(Value::Bool(false)),
        ];
        let statements: Vec<Result<Statement, ParserError>> = parser.collect();

        assert_eq!(expected_nodes.len(), statements.len());
        assert_eq!(expected_evals.len(), statements.len());

        for ((rstatement, expected), expected_val) in statements
            .into_iter()
            .zip(expected_nodes.into_iter())
            .zip(expected_evals.into_iter())
        {
            let statement = rstatement?;
            let mut scope = Scope::new();
            assert_eq!(
                statement, expected,
                "Expected {:?} to be {:?}",
                statement, expected
            );

            let evaluated = match &statement {
                Statement::Expr(e) => match e.eval(&mut scope) {
                    Err(err) => {
                        panic!("Error: {:?}", err);
                    }
                    Ok(val) => Some(val),
                },
                Statement::Assign {
                    identifier,
                    expr,
                    typ,
                } => None,
            };

            assert_eq!(
                evaluated, expected_val,
                "Expected {:?} to eval to {:?}",
                statement, expected_val,
            );
        }

        Ok(())
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
                BinOp::Mult.of(Value::Integer(5).ex(), Value::Integer(4).ex()),
                BinOp::Add.of(
                    Value::Integer(2).ex(),
                    BinOp::Div.of(Value::Integer(1).ex(), Value::Integer(6).ex())
                )
            ))
        );

        let mut scope = Scope::new();
        assert_eq!(node.eval(&mut scope), Ok(Value::Bool(false)));

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }

    #[test]
    fn test_const() {
        let mut parser = Parser::from_str(
            r#"const x = 10;
            const y = x;
            const z = "a" + "b";
            "#,
        );

        let expected_stmts = vec![
            Statement::Assign {
                identifier: "x".to_string(),
                expr: Value::Integer(10).ex(),
                typ: AssignType::CreateConst,
            },
            Statement::Assign {
                identifier: "y".to_string(),
                expr: Expr::Reference("x".to_string()),
                typ: AssignType::CreateConst,
            },
            Statement::Assign {
                identifier: "z".to_string(),
                expr: BinOp::Add.of(
                    Value::String("a".to_string()).ex(),
                    Value::String("b".to_string()).ex(),
                ),
                typ: AssignType::CreateConst,
            },
        ];

        for expected in expected_stmts {
            let stmt = parser
                .next()
                .expect("expected statement")
                .expect("expected no error");
            assert_eq!(stmt, expected, "Expected {:?} to be {:?}", stmt, expected);
        }

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
                BinOp::Mult.of(Value::Integer(5).ex(), Value::Integer(4).ex()),
                Value::Integer(2).ex()
            )
        );

        let mut scope = Scope::new();
        assert_eq!(node.eval(&mut scope), Ok(Value::Integer(22)));

        assert_eq!(parser.parse_precedence(0), None);
        assert_eq!(parser.parse_precedence(0), None);
    }
}
// TODO a*b = 5 should fail
