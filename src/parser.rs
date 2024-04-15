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

fn ident(name: &str) -> Expr {
    Expr::Identifier(name.to_owned())
}

fn boolean(b: bool) -> Expr {
    Value::Bool(b).ex()
}

fn int(x: i64) -> Expr {
    Value::Integer(x).ex()
}

fn string(s: &str) -> Expr {
    Value::String(s.to_string()).ex()
}

fn float(f: f64) -> Expr {
    Value::Float(Float(f)).ex()
}

impl BinOp {
    fn of(self, left: Expr, right: Expr) -> Expr {
        Expr::Binary(left.b(), self, right.b())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnaOp {
    Neg,
    Not,
}

impl UnaOp {
    fn of(self, node: Expr) -> Expr {
        Expr::Unary(self, node.b())
    }
}

type BExpr = Box<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Expr {
    Binary(BExpr, BinOp, BExpr),
    Unary(UnaOp, BExpr),
    Literal(Value),
    Identifier(String),
    Assign { identifier: String, expr: BExpr },
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum AssignType {
    CreateConst,
    CreateVar,
}

#[derive(Debug, PartialEq, Eq)]
enum Statement {
    Expr(Expr),
    Assign {
        identifier: String,
        expr: Expr,
        typ: AssignType,
    },
    Conditional {
        branches: Vec<(Expr, Vec<Statement>)>,
        default: Vec<Statement>,
    },
}

impl Statement {
    fn eval(self, scope: &mut Scope) -> Result<(), EvalError> {
        match self {
            Statement::Expr(expr) => expr.eval(scope).map(|_| ()),
            Statement::Assign {
                identifier,
                expr,
                typ,
            } => {
                let val = expr.eval(scope)?;
                scope.set(identifier, val, typ == AssignType::CreateConst)
            }
            Statement::Conditional { branches, default } => {
                for (expr, branch) in branches {
                    if let Value::Bool(true) = expr.eval(scope)? {
                        for stmt in branch {
                            stmt.eval(scope)?;
                        }
                        return Ok(());
                    };
                }

                for stmt in default {
                    stmt.eval(scope)?;
                }

                Ok(())
            }
        }
    }

    fn type_check(&self, scope: &mut ParserScope) -> Result<Type, ParserError> {
        match self {
            Statement::Expr(expr) => expr.type_check(scope),
            // TODO: also check that we don't override constants
            Statement::Assign {
                identifier,
                expr,
                typ: assign_type,
            } => {
                let typ = expr.type_check(scope)?;
                scope.mark(
                    identifier.clone(),
                    typ.clone(),
                    *assign_type == AssignType::CreateConst,
                )?;
                Ok(typ)
            }
            Statement::Conditional { branches, default } => {
                for (cond, branch) in branches {
                    let cond_type = cond.type_check(scope)?;
                    if cond_type != Type::Boolean {
                        return Err(ParserError::InvalidConditionType(cond.clone(), cond_type));
                    }

                    for stmt in branch {
                        stmt.type_check(scope)?;
                    }
                }

                for stmt in default {
                    stmt.type_check(scope)?;
                }

                Ok(Type::Statement)
            }
        }
    }
}

impl Expr {
    fn b(self) -> BExpr {
        Box::new(self)
    }

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
            Expr::Identifier(ident) => scope.get(ident),
            Expr::Assign { identifier, expr } => {
                scope.can_be_assigned(identifier, expr)?;
                expr.type_check(scope)
            }
        }
    }

    fn eval(self, scope: &mut Scope) -> Result<Value, EvalError> {
        match self {
            Expr::Binary(left, op, right) => left.eval(scope)?.bin(&op, &right.eval(scope)?),
            Expr::Unary(op, node) => node.eval(scope)?.unary(&op),
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Identifier(ident) => match scope.get(&ident) {
                // TODO: don't clone
                Some(val) => Ok(val.clone()),
                None => Err(EvalError::UnknownVariableReference(ident.to_string())),
            },
            Expr::Assign { identifier, expr } => {
                let val = expr.eval(scope)?;
                // TODO: no clone
                scope.set(identifier, val.clone(), false)?;
                Ok(val)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    String,
    Float,
    Integer,
    Boolean,
    // Used for statement blocks that do not return anything.
    Statement,
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
    InvalidAssignment(Expr),
    InvalidAssignmentToConst(String, Expr),
    InvalidConditionType(Expr, Type),
}

#[derive(Debug, PartialEq, Eq)]
enum EvalError {
    BinTypeMismatch(Value, Value),
    UnimplementedBinaryOperator(Value, BinOp),
    UnimplementedUnaryOperator(Value, UnaOp),
    UnknownVariableReference(String),
    OverridingConstant(String, Value),
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
            Value::Bool(left) => {
                let Value::Bool(right) = other else {
                    return Err(EvalError::BinTypeMismatch(self, other.clone()));
                };

                return Ok(match op {
                    BinOp::GT => Value::Bool(left > *right),
                    BinOp::LT => Value::Bool(left < *right),
                    BinOp::GTEQ => Value::Bool(left >= *right),
                    BinOp::LTEQ => Value::Bool(left <= *right),
                    BinOp::EQ => Value::Bool(left == *right),
                    BinOp::NEQ => Value::Bool(left != *right),
                    BinOp::Mult => Value::Bool(left && *right),
                    BinOp::Add => Value::Bool(left || *right),
                    BinOp::Div | BinOp::Sub => unimplemented!(),
                });
            }
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
                if constant {
                    self.constants.insert(name);
                }
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

    fn can_be_assigned(&self, identifier: &str, expr: &Expr) -> Result<(), ParserError> {
        if self.constants.contains(identifier) {
            return Err(ParserError::InvalidAssignmentToConst(
                identifier.to_string(),
                expr.clone(),
            ));
        }

        if let Some(ref par) = self.parent {
            return par.can_be_assigned(identifier, expr);
        };

        Ok(())
    }
}

struct Scope<'a> {
    parent: Option<Box<&'a Scope<'a>>>,
    locals: HashMap<String, Value>,
    constants: HashSet<String>,
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

    fn set(&mut self, name: String, val: Value, constant: bool) -> Result<(), EvalError> {
        if self.constants.contains(name.as_str()) {
            return Err(EvalError::OverridingConstant(name.to_string(), val));
        }

        if constant {
            self.constants.insert(name.to_string());
        }

        self.locals.insert(name.to_string(), val);

        Ok(())
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
        if self.error {
            None
        } else {
            self.parse_statement().inspect(|r| self.error = r.is_err())
        }
    }
}

struct Parser<'a> {
    error: bool,
    lexer: Peekable<Lexer>,
    scope: ParserScope<'a>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser<'a> {
        Parser {
            error: false,
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

    fn parse_assignment(
        &mut self,
        identifier: String,
        typ: AssignType,
        token: Token,
    ) -> Option<Result<Statement, ParserError>> {
        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::Eq)) {
            return Some(Err(err));
        }

        let val = match self.parse_new_expr() {
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
            typ,
        }))
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

        let typ = if constant {
            AssignType::CreateConst
        } else {
            AssignType::CreateVar
        };

        self.parse_assignment(identifier, typ, token)
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, ParserError>> {
        let mut expect_colon = true;
        let result: Option<Result<Statement, ParserError>> = match self.lexer.peek() {
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Const) => {
                self.parse_declaration(true)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Var) => {
                self.parse_declaration(false)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::If) => {
                expect_colon = false;
                self.parse_conditional()
            }
            None => {
                return None;
            }
            _ => self.parse_new_expr().map(|r| r.map(Statement::Expr)),
        };

        if expect_colon {
            if let Err(err) = self.consume(TokenType::Symbol(SymbolType::SemiColon)) {
                return Some(Err(err));
            }
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

            node = match symbol {
                SymbolType::Plus => BinOp::Add.of(node, next_node),
                SymbolType::Minus => BinOp::Sub.of(node, next_node),
                SymbolType::Slash => BinOp::Div.of(node, next_node),
                SymbolType::Star => BinOp::Mult.of(node, next_node),
                SymbolType::GTEQ => BinOp::GTEQ.of(node, next_node),
                SymbolType::GT => BinOp::GT.of(node, next_node),
                SymbolType::LT => BinOp::LT.of(node, next_node),
                SymbolType::LTEQ => BinOp::LTEQ.of(node, next_node),
                SymbolType::EqEq => BinOp::EQ.of(node, next_node),
                SymbolType::BangEq => BinOp::NEQ.of(node, next_node),
                SymbolType::Eq => {
                    let Expr::Identifier(name) = node else {
                        return Some(Err(ParserError::InvalidAssignment(node)));
                    };

                    Expr::Assign {
                        identifier: name,
                        expr: next_node.b(),
                    }
                }
                SymbolType::Bang => todo!(),
                SymbolType::Percentage => todo!(),
                SymbolType::Colon => todo!(),
                SymbolType::SemiColon => todo!(),
                SymbolType::RBracket
                | SymbolType::LBracket
                | SymbolType::Rparen
                | SymbolType::Lparen => todo!(),
            };
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
            TokenType::Identifier(identifier) => Ok(Expr::Identifier(identifier)),
        }
    }

    fn lex_until<C, F, T>(&mut self, cond: C, mut consumer: F) -> Result<Vec<T>, ParserError>
    where
        C: Fn(&Token) -> bool,
        F: FnMut(&mut Self) -> Option<Result<T, ParserError>>,
    {
        let mut acc = Vec::new();
        loop {
            match self.lexer.peek() {
                None => return Ok(acc),
                Some(Err(err)) => return Err(ParserError::Lexing(err.clone())),
                Some(Ok(token)) => {
                    if cond(token) {
                        return Ok(acc);
                    }

                    if let Some(data) = consumer(self) {
                        acc.push(data?);
                    } else {
                        return Ok(acc);
                    }
                }
            }
        }
    }

    fn parse_block(&mut self) -> Option<Result<Vec<Statement>, ParserError>> {
        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::LBracket)) {
            return Some(Err(err));
        }

        let block = self.lex_until(
            |token| token.typ == TokenType::Symbol(SymbolType::RBracket),
            |this| this.parse_statement(),
        );
        if let Err(err) = block {
            return Some(Err(err));
        }

        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::RBracket)) {
            return Some(Err(err));
        }
        self.lexer.peek();

        Some(Ok(block.unwrap()))
    }

    fn parse_condition_branch(
        &mut self,
        typ: KeywordType,
    ) -> Option<Result<(Expr, Vec<Statement>), ParserError>> {
        let Some(Ok(TokenType::Keyword(ktyp))) =
            self.lexer.peek().map(|x| x.as_ref().map(|t| &t.typ))
        else {
            return None;
        };

        if typ != *ktyp {
            return None;
        }

        if let Err(err) = self.consume(TokenType::Keyword(typ)) {
            return Some(Err(err));
        }

        let cond = self.parse_new_expr()?;
        if let Err(err) = cond {
            return Some(Err(err));
        }
        let cond = cond.unwrap();
        let if_true = self.parse_block()?;
        if let Err(err) = if_true {
            return Some(Err(err));
        }

        let tuple: (Expr, Vec<Statement>) = (cond, if_true.unwrap());

        Some(Ok(tuple))
    }

    fn parse_conditional(&mut self) -> Option<Result<Statement, ParserError>> {
        let mut branches = Vec::new();
        let if_parse = self.parse_condition_branch(KeywordType::If)?;
        let Ok((cond, statements)) = if_parse else {
            return Some(Err(if_parse.unwrap_err()));
        };
        branches.push((cond, statements));

        loop {
            match self.parse_condition_branch(KeywordType::Elif) {
                None => {
                    break;
                }
                Some(Err(err)) => return Some(Err(err)),
                Some(Ok(branch)) => branches.push(branch),
            }
        }

        let mut default = Vec::new();
        if let Some(Ok(TokenType::Keyword(KeywordType::Else))) =
            self.lexer.peek().map(|x| x.as_ref().map(|t| &t.typ))
        {
            if let Err(err) = self.consume(TokenType::Keyword(KeywordType::Else)) {
                return Some(Err(err));
            }

            match self.parse_block() {
                None => (),
                Some(Ok(r)) => default = r,
                Some(Err(err)) => {
                    return Some(Err(err));
                }
            }
        };

        Some(Ok(Statement::Conditional { branches, default }))
    }

    fn parse_new_expr(&mut self) -> Option<Result<Expr, ParserError>> {
        self.parse_precedence(0)
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
            "true > false",
            "true >= false",
            "true == true",
            "true == !false",
            "true * true",
            "true + false",
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
            let node = Parser::from_str(expr)
                .parse_new_expr()
                .expect("expect node")
                .expect("no error");

            let mut scope = Scope::new();
            let val = node.eval(&mut scope);
            assert_eq!(
                val,
                Ok(Value::Bool(true)),
                "Evaluating {}, got {:?}",
                expr,
                val,
            );
        }
    }

    #[test]
    fn should_fail_to_parse() {
        let exprs = vec![
            (
                "5 < 1.0;",
                ParserError::BinTypeError {
                    left: Type::Integer,
                    right: Type::Float,
                    expr: BinOp::LT.of(int(5), float(1.0)),
                },
            ),
            (
                r#""a" + 1.0;"#,
                ParserError::BinTypeError {
                    left: Type::String,
                    right: Type::Float,
                    expr: BinOp::Add.of(string("a"), float(1.0)),
                },
            ),
            (
                r"const x = 10; x = 4;",
                ParserError::InvalidAssignmentToConst("x".to_string(), int(4)),
            ),
        ];

        for (expr, expected_err) in exprs {
            let mut parser = Parser::from_str(expr);
            let mut errored = false;
            while let Some(res) = parser.next() {
                match res {
                    Err(err) => {
                        assert_eq!(err, expected_err);
                        errored = true;
                    }
                    Ok(_) => {}
                }
            }

            assert!(errored, "expected error when parsing: {expr:?}")
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
            let node = Parser::from_str(expr)
                .parse_new_expr()
                .expect("expect node")
                .expect("no error");

            let mut scope = Scope::new();
            let val = node.eval(&mut scope);
            assert_eq!(
                val,
                Ok(Value::Bool(false)),
                "Evaluating {}, got {:?}",
                expr,
                val,
            );
        }
    }

    fn assert_statements(
        code: &str,
        expected_nodes: &[Statement],
        expected_evals: &[Option<Value>],
    ) -> Result<(), ParserError> {
        let parser = Parser::from_str(code);
        let statements: Vec<Result<Statement, ParserError>> = parser.collect();

        assert_eq!(
            expected_nodes.len(),
            statements.len(),
            "\nExpect: {expected_nodes:?}\n---\nActual:{statements:?}"
        );
        assert_eq!(expected_evals.len(), statements.len());

        let mut scope = Scope::new();
        for ((rstatement, expected), expected_val) in statements
            .into_iter()
            .zip(expected_nodes.into_iter())
            .zip(expected_evals.into_iter())
        {
            let statement = rstatement?;
            assert_eq!(
                statement, *expected,
                "Expected {:?} to be {:?}",
                statement, expected
            );

            let repr = format!("{:?}", statement);

            let evaluated = match statement {
                Statement::Expr(e) => match e.eval(&mut scope) {
                    Err(err) => {
                        panic!("Error: {:?}", err);
                    }
                    Ok(val) => Some(val),
                },
                _ => match statement.eval(&mut scope) {
                    Err(err) => {
                        panic!("Assignment error: {:?}", err);
                    }
                    Ok(_) => None,
                },
            };

            assert_eq!(
                evaluated, *expected_val,
                "Expected {} to eval to {:?}",
                repr, expected_val,
            );
        }

        Ok(())
    }

    #[test]
    fn test_conditions() -> Result<(), ParserError> {
        let code = r#"var x = 4.1;
        if x > 1.0 {
            x = x + 0.1;
            x = 2.0 * x;
        } elif false {
            x = 4.2;
        } else {
            if x <= 10.0 {
                x = 1.1;
            }
        }
        "#;
        let expected_nodes: Vec<Statement> = vec![
            Statement::Assign {
                identifier: "x".to_string(),
                expr: float(4.1),
                typ: AssignType::CreateVar,
            },
            Statement::Conditional {
                branches: vec![
                    (
                        BinOp::GT.of(ident("x"), float(1.0)),
                        vec![
                            Expr::Assign {
                                identifier: "x".to_owned(),
                                expr: BinOp::Add.of(ident("x"), float(0.1)).b(),
                            }
                            .statement(),
                            Expr::Assign {
                                identifier: "x".to_owned(),
                                expr: BinOp::Mult.of(float(2.0), ident("x")).b(),
                            }
                            .statement(),
                        ],
                    ),
                    (
                        boolean(false),
                        vec![Expr::Assign {
                            identifier: "x".to_owned(),
                            expr: float(4.2).b(),
                        }
                        .statement()],
                    ),
                ],
                default: vec![Statement::Conditional {
                    branches: vec![(
                        BinOp::LTEQ.of(ident("x"), float(10.0)),
                        vec![Expr::Assign {
                            identifier: "x".to_owned(),
                            expr: float(1.1).b(),
                        }
                        .statement()],
                    )],
                    default: vec![],
                }],
            },
        ];
        let expected_evals: Vec<Option<Value>> = vec![None, None];

        assert_statements(code, &expected_nodes, &expected_evals)
    }

    #[test]
    fn test_statements() -> Result<(), ParserError> {
        let code = r#"5 + 3 * -1;
            const x =  5.1;
            var y = "z";
            10 / 2;
            "ab" + "cd";
            x + 2.0;
            const z = y = y + "a";
            z;
            1 == 6;"#;
        let expected_nodes: Vec<Statement> = vec![
            BinOp::Add
                .of(int(5), BinOp::Mult.of(int(3), UnaOp::Neg.of(int(1))))
                .statement(),
            Statement::Assign {
                identifier: "x".to_string(),
                expr: float(5.1),
                typ: AssignType::CreateConst,
            },
            Statement::Assign {
                identifier: "y".to_string(),
                expr: string("z"),
                typ: AssignType::CreateVar,
            },
            BinOp::Div.of(int(10), int(2)).statement(),
            BinOp::Add.of(string("ab"), string("cd")).statement(),
            BinOp::Add.of(ident("x"), float(2.0)).statement(),
            Statement::Assign {
                identifier: "z".to_string(),
                expr: Expr::Assign {
                    identifier: "y".to_string(),
                    expr: BinOp::Add.of(ident("y"), string("a")).b(),
                },
                typ: AssignType::CreateConst,
            },
            ident("z").statement(),
            BinOp::EQ.of(int(1), int(6)).statement(),
        ];
        let expected_evals: Vec<Option<Value>> = vec![
            Some(Value::Integer(2)),
            None,
            None,
            Some(Value::Integer(5)),
            Some(Value::String("abcd".to_string())),
            Some(Value::Float(Float(7.1))),
            None,
            Some(Value::String("za".to_string())),
            Some(Value::Bool(false)),
        ];

        assert_statements(code, &expected_nodes, &expected_evals)
    }

    #[test]
    fn test_compare() {
        let mut parser = Parser::from_str("!(5 * 4 >= 2 + 1 / 6)");

        let node = parser
            .parse_new_expr()
            .expect("expected node")
            .expect("expected no error");
        assert_eq!(
            node,
            UnaOp::Not.of(BinOp::GTEQ.of(
                BinOp::Mult.of(int(5), int(4)),
                BinOp::Add.of(int(2), BinOp::Div.of(int(1), int(6)))
            ))
        );

        let mut scope = Scope::new();
        assert_eq!(node.eval(&mut scope), Ok(Value::Bool(false)));

        assert_eq!(parser.parse_new_expr(), None);
        assert_eq!(parser.parse_new_expr(), None);
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
                expr: int(10),
                typ: AssignType::CreateConst,
            },
            Statement::Assign {
                identifier: "y".to_string(),
                expr: ident("x"),
                typ: AssignType::CreateConst,
            },
            Statement::Assign {
                identifier: "z".to_string(),
                expr: BinOp::Add.of(string("a"), string("b")),
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

        assert_eq!(parser.parse_new_expr(), None);
        assert_eq!(parser.parse_new_expr(), None);
    }

    #[test]
    fn test_arithmetic() {
        let mut parser = Parser::from_str("5 * 4 + 2");

        let node = parser
            .parse_new_expr()
            .expect("expected node")
            .expect("expected no error");
        assert_eq!(node, BinOp::Add.of(BinOp::Mult.of(int(5), int(4)), int(2)));

        let mut scope = Scope::new();
        assert_eq!(node.eval(&mut scope), Ok(Value::Integer(22)));

        assert_eq!(parser.parse_new_expr(), None);
        assert_eq!(parser.parse_new_expr(), None);
    }
}
// TODO a*b = 5 should fail
