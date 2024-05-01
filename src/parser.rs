use crate::lexer::{self, Float, KeywordType, Lexer, LexerError, SymbolType, Token, TokenType};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{write, Debug, Display},
    iter::Peekable,
    rc::Rc,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Or,
    And,
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Or => "Or",
                BinOp::And => "And",
                BinOp::Add => "Add",
                BinOp::Sub => "Sub",
                BinOp::Mult => "Mult",
                BinOp::Div => "Div",
                BinOp::GT => "GT",
                BinOp::LT => "LT",
                BinOp::GTEQ => "GTEQ",
                BinOp::LTEQ => "LTEQ",
                BinOp::EQ => "EQ",
                BinOp::NEQ => "NEQ",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncSignature {
    pub args: Vec<(String, Type)>,
    ret_type: Type,
}

impl Display for FuncSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function({:?}) -> {}", self.args, self.ret_type)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    String(String),
    Float(Float),
    Integer(i64),
    Bool(bool),
    Function {
        signature: Rc<FuncSignature>,
        body: Vec<Statement>,
    },
}

impl Value {
    pub fn ex(self) -> Expr {
        Expr::Literal(self)
    }

    pub fn write(&self, writer: Rc<RefCell<dyn std::io::Write>>) -> Result<(), std::io::Error> {
        let mut writer = (*writer).borrow_mut();
        match self {
            Value::String(s) => writer.write(s.as_bytes()).map(|_| ()),
            Value::Float(Float(f)) => write!(writer, "{f}"),
            Value::Integer(i) => write!(writer, "{i}"),
            Value::Bool(b) => write!(writer, "{b}"),
            Value::Function { signature, body } => write!(writer, "{signature}"),
        }
    }
}

pub fn ident(name: &str) -> Expr {
    Expr::Identifier(name.to_owned())
}

pub fn param(i: usize) -> Expr {
    Expr::Param(i)
}

pub fn boolean(b: bool) -> Expr {
    Value::Bool(b).ex()
}

pub fn int(x: i64) -> Expr {
    Value::Integer(x).ex()
}

pub fn string(s: &str) -> Expr {
    Value::String(s.to_string()).ex()
}

pub fn float(f: f64) -> Expr {
    Value::Float(Float(f)).ex()
}

impl BinOp {
    pub fn of(self, left: Expr, right: Expr) -> Expr {
        Expr::Binary(left.b(), self, right.b())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaOp {
    Neg,
    Not,
}

impl Display for UnaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaOp::Neg => "neg",
                UnaOp::Not => "not",
            }
        )
    }
}

impl UnaOp {
    pub fn of(self, node: Expr) -> Expr {
        Expr::Unary(self, node.b())
    }
}

type BExpr = Box<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Binary(BExpr, BinOp, BExpr),
    Unary(UnaOp, BExpr),
    Literal(Value),
    Identifier(String),
    Param(usize),
    Assign { identifier: String, expr: BExpr },
    FuncCall { identifier: String, args: Vec<Expr> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignType {
    CreateConst,
    CreateVar,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
    While {
        cond: Expr,
        statements: Vec<Statement>,
    },
    Break,
    Return(Expr),
    Assign {
        identifier: String,
        expr: Expr,
        typ: AssignType,
    },
    Conditional {
        branches: Vec<(Expr, Vec<Statement>)>,
        default: Vec<Statement>,
    },
    Print {
        expr: Expr,
        newline: bool,
    },
}

impl Statement {
    fn type_check(&self, scope: &mut ParserScope) -> Result<Type, ParserError> {
        match self {
            Statement::Expr(expr) => expr.type_check(scope),
            // TODO: also check that we don't override constants
            Statement::Assign {
                identifier,
                expr,
                typ: assign_type,
            } => {
                // We do this here to handle recursion, otherwise we wouldn't be able to know the
                // return type until we type check but upon typechecking we would consider the
                // recursive call as unknown
                let mut marked = false;
                let constant = *assign_type == AssignType::CreateConst;

                if let Expr::Literal(Value::Function { signature, body }) = expr {
                    scope.mark(
                        identifier.clone(),
                        Type::Function(Rc::clone(signature)),
                        constant,
                    )?;
                    marked = true;
                };

                let typ = expr.type_check(scope)?;

                if !marked {
                    scope.mark(identifier.clone(), typ.clone(), constant)?;
                }
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
            Statement::Print { expr, newline } => expr.type_check(scope),
            Statement::While { cond, statements } => {
                cond.type_check(scope)?;
                for s in statements {
                    s.type_check(scope)?;
                }
                Ok(Type::Statement)
            }
            Statement::Break => Ok(Type::Statement),
            Statement::Return(expr) => {
                let Some(_) = scope.return_type.last() else {
                    return Err(ParserError::InvalidReturnOutsideFunction(expr.clone()));
                };

                let typ = expr.type_check(scope)?;
                let expected = scope.return_type.last().unwrap();
                if *expected != typ {
                    return Err(ParserError::InvalidReturnType {
                        expected: expected.clone(),
                        actual: typ,
                    });
                }

                Ok(Type::Statement)
            }
        }
    }
}

impl Expr {
    pub fn b(self) -> BExpr {
        Box::new(self)
    }

    pub fn statement(self) -> Statement {
        Statement::Expr(self)
    }

    fn type_check(&self, scope: &mut ParserScope) -> Result<Type, ParserError> {
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

                let ops = op.to_string();

                // We only check left since we already checked that left and right were equal
                match op {
                    BinOp::Or | BinOp::And => is_type(&ops, left, ltype, &vec![Type::Boolean]),
                    BinOp::Add => {
                        // TODO Avoid computing the type multiple times.
                        is_type(
                            &ops,
                            left,
                            ltype,
                            &vec![Type::Boolean, Type::Integer, Type::Float, Type::String],
                        )
                    }
                    BinOp::Mult => is_type(
                        &ops,
                        left,
                        ltype,
                        &vec![Type::Integer, Type::Float, Type::Boolean],
                    ),
                    BinOp::Sub | BinOp::Mult | BinOp::Div => {
                        is_type(&ops, left, ltype, &vec![Type::Integer, Type::Float])
                    }
                    BinOp::GT | BinOp::LT | BinOp::GTEQ | BinOp::LTEQ | BinOp::EQ | BinOp::NEQ => {
                        // As long as the two types are equal we're good.
                        Ok(Type::Boolean)
                    }
                }
            }
            Expr::Unary(op, expr) => {
                let typ = expr.type_check(scope)?;
                let ops = op.to_string();
                match op {
                    UnaOp::Neg => is_type(&ops, expr, typ, &vec![Type::Integer, Type::Float]),
                    UnaOp::Not => is_type(&ops, expr, typ, &vec![Type::Boolean]),
                }
            }
            Expr::Literal(l) => Ok(match l {
                Value::String(_) => Type::String,
                Value::Float(_) => Type::Float,
                Value::Integer(_) => Type::Integer,
                Value::Bool(_) => Type::Boolean,
                Value::Function { signature, body } => {
                    scope.start_func(signature);
                    for s in body {
                        s.type_check(scope)?;
                    }
                    scope.end_func(signature);

                    Type::Function(Rc::clone(signature))
                }
            }),
            Expr::Identifier(ident) => scope.get(ident).cloned(),
            Expr::Assign { identifier, expr } => {
                scope.can_be_assigned(identifier, expr)?;
                expr.type_check(scope)
            }
            Expr::FuncCall { identifier, args } => {
                let typ = scope.get(identifier)?.clone();
                let Type::Function(signature) = typ else {
                    return Err(ParserError::InvalidFunc(identifier.clone(), typ.clone()));
                };

                if signature.args.len() != args.len() {
                    return Err(ParserError::InvalidFuncArgsNumber {
                        func: identifier.clone(),
                        expected: Rc::clone(&signature),
                        got: args.clone(),
                    });
                }

                for (idx, (arg, (name, expected))) in
                    args.iter().zip(signature.args.iter()).enumerate()
                {
                    let argtype = arg.type_check(scope)?;
                    if argtype != *expected {
                        return Err(ParserError::InvalidArgType {
                            func: identifier.clone(),
                            argidx: idx,
                            argname: name.clone(),
                            expected: expected.clone(),
                            got: argtype.clone(),
                            arg: arg.clone(),
                        });
                    }
                }

                Ok(signature.ret_type.clone())
            }
            Expr::Param(idx) => match scope.param_types.get(*idx) {
                Some(v) => Ok(v.clone()),
                None => Err(ParserError::UnknownParamReference(*idx)),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    String,
    Float,
    Integer,
    Boolean,
    // Used for statement blocks that do not return anything.
    Statement,
    Function(Rc<FuncSignature>),
    // TODO: functions and custom types
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::String => "string",
                Type::Float => "float",
                Type::Integer => "int",
                Type::Boolean => "bool",
                Type::Statement => "statement",
                Type::Function(signature) =>
                    return write!(f, "{}", signature.to_string().as_str()),
            }
        )
    }
}

fn is_type(op: &str, expr: &Expr, actual: Type, expected: &[Type]) -> Result<Type, ParserError> {
    if !expected.contains(&actual) {
        Err(ParserError::TypesError {
            op: op.to_owned(),
            expected: Vec::from(expected),
            actual,
            expr: expr.clone(),
        })
    } else {
        Ok(actual)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    Lexing(LexerError),
    ConsumeError {
        actual: Option<Token>,
        detail: String,
    },
    TypesError {
        op: String,
        expected: Vec<Type>,
        actual: Type,
        expr: Expr,
    },
    BinTypeError {
        left: Type,
        right: Type,
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
    UnterminatedPrint(Token),
    ParseTypeError(Token),
    EmptyTypeDecl,
    UnterminatedFuncDecl,
    InvalidTypeDecl(Box<ParserError>),
    EmptyReturnValue,
    InvalidReturnOutsideFunction(Expr),
    InvalidReturnType {
        expected: Type,
        actual: Type,
    },
    InvalidFuncCall(Expr),
    InvalidFunc(String, Type),
    InvalidFuncArgsNumber {
        func: String,
        expected: Rc<FuncSignature>,
        got: Vec<Expr>,
    },
    InvalidArgType {
        func: String,
        argidx: usize,
        expected: Type,
        got: Type,
        arg: Expr,
        argname: String,
    },
    UnknownParamReference(usize),
}

#[derive(Debug)]
struct ParserScope {
    var_types: HashMap<String, Type>,
    constants: HashSet<String>,
    // vec to allow shadowing variable references
    params: HashMap<String, Vec<usize>>,
    param_types: Vec<Type>,
    // The expected return types of the current function if we are in a function.
    return_type: Vec<Type>,
}

impl ParserScope {
    fn new() -> ParserScope {
        ParserScope {
            var_types: HashMap::new(),
            constants: HashSet::new(),
            params: HashMap::new(),
            param_types: Vec::new(),
            return_type: Vec::new(),
        }
    }

    fn start_func(&mut self, signature: &FuncSignature) {
        // TODO no clone
        self.return_type.push(signature.ret_type.clone());
        for (name, typ) in signature.args.iter() {
            self.params
                .entry(name.clone())
                .or_insert_with(|| Vec::new())
                .push(self.param_types.len());
            self.param_types.push(typ.clone());
        }
    }

    fn end_func(&mut self, signature: &FuncSignature) {
        for (name, _) in signature.args.iter() {
            self.params
                .get_mut(name)
                .expect("param name shouldn't be empty")
                .pop();
            // TODO we could check that the type matches here
            self.param_types.pop();
        }
        // TODO we could check that the type matches here
        self.return_type.pop();
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
    fn get(&self, name: &str) -> Result<&Type, ParserError> {
        if let Some(val) = self.var_types.get(name) {
            return Ok(val);
        };

        if let Some(idxs) = self.params.get(name) {
            return idxs
                .last()
                .ok_or(ParserError::UnknownVariable(name.to_string()))
                .map(|x| &self.param_types[*x]);
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

        Ok(())
    }
}

impl Iterator for Parser {
    type Item = Result<Statement, ParserError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.error {
            None
        } else {
            match self.parse_statement() {
                None => None,
                Some(Err(err)) => {
                    self.error = true;
                    Some(Err(err))
                }
                Some(Ok(stmt)) => {
                    if let Err(err) = stmt.type_check(&mut self.scope) {
                        return Some(Err(err));
                    }
                    Some(Ok(stmt))
                }
            }
        }
    }
}

pub struct Parser {
    error: bool,
    lexer: Peekable<Lexer>,
    scope: ParserScope,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            error: false,
            lexer: lexer.peekable(),
            scope: ParserScope::new(),
        }
    }

    pub fn from_str(str: &str) -> Parser {
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

    fn expect<F>(&mut self, cond: F, detail: String) -> Result<Token, ParserError>
    where
        F: FnOnce(&TokenType) -> bool,
    {
        let Some(res) = self.lexer.next() else {
            return Err(ParserError::ConsumeError {
                actual: None,
                detail,
            });
        };

        let Ok(token) = res else {
            return Err(ParserError::Lexing(res.unwrap_err()));
        };

        if !cond(&token.typ) {
            Err(ParserError::ConsumeError {
                actual: Some(token),
                detail,
            })
        } else {
            Ok(token)
        }
    }

    fn consume(&mut self, expected: TokenType) -> Result<Token, ParserError> {
        self.expect(
            |typ| *typ == expected,
            format!("Expected token type: {expected:?}"),
        )
    }

    fn parse_return(&mut self) -> Option<Result<Statement, ParserError>> {
        let token = self.consume(TokenType::Keyword(KeywordType::Return));
        if let Err(err) = token {
            return Some(Err(err));
        }

        // TODO handle empty returns
        let expr = self.parse_new_expr();
        match expr {
            None => Some(Err(ParserError::EmptyReturnValue)),
            Some(Err(err)) => Some(Err(err)),
            Some(Ok(expr)) => Some(Ok(Statement::Return(expr))),
        }
    }

    fn parse_break(&mut self) -> Option<Result<Statement, ParserError>> {
        let token = self.consume(TokenType::Keyword(KeywordType::Break));
        if let Err(err) = token {
            return Some(Err(err));
        }
        Some(Ok(Statement::Break))
    }

    fn parse_print(&mut self, newline: bool) -> Option<Result<Statement, ParserError>> {
        let token = self.consume(TokenType::Keyword(if newline {
            KeywordType::Println
        } else {
            KeywordType::Print
        }));
        if let Err(err) = token {
            return Some(Err(err));
        }
        let token = token.unwrap();

        let expr = match self.parse_new_expr() {
            Some(res) => match res {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err)),
            },
            None => {
                return Some(Err(ParserError::UnterminatedPrint(token)));
            }
        };

        Some(Ok(Statement::Print { expr, newline }))
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
                            detail: "expected identifier name".to_owned(),
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
            Some(Err(err)) => return Some(Err(ParserError::Lexing(err.clone()))),
            None => return None,
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Const) => {
                self.parse_declaration(true)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Var) => {
                self.parse_declaration(false)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Return) => {
                self.parse_return()
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Break) => {
                self.parse_break()
            }
            Some(Ok(tok))
                if tok.typ == TokenType::Keyword(KeywordType::Print)
                    || tok.typ == TokenType::Keyword(KeywordType::Println) =>
            {
                let newline = tok.typ == TokenType::Keyword(KeywordType::Println);
                self.parse_print(newline)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::While) => {
                expect_colon = false;
                self.parse_while()
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::If) => {
                expect_colon = false;
                self.parse_conditional()
            }
            _ => self.parse_new_expr().map(|r| r.map(Statement::Expr)),
        };

        if expect_colon {
            if let Err(err) = self.consume(TokenType::Symbol(SymbolType::SemiColon)) {
                return Some(Err(err));
            }
        }

        result
    }

    fn parse_func_call(&mut self, left: Expr) -> Result<Expr, ParserError> {
        // TODO: we could support inline function declarations and call
        // (func () -> int)()
        // by removing this identifier restriction
        let Expr::Identifier(name) = left else {
            return Err(ParserError::InvalidFuncCall(left));
        };

        let args: Vec<Expr> = self.lex_until(
            |token| token.typ == TokenType::Symbol(SymbolType::Rparen),
            |this, idx| {
                if idx > 0 {
                    if let Err(err) = this.consume(TokenType::Symbol(SymbolType::Comma)) {
                        return Some(Err(err));
                    }
                }
                this.parse_new_expr()
            },
        )?;

        self.consume(TokenType::Symbol(SymbolType::Rparen))?;

        // TODO resolve the func now
        Ok(Expr::FuncCall {
            identifier: name,
            args,
        })
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

            // Function calls don't "just" evaluate the next node so handle them separately.
            if symbol == SymbolType::Lparen {
                node = match self.parse_func_call(node) {
                    Ok(n) => n,
                    Err(err) => return Some(Err(err)),
                };
                continue;
            }

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
                SymbolType::Lparen
                | SymbolType::Arrow
                | SymbolType::Bang
                | SymbolType::Comma
                | SymbolType::Percentage
                | SymbolType::Colon
                | SymbolType::SemiColon
                | SymbolType::RBracket
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
        let Some(result) = self.parse_precedence(token.typ.precedence(true)) else {
            return Err(ParserError::UnterminatedGrouping(token.clone()));
        };

        if let Err(err) = self.consume(TokenType::Symbol(SymbolType::Rparen)) {
            return Err(err);
        };

        result
    }

    fn parse_type_decl(&mut self) -> Result<Type, ParserError> {
        let Some(res) = self.lexer.next() else {
            return Err(ParserError::EmptyTypeDecl);
        };

        let token = res.map_err(|err| ParserError::Lexing(err))?;
        Ok(match token.typ {
            TokenType::Keyword(KeywordType::Bool) => Type::Boolean,
            TokenType::Keyword(KeywordType::Int) => Type::Integer,
            TokenType::Keyword(KeywordType::String) => Type::String,
            TokenType::Keyword(KeywordType::Float) => Type::Float,
            TokenType::Keyword(KeywordType::Func) => {
                Type::Function(Rc::new(self.parse_func_signature()?))
            }
            _ => return Err(ParserError::ParseTypeError(token)),
        })
    }

    fn parse_func_signature(&mut self) -> Result<FuncSignature, ParserError> {
        self.consume(TokenType::Symbol(SymbolType::Lparen))?;

        let args: Vec<(String, Type)> = self.lex_until(
            |token| token.typ == TokenType::Symbol(SymbolType::Rparen),
            |this, idx| {
                idx;
                if idx > 0 {
                    if let Err(err) = this.consume(TokenType::Symbol(SymbolType::Comma)) {
                        return Some(Err(err));
                    }
                }
                // parse identifier and then parse type declaration
                let ident = this.expect(
                    |typ| matches!(typ, TokenType::Identifier(_)),
                    "expected argument name".to_owned(),
                );
                let name = match ident {
                    Err(err) => return Some(Err(err)),
                    Ok(Token {
                        typ: TokenType::Identifier(name),
                        ..
                    }) => name,
                    _ => unreachable!(),
                };

                let type_decl = this.parse_type_decl();
                if let Err(err) = type_decl {
                    return Some(Err(ParserError::InvalidTypeDecl(Box::new(err))));
                }

                Some(Ok((name, type_decl.expect("Error checked before"))))
            },
        )?;

        self.consume(TokenType::Symbol(SymbolType::Rparen))?;
        self.consume(TokenType::Symbol(SymbolType::Arrow))?;
        let ret_type = self.parse_type_decl()?;
        Ok(FuncSignature { args, ret_type })
    }

    fn parse_func(&mut self) -> Result<Expr, ParserError> {
        let signature = self.parse_func_signature()?;

        self.scope.start_func(&signature);
        let body = self.parse_block().unwrap_or_else(|| Ok(vec![]))?;
        self.scope.end_func(&signature);

        Ok(Expr::Literal(Value::Function {
            signature: Rc::new(signature),
            body,
        }))
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
                KeywordType::Func => self.parse_func(),
                KeywordType::False => Ok(Expr::Literal(Value::Bool(false))),
                KeywordType::True => Ok(Expr::Literal(Value::Bool(true))),
                _ => Err(ParserError::InvalidPrefix(token)),
            },
            TokenType::Identifier(identifier) => Ok(match self.scope.params.get(&identifier) {
                Some(idxs) => idxs
                    .last()
                    .map(|idx| Expr::Param(*idx))
                    .unwrap_or(Expr::Identifier(identifier)),
                None => Expr::Identifier(identifier),
            }),
        }
    }

    fn lex_until<C, F, T>(&mut self, cond: C, mut consumer: F) -> Result<Vec<T>, ParserError>
    where
        C: Fn(&Token) -> bool,
        F: FnMut(&mut Self, usize) -> Option<Result<T, ParserError>>,
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

                    if let Some(data) = consumer(self, acc.len()) {
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
            |this, _| this.parse_statement(),
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

    fn parse_while(&mut self) -> Option<Result<Statement, ParserError>> {
        let if_parse = self.parse_condition_branch(KeywordType::While)?;
        let Ok((cond, statements)) = if_parse else {
            return Some(Err(if_parse.unwrap_err()));
        };

        Some(Ok(Statement::While { cond, statements }))
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

    pub fn parse_new_expr(&mut self) -> Option<Result<Expr, ParserError>> {
        self.parse_precedence(0)
    }
}

mod test {
    use super::*;

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
    fn test_func_typechecks() {
        let table: Vec<(&str, ParserError)> = vec![
            (
                r#"const fx = func(a int) -> int {
                    return a + 1;
                };
                fx(1);
                fx(1.0);
                "#,
                ParserError::InvalidArgType {
                    func: "fx".to_owned(),
                    argidx: 0,
                    expected: Type::Integer,
                    got: Type::Float,
                    arg: float(1.0),
                    argname: "a".to_owned(),
                },
            ),
            (
                r#"const fx = func(a int) -> float {
                    return a + 1.0;
                };"#,
                ParserError::BinTypeError {
                    left: Type::Integer,
                    right: Type::Float,
                    expr: BinOp::Add.of(param(0), float(1.0)),
                },
            ),
            (
                r#"const fx = func() -> int {
                    return "1";
                };"#,
                ParserError::InvalidReturnType {
                    expected: Type::Integer,
                    actual: Type::String,
                },
            ),
        ];

        let mut found_err = false;
        for (inp, expected_err) in table {
            let mut parser = Parser::from_str(inp);
            while let Some(chunk) = parser.next() {
                match chunk {
                    Err(err) => {
                        assert_eq!(err, expected_err);
                        assert_eq!(parser.next(), None);
                        found_err = true;
                    }
                    _ => (),
                }
            }
            assert!(found_err, "Expected error in:\n{inp}");
        }
    }

    #[test]
    fn test_func() {
        let signature = Rc::new(FuncSignature {
            args: vec![
                ("x".to_owned(), Type::Integer),
                ("y".to_owned(), Type::String),
                ("b".to_owned(), Type::Float),
            ],
            ret_type: Type::Float,
        });

        let signature_empty = Rc::new(FuncSignature {
            args: vec![],
            ret_type: Type::Integer,
        });

        let signature_mono = Rc::new(FuncSignature {
            args: vec![("a".to_owned(), Type::Integer)],
            ret_type: Type::Integer,
        });

        let signature_of_func = Rc::new(FuncSignature {
            args: vec![
                ("a".to_owned(), Type::Integer),
                ("f".to_owned(), Type::Function(Rc::clone(&signature_mono))),
                ("b".to_owned(), Type::String),
            ],
            ret_type: Type::Function(Rc::clone(&signature_empty)),
        });
        let table: Vec<(&str, Vec<Statement>)> = vec![
            (
                r"const fx = func() -> int {
                    return 1;
                };",
                vec![Statement::Assign {
                    identifier: "fx".to_owned(),
                    expr: Expr::Literal(Value::Function {
                        signature: Rc::clone(&signature_empty),
                        body: vec![Statement::Return(int(1))],
                    }),
                    typ: AssignType::CreateConst,
                }],
            ),
            (
                r"const fx = func(a int, f func(a int) -> int, b string) -> func() -> int {
                    return func() -> int {
                        return a + 1;
                    };
                };",
                vec![Statement::Assign {
                    identifier: "fx".to_owned(),
                    expr: Expr::Literal(Value::Function {
                        signature: Rc::clone(&signature_of_func),
                        body: vec![Statement::Return(
                            Value::Function {
                                signature: Rc::clone(&signature_empty),
                                body: vec![Statement::Return(BinOp::Add.of(param(0), int(1)))],
                            }
                            .ex(),
                        )],
                    }),
                    typ: AssignType::CreateConst,
                }],
            ),
            (
                r#"const f = func(x int, y string, b float) -> float {
                if b > 1.0 {
                    return b;
                } else {
                    return b - 4.3;
                }
            };
            "#,
                vec![Statement::Assign {
                    identifier: "f".to_owned(),
                    expr: Expr::Literal(Value::Function {
                        signature: Rc::clone(&signature),
                        body: vec![Statement::Conditional {
                            branches: vec![(
                                BinOp::GT.of(param(2), float(1.0)),
                                vec![Statement::Return(param(2))],
                            )],
                            default: vec![Statement::Return(BinOp::Sub.of(param(2), float(4.3)))],
                        }],
                    }),
                    typ: AssignType::CreateConst,
                }],
            ),
        ];

        for (inp, expected_stmts) in table {
            let mut parser = Parser::from_str(inp);
            for expected in expected_stmts {
                let stmt = parser
                    .next()
                    .expect("expected statement")
                    .expect("expected no error");
                assert_eq!(
                    stmt, expected,
                    "Expected {:?} to be {:?} for input:\n{}",
                    stmt, expected, inp
                );
            }

            assert_eq!(parser.next(), None);
            assert_eq!(parser.next(), None);
        }
    }
}
// TODO test return;
