use crate::lexer::{Float, KeywordType, Lexer, LexerError, SymbolType, Token, TokenType};
use std::{
    borrow::BorrowMut,
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
pub enum Value {
    String(String),
    Float(Float),
    Integer(i64),
    Bool(bool),
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
        }
    }
}

pub fn ident(name: &str) -> Expr {
    Expr::Identifier(name.to_owned())
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
    Assign { identifier: String, expr: BExpr },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignType {
    CreateConst,
    CreateVar,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
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
    Print(Expr),
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
            Statement::Print(expr) => expr.type_check(scope),
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
            }),
            Expr::Identifier(ident) => scope.get(ident),
            Expr::Assign { identifier, expr } => {
                scope.can_be_assigned(identifier, expr)?;
                expr.type_check(scope)
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
        expected: TokenType,
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

pub struct Parser<'a> {
    error: bool,
    lexer: Peekable<Lexer>,
    scope: ParserScope<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser<'a> {
        Parser {
            error: false,
            lexer: lexer.peekable(),
            scope: ParserScope::new(),
        }
    }

    pub fn from_str(str: &'a str) -> Parser {
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
                expected,
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

    fn parse_print(&mut self) -> Option<Result<Statement, ParserError>> {
        let token = self.consume(TokenType::Keyword(KeywordType::Print));
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

        Some(Ok(Statement::Print(expr)))
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
            Some(Err(err)) => return Some(Err(ParserError::Lexing(err.clone()))),
            None => return None,
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Const) => {
                self.parse_declaration(true)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Var) => {
                self.parse_declaration(false)
            }
            Some(Ok(tok)) if tok.typ == TokenType::Keyword(KeywordType::Print) => {
                self.parse_print()
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
}
