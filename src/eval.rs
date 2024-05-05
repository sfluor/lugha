use wasm_bindgen::JsValue;

use crate::lexer::Float;
use crate::parser::*;
use std::borrow::BorrowMut;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::io::Cursor;
use std::ops::Deref;
use std::rc::Rc;
use std::string::FromUtf8Error;

const NEWLINE_VEC: [u8; 1] = [b'\n'];

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    BinTypeMismatch(Value, Value),
    UnimplementedBinaryOperator(Value, BinOp),
    UnimplementedUnaryOperator(Value, UnaOp),
    UnknownVariableReference(String),
    OverridingConstant(String, Value),
    WriteError,
    MissingReturnInFunction(Rc<FuncSignature>),
    UnknownFuncReference(String),
    StackOverflowError,
    UnknownIdentifier(Ident),
    Parsing(ParserError),
    InvalidNonUTF8Output(FromUtf8Error),
}

struct Variable {
    val: Value,
    constant: bool,
}

pub struct Scope {
    writer: Rc<RefCell<dyn std::io::Write>>,
    stack: Vec<Value>,
    stack_offsets: Vec<usize>,
}

impl Scope {
    fn new_stdout() -> Scope {
        Self::new(Rc::new(RefCell::new(std::io::stdout())))
    }

    pub fn new_cursor() -> (Scope, Rc<RefCell<Cursor<Vec<u8>>>>) {
        let cursor = Cursor::new(Vec::new());
        let cell = Rc::new(RefCell::new(cursor));
        let cloned = Rc::clone(&cell);
        (Self::new(cell), cloned)
    }

    fn new(writer: Rc<RefCell<dyn std::io::Write>>) -> Scope {
        Scope {
            writer,
            stack: Vec::new(),
            stack_offsets: Vec::new(),
        }
    }
    // TODO: don't use hash map to index variables
    fn get(&self, ident: &Ident) -> Option<&Value> {
        let offset = if ident.param {
            self.stack_offsets.last().unwrap_or(&(0 as usize))
        } else {
            &0
        };
        self.stack.get(ident.stack_idx + offset)
    }

    fn replace(&mut self, ident: &Ident, val: Value) -> Result<(), EvalError> {
        let offset = if ident.param {
            self.stack_offsets.last().unwrap_or(&(0 as usize))
        } else {
            &0
        };
        *self
            .stack
            .get_mut(ident.stack_idx + offset)
            .ok_or_else(|| EvalError::UnknownIdentifier(ident.clone()))? = val;
        Ok(())
    }

    fn eval_func(&mut self, identifier: &Ident, args: &Vec<Expr>) -> Result<Value, EvalError> {
        let func = self.get(identifier);
        if func.is_none() {
            return Err(EvalError::UnknownFuncReference(identifier.name.clone()));
        };

        let func = func.unwrap();
        let Value::Function {
            ref signature,
            ref body,
        } = *func
        else {
            // We type checked before already;
            unreachable!();
        };
        let signature = Rc::clone(signature);
        let body = Rc::clone(body);

        const MAX_STACK_SIZE: usize = 19;
        if self.stack_offsets.len() > MAX_STACK_SIZE {
            return Err(EvalError::StackOverflowError);
        }
        for arg in args {
            let val = arg.eval(self)?;
            self.push(val);
        }

        self.stack_offsets.push(self.stack.len() - args.len());

        for stmt in body.iter() {
            if let StatementValue::Return(r) = stmt.eval(self)? {
                self.end_func();
                return Ok(r);
            }
        }
        // TODO: validate at parse time
        Err(EvalError::MissingReturnInFunction(Rc::clone(&signature)))
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn end_func(&mut self) {
        let start = self.stack_offsets.pop().expect("non empty stack offsets");
        while self.stack.len() > start {
            self.stack
                .pop()
                .expect("empty stack when popping at the end of function");
        }
    }
}

impl Value {
    pub fn unary(self, op: &UnaOp) -> Result<Value, EvalError> {
        match op {
            UnaOp::Neg => match self {
                Value::Float(Float(f)) => Ok(Value::Float(Float(-f))),
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::String(_)
                | Value::Bool(_)
                | Value::Function {
                    signature: _,
                    body: _,
                } => Err(EvalError::UnimplementedUnaryOperator(self, op.clone())),
            },
            UnaOp::Not => match self {
                Value::String(_)
                | Value::Float(_)
                | Value::Integer(_)
                | Value::Function {
                    signature: _,
                    body: _,
                } => Err(EvalError::UnimplementedUnaryOperator(self, op.clone())),
                Value::Bool(v) => Ok(Value::Bool(!v)),
            },
        }
    }

    pub fn bin(self, op: &BinOp, other: &Value) -> Result<Value, EvalError> {
        match self {
            Value::Function {
                signature: _,
                body: _,
            } => Err(EvalError::UnimplementedBinaryOperator(self, op.clone())),

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
                    BinOp::Sub | BinOp::Mult | BinOp::Div | BinOp::And | BinOp::Or => {
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
                    BinOp::And | BinOp::Or => {
                        return Err(EvalError::UnimplementedBinaryOperator(self, op.clone()));
                    }
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
                    BinOp::And | BinOp::Or => {
                        return Err(EvalError::UnimplementedBinaryOperator(self, op.clone()));
                    }
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
                    BinOp::Mult | BinOp::And => Value::Bool(left && *right),
                    BinOp::Add | BinOp::Or => Value::Bool(left || *right),
                    BinOp::Div | BinOp::Sub => {
                        return Err(EvalError::UnimplementedBinaryOperator(self, op.clone()));
                    }
                });
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementValue {
    Break,
    Return(Value),
    Empty,
}

impl Statement {
    pub fn eval(&self, scope: &mut Scope) -> Result<StatementValue, EvalError> {
        match self {
            Statement::Expr(expr) => expr.eval(scope).map(|_| StatementValue::Empty),
            Statement::Create { name, expr, typ } => {
                let val = expr.eval(scope)?;
                scope.push(val);
                Ok(StatementValue::Empty)
            }
            Statement::Conditional { branches, default } => {
                for (expr, branch) in branches {
                    if let Value::Bool(true) = expr.eval(scope)? {
                        for s in branch {
                            // We must propagate up returns even inside conditions.
                            let sv = s.eval(scope)?;
                            match sv {
                                StatementValue::Break | StatementValue::Return(_) => return Ok(sv),
                                StatementValue::Empty => (),
                            }
                        }
                        return Ok(StatementValue::Empty);
                    };
                }

                for s in default {
                    // We must propagate up returns even inside conditions.
                    let sv = s.eval(scope)?;
                    match sv {
                        StatementValue::Break | StatementValue::Return(_) => return Ok(sv),
                        StatementValue::Empty => (),
                    }
                }

                Ok(StatementValue::Empty)
            }
            Statement::Print { expr, newline } => {
                let val = expr.eval(scope)?;

                let res = val
                    .write(Rc::clone(&scope.writer))
                    .map(|_| StatementValue::Empty)
                    .map_err(|_| EvalError::WriteError)?;

                if *newline {
                    (*scope.writer)
                        .borrow_mut()
                        .write(&NEWLINE_VEC)
                        .map(|_| StatementValue::Empty)
                        .map_err(|_| EvalError::WriteError)?;
                }

                Ok(res)
            }
            Statement::While { cond, statements } => loop {
                let cond_val = cond.eval(scope)?;
                if cond_val != Value::Bool(true) {
                    return Ok(StatementValue::Empty);
                }

                for s in statements {
                    let sv = s.eval(scope)?;
                    match sv {
                        StatementValue::Break | StatementValue::Return(_) => return Ok(sv),
                        StatementValue::Empty => (),
                    }
                }
            },
            Statement::Break => Ok(StatementValue::Break),
            Statement::Return(expr) => Ok(StatementValue::Return(expr.eval(scope)?)),
        }
    }
}

impl Expr {
    pub fn eval(&self, scope: &mut Scope) -> Result<Value, EvalError> {
        match self {
            // TODO: fix a || b shouldn't evaluate b
            Expr::Binary(left, op, right) => left.eval(scope)?.bin(&op, &right.eval(scope)?),
            Expr::Unary(op, node) => node.eval(scope)?.unary(&op),
            Expr::Literal(value) => Ok(value.clone()),
            Expr::FuncCall { identifier, args } => scope.eval_func(identifier, args),
            Expr::Assign { identifier, expr } => {
                let val = expr.eval(scope)?;
                // TODO: don't clone
                scope.replace(identifier, val.clone())?;
                Ok(val)
            }
            // TODO: don't clone
            Expr::Identifier(ident) => match scope.get(ident) {
                Some(v) => Ok(v.clone()),
                None => Err(EvalError::UnknownIdentifier(ident.clone())),
            },
        }
    }
}

enum ExecError {
    Parsing(ParserError),
    Eval(EvalError),
}

pub fn parse(code: &str) -> Result<Vec<Statement>, EvalError> {
    let parser = Parser::from_str(code);

    let mut statements = Vec::new();
    for res in parser {
        let statement = res.map_err(|err| EvalError::Parsing(err))?;
        statements.push(statement);
    }

    Ok(statements)
}

pub fn exec(code: &str) -> Result<String, EvalError> {
    let (mut scope, cursor) = Scope::new_cursor();
    for statement in parse(code)? {
        statement.eval(&mut scope)?;
    }
    let raw = cursor.as_ref().borrow().get_ref().clone();
    let encoded = String::from_utf8(raw);

    encoded.map_err(|err| EvalError::InvalidNonUTF8Output(err))
}

pub fn exec_and_get_stdout(code: &str) -> String {
    exec(code).expect("No failure")
}

mod test {
    use std::borrow::Borrow;

    use super::*;

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

            let mut scope = Scope::new_stdout();
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

            let mut scope = Scope::new_stdout();
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

        let mut scope = Scope::new_stdout();
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
            Statement::Create {
                name: "x".to_string(),
                expr: float(4.1),
                typ: AssignType::CreateVar,
            },
            Statement::Conditional {
                branches: vec![
                    (
                        BinOp::GT.of(local("x", 0), float(1.0)),
                        vec![
                            Expr::Assign {
                                identifier: i_local("x", 0),
                                expr: BinOp::Add.of(local("x", 0), float(0.1)).b(),
                            }
                            .statement(),
                            Expr::Assign {
                                identifier: i_local("x", 0),
                                expr: BinOp::Mult.of(float(2.0), local("x", 0)).b(),
                            }
                            .statement(),
                        ],
                    ),
                    (
                        boolean(false),
                        vec![Expr::Assign {
                            identifier: i_local("x", 0),
                            expr: float(4.2).b(),
                        }
                        .statement()],
                    ),
                ],
                default: vec![Statement::Conditional {
                    branches: vec![(
                        BinOp::LTEQ.of(local("x", 0), float(10.0)),
                        vec![Expr::Assign {
                            identifier: i_local("x", 0),
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
            print z;
            println z;
            1 == 6;"#;
        let expected_nodes: Vec<Statement> = vec![
            BinOp::Add
                .of(int(5), BinOp::Mult.of(int(3), UnaOp::Neg.of(int(1))))
                .statement(),
            Statement::Create {
                name: "x".to_string(),
                expr: float(5.1),
                typ: AssignType::CreateConst,
            },
            Statement::Create {
                name: "y".to_string(),
                expr: string("z"),
                typ: AssignType::CreateVar,
            },
            BinOp::Div.of(int(10), int(2)).statement(),
            BinOp::Add.of(string("ab"), string("cd")).statement(),
            BinOp::Add.of(local("x", 0), float(2.0)).statement(),
            Statement::Create {
                name: "z".to_string(),
                expr: Expr::Assign {
                    identifier: i_local("y", 1),
                    expr: BinOp::Add.of(local("y", 1), string("a")).b(),
                },
                typ: AssignType::CreateConst,
            },
            local("z", 2).statement(),
            Statement::Print {
                expr: local("z", 2),
                newline: false,
            },
            Statement::Print {
                expr: local("z", 2),
                newline: true,
            },
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
            None,
            None,
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

        let mut scope = Scope::new_stdout();
        assert_eq!(node.eval(&mut scope), Ok(Value::Bool(false)));

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

        let mut scope = Scope::new_stdout();
        assert_eq!(node.eval(&mut scope), Ok(Value::Integer(22)));

        assert_eq!(parser.parse_new_expr(), None);
        assert_eq!(parser.parse_new_expr(), None);
    }

    #[test]
    fn test_with_print() {
        for (code, expect) in vec![
            (
                "const fibo = func(n int) -> int {
                                if n == 0 {
                                    return 0;
                                } elif n == 1 {
                                    return 1;
                                } else {
                                    return fibo(n-1) + fibo(n-2);
                                }
                            };
                            println fibo(10);
                            print fibo(20);",
                "55
6765",
            ),
            ("print 5 * 4;", "20"),
            ("print true + false;", "true"),
            ("print true * false;", "false"),
            (
                "
                const x = 10 + 2;
                print x-1;",
                "11",
            ),
            (
                "const fact = func(n int) -> int {
                                if n < 2 {
                                    return 1;
                                }

                                return n * fact(n-1);
                            };
                            print fact(10);",
                "3628800",
            ),
            (
                r#"
                var x = 0;
                println x;
                print "-";
                println "";
                while x < 100 {
                    x = x + 1;
                }
                println x;
                print "-";
                println "";
                while x < 500 {
                    x = x + 7;
                    if x > 150 {
                        println "breaking";
                        break;
                    }
                }
                print x;"#,
                "0
-
100
-
breaking
156",
            ),
            (
                r#"
                            var x = "abc" + "d";
                            x = x + "e";
                            print x + "z";
                            print 55.123;
                            print x = x + "j";
                            "#,
                "abcdez55.123abcdej",
            ),
        ] {
            assert_eq!(exec_and_get_stdout(code), expect);
        }
    }
}

// TODO test variable shadowing (reassigning same variable) + test it across different scopes
// TODO a*b = 5 should fail
// TODO disallow return inside loops
// TODO disallow breaks outside loops
