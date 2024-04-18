use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::io::Cursor;
use std::rc::Rc;

use crate::lexer::Float;
use crate::parser::*;

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
}

struct Variable {
    val: Value,
    constant: bool,
}

pub struct Scope<'a> {
    writer: Rc<RefCell<dyn std::io::Write>>,
    parent: Option<Box<&'a Scope<'a>>>,
    locals: HashMap<String, Value>,
    constants: HashSet<String>,
}

impl<'a> Scope<'a> {
    fn new_stdout() -> Scope<'a> {
        Self::new(Rc::new(RefCell::new(std::io::stdout())))
    }

    pub fn new_cursor() -> (Scope<'a>, Rc<RefCell<Cursor<Vec<u8>>>>) {
        let mut cursor = Cursor::new(Vec::new());
        let cell = Rc::new(RefCell::new(cursor));
        let cloned = Rc::clone(&cell);
        (Self::new(cell), cloned)
    }

    fn new(writer: Rc<RefCell<dyn std::io::Write>>) -> Scope<'a> {
        Scope {
            writer,
            parent: None,
            locals: HashMap::new(),
            constants: HashSet::new(),
        }
    }

    fn child(&self) -> Scope {
        let mut ch = Scope::new(Rc::clone(&self.writer));
        ch.parent = Some(Box::new(self));

        ch
    }

    fn set(&mut self, name: &str, val: Value, constant: bool) -> Result<(), EvalError> {
        if self.constants.contains(name) {
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
            Statement::Assign {
                identifier,
                expr,
                typ,
            } => {
                let val = expr.eval(scope)?;
                scope
                    .set(identifier, val, *typ == AssignType::CreateConst)
                    .map(|_| StatementValue::Empty)
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
            Statement::While { cond, statements } => 'outer: loop {
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
            Expr::Identifier(ident) => match scope.get(&ident) {
                // TODO: don't clone
                Some(val) => Ok(val.clone()),
                None => Err(EvalError::UnknownVariableReference(ident.to_string())),
            },
            Expr::FuncCall { identifier, args } => {
                // TODO use a stack for arguments instead of referencing them via the scope
                let Value::Function { signature, body } = scope
                    .get(identifier)
                    .ok_or_else(|| EvalError::UnknownFuncReference(identifier.clone()))?
                else {
                    // We type-checked before
                    unreachable!();
                };

                let mut scope = scope.child();

                for (arg, (name, _)) in args.iter().zip(signature.args.iter()) {
                    let val = arg.eval(&mut scope)?;
                    scope.set(name, val, false)?;
                }

                for stmt in body {
                    if let StatementValue::Return(r) = stmt.eval(&mut scope)? {
                        return Ok(r);
                    }
                }

                // TODO: validate at parse time
                Err(EvalError::MissingReturnInFunction(Rc::clone(signature)))
            }
            Expr::Assign { identifier, expr } => {
                let val = expr.eval(scope)?;
                // TODO: no clone
                scope.set(identifier, val.clone(), false)?;
                Ok(val)
            }
        }
    }
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
            print z;
            println z;
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
            Statement::Print {
                expr: ident("z"),
                newline: false,
            },
            Statement::Print {
                expr: ident("z"),
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

    fn exec_and_get_stdout(code: &str) -> String {
        let mut parser = Parser::from_str(code);
        let (mut scope, cursor) = Scope::new_cursor();

        for res in parser {
            let statement = res.expect("Expected no error while parsing");
            statement
                .eval(&mut scope)
                .expect("Expected no error while evaluating");
        }
        let raw = cursor.as_ref().borrow().get_ref().clone();
        let encoded = String::from_utf8(raw);

        encoded.expect("Expected valid UTF8")
    }

    #[test]
    fn test_with_print() {
        for (code, expect) in vec![
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

// TODO a*b = 5 should fail
// TODO disallow return inside loops
// TODO disallow breaks outside loops
