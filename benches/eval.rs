use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lugha::eval::Scope;
use lugha::parser::{Parser, Statement};
use pprof::criterion::{Output, PProfProfiler};

fn fibonacci(n: i32) {
    let code = "const fibo = func(n int) -> int {
                    if n == 0 {
                        return 0;
                    } elif n == 1 {
                        return 1;
                    } else {
                        return fibo(n-1) + fibo(n-2);
                    }
                };
                print fibo("
        .to_owned()
        + n.to_string().as_str()
        + ");";
    let statements: Vec<Statement> = Parser::from_str(code.as_str())
        .map(|r| r.expect("statement"))
        .collect();

    // Run it multiple times to amortize the parsing
    for _ in 1..10 {
        let (mut scope, _) = Scope::new_cursor();

        for s in &statements {
            s.eval(&mut scope).expect("no error");
        }
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = criterion_benchmark
}
criterion_main!(benches);
