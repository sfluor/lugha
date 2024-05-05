# lugha

A toy interpreter for a C like language to learn more about rust.

Playground available here: https://sfluor.github.io/lugha/

## Dev commands

Rebuilding the wasm modules with `wasm-pack build`

Run the server with `npm run start`, if you encounter a SSL error you might need to do `NODE_OPTIONS=--openssl-legacy-provider npm run start` instead.

Profiling with `cargo flamegraph --bench eval -- --bench`

Running test with `cargo test` (or `cargo watch -q -x test -c`)
