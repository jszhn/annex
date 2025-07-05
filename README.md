# Annex

*Annex* is a hobbyist imperative, compiled programming language.
It will compile to RISC-V assembly and is built in Rust. It aims
to implement the standard compiler workflow from scratch with minimal
dependencies, including lexical analysis, parsing, abstract syntax tree
generation, semantic analysis, and code generation.

Its syntax is roughly inspired by LLVM and Rust. A sample snippet:

```llvm
var i32[10] arr = 0;
var i32 x = 0;
const u32 addr = 1000009;
var i32[2] y = arr[3];

fn process_array{var i32[100] input} i32 {
    return input[50];
}
```

with the full language grammar specified at `annex.g4`.

## Getting started

The project primarily relies on standard Rust tooling to build, run, and test the code.

To build the project:

```bash
cargo build --release
```

To run tests:

```bash
cargo test
```

The CI pipeline also has linting and formatting checks:

```bash
cargo clippy -- -D warnings
cargo fmt
```