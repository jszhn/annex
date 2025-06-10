# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

This is a standard Rust project using Cargo. Key commands:

- `cargo build` - Build the compiler
- `cargo run <file.ax>` - Run the compiler on an Annex source file
- `cargo test` - Run all tests
- `cargo test <test_name>` - Run a specific test
- `cargo clippy -- -D warnings` - Lint with strict warnings
- `cargo fmt` - Format code

## Compiler Architecture

Annex implements a traditional compiler pipeline with clear module separation:

**Pipeline Flow**: Lexer → Parser → AST → Semantic Analysis → IR → Assembly Generation

### Key Modules and Data Flow:

1. **Lexer** (`src/lexer/`): Tokenizes source code into `Token` enum variants
2. **Parser** (`src/parse/`): Converts tokens to parse tree using recursive descent with Pratt parsing for expressions
3. **AST** (`src/ast/`): Transforms parse tree into typed abstract syntax tree with rich type information
4. **Semantic Analysis** (`src/sem/`): Symbol table construction and type checking (partially complete)
5. **IR Generation** (`src/ir/`): Converts AST to three-address code intermediate representation
6. **Assembly Generation** (`src/gen/`): RISC-V assembly output (not yet implemented)

### Important Data Structures:

- **Token types**: Control flow, functions, types, operators, literals, identifiers
- **AST nodes**: Functions, variables, arrays, expressions, statements with full type information
- **IR instructions**: Binary/unary operations, moves, loads, stores, branches, calls, returns
- **Symbol tables**: Scoped tracking of functions, variables, and types

### Type System:

Rich type system with integers (i8-i64), unsigned integers (u8-u64), floats (f32/f64), booleans, arrays, and void. Storage classes include const, var, and vol.

## Language Specification

The complete grammar is defined in `annex.g4` using ANTLR4 syntax. Key language features:

- **Functions**: `fn name{params} return_type { ... }`
- **Variables**: `var type name;` or `const type name;`
- **Arrays**: `var type[size] name;`
- **Control flow**: if/elif/else, while, for loops
- **Expressions**: Full operator precedence with binary/unary operators

## Testing and Debugging

### Test Structure:
- Integration tests in `tests/files/` with `.ax` source files and `.expected` output files
- Two main test categories: lexer tokenization and parser tree generation

### Debugging Utilities:
- Extensive print utilities for AST and parse trees with proper indentation
- Comprehensive logging using `colog` with colored output
- Custom error types for each compiler phase

### Current Implementation Status:
- ✅ **Complete**: Lexer, Parser, AST generation
- ⚠️ **Partial**: Semantic analysis (basic symbol tables), IR generation (missing arrays/function calls)
- ❌ **Missing**: Assembly generation

## Development Notes

- The project uses minimal dependencies and implements most functionality from scratch
- Error handling is comprehensive with custom error types per module
- The IR uses infinite register model with later register allocation
- Assembly target is RISC-V architecture
- Code follows Rust conventions with extensive use of Result types for error handling