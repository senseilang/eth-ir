# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Ethereum IR (Intermediate Representation) compiler/backend project written in Rust. It defines an IR for EVM bytecode and provides a backend for compiling this IR to actual EVM bytecode.

## Development Commands

### Code Quality
```bash
# Check if code compiles without building
cargo check

# Run clippy for linting
cargo +nightly clippy --workspace --all --all-features --locked -- -D warnings

# Format code
cargo fmt
```

### Testing
```bash
cargo test
```

## Architecture

### Workspace Structure
- **`/crates/representation`** (package: `eth-ir`): Core IR data structures
  - Defines IR types: functions, basic blocks, statements, operations
  - Local variable management with `LocalId` system
  - Support for EVM opcodes and control flow

- **`/crates/backend`** (package: `eth-ir-backend`): Backend compiler
  - `simple.rs`: Simple backend implementation with static memory allocation
  - `stack.rs`: Stack management for EVM stack scheduling

### Key IR Concepts
- **Functions**: Can be main (entry point) or internal functions with inputs/outputs
- **Basic Blocks**: Control flow units with parameters, statements, and termination
- **Statements**: Operations with inputs (`OpInput`) and output locals
- **Local Variables**: Single-assignment variables identified by `LocalId`
- **Control Flow**: Goto, branch, switch, and internal returns

### Important Types
- `IR`: Top-level structure containing functions and data section
- `IROp`: Enum of all supported operations (arithmetic, memory, control flow)
- `OpInput`: Either a local variable reference or a constant
- `BBEnd`: How a basic block terminates (goto, branch, switch, return)

## Development Notes

- Uses Rust 2024 edition
- Dependencies include `alloy-primitives` for Ethereum types and `evm-glue` for opcodes
- The project is work-in-progress (commit: "WIP initial IR")
- No test infrastructure currently exists