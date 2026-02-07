# algol68-llvm

An Algol 68 compiler targeting LLVM IR, written in Rust.

## Why

Someone once quipped that building a C compiler is a solved student exercise —
something impressive would be an Algol 68 compiler that generates modern LLVM
IR. They were right. Algol 68 is one of the most ambitious language designs ever
attempted: orthogonal type composition, six implicit coercions across five
syntactic contexts, first-class procedures, parallel clauses, and a formal
definition so powerful it's Turing-complete. No mainstream toolchain targets it
today.

This project takes that challenge seriously. By targeting LLVM IR, the compiled
output benefits from world-class optimization passes and runs on every
architecture LLVM supports — x86-64, AArch64, RISC-V, WebAssembly, and more.

## Goals

- Implement the **Revised Report on the Algorithmic Language ALGOL 68** as
  faithfully as practical, in incremental phases
- Generate efficient native code via LLVM IR — not an interpreter
- Develop tutorials at each milestone so the project doubles as a learning
  resource for both Algol 68 the language and compiler construction with Rust +
  LLVM
- Prioritize correctness over completeness — each phase should produce a
  working compiler for a growing subset

## Non-Goals

- Bit-for-bit compatibility with a68g or any other implementation
- Full formatted transput (the FORMAT mini-language) in early phases
- LONG LONG arbitrary-precision arithmetic
- Pretending the W-grammar is a parsing algorithm

## Architecture Overview

```
Algol 68 source (.a68)
  --> Lexer (hand-coded, position-tracked)
  --> Parser (recursive descent + Pratt for expressions)
  --> AST with mode annotations
  --> Mode resolver (structural equivalence, coercion insertion)
  --> LLVM IR emitter (inkwell / llvm-sys)
  --> LLVM optimization passes
  --> Native binary (via LLVM's codegen)
```

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for the detailed compiler
pipeline design.

## Roadmap

The project follows a phased approach. Each phase produces a working compiler
for a larger subset of the language, and most phases include a tutorial diversion
that documents what has been built so far with worked examples.

See [docs/ROADMAP.md](docs/ROADMAP.md) for the full plan.

## Building

*To be added once Phase 0 is complete.*

```bash
cargo build --release
./target/release/algol68c examples/hello.a68 -o hello
./hello
```

## References

- [Revised Report on the Algorithmic Language ALGOL 68](https://jmvdveer.home.xs4all.nl/en.post.algol-68-revised-report.html)
- Lindsey, C.H. & van der Meulen, S.G. — *Informal Introduction to ALGOL 68*
- Tanenbaum, A.S. — *A Tutorial on ALGOL 68*
- [Algol 68 Genie (a68g)](https://jmvdveer.home.xs4all.nl/en.algol-68-genie.html) — reference interpreter
- [Learning Algol 68 Genie (PDF)](https://jmvdveer.home.xs4all.nl/learning-algol-68-genie.pdf)

## License

MIT
