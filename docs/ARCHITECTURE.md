# Compiler Architecture

## Pipeline Overview

```
                  ┌─────────┐
  source.a68 ──>  │  Lexer  │
                  └────┬────┘
                       │ Token stream (with source positions)
                  ┌────▼────┐
                  │ Parser  │
                  └────┬────┘
                       │ Untyped AST
                  ┌────▼─────────┐
                  │ Mode Resolver│
                  └────┬─────────┘
                       │ Typed AST (modes assigned, coercions inserted)
                  ┌────▼──────────┐
                  │ IR Lowering   │
                  └────┬──────────┘
                       │ LLVM IR (in-memory Module)
                  ┌────▼──────────┐
                  │ LLVM Passes   │
                  └────┬──────────┘
                       │ Optimized IR
                  ┌────▼──────────┐
                  │ Object Emit   │
                  └────┬──────────┘
                       │ .o file
                  ┌────▼──────────┐
                  │ Linker (cc)   │
                  └────┬──────────┘
                       ▼
                    native binary
```

## Lexer

Hand-coded lexer. No generator tools — Algol 68's lexical structure is
straightforward once you separate it from the W-grammar formalism.

Responsibilities:
- Tokenize keywords (bold stropping: `IF`, `THEN`, `FI`, etc.)
- Handle alternative stropping styles (dot stropping `.IF.`, quote stropping
  'if', upper stropping) — start with bold/upper only
- Numeric literals: integers, reals, real-with-exponent
- String denotations (with escape sequences)
- Operator symbols (built-in and user-defined)
- Track source positions (file, line, column) on every token for diagnostics
- Skip comments (Algol 68 uses `CO ... CO` or `COMMENT ... COMMENT` or
  `# ... #`)

Design choice: **definition-before-use** (following ALGOL 68-R). This avoids
the need for a forward-declaration pass and simplifies the parser significantly.
The Revised Report allows mutual recursion through MODE declarations, which
we handle as a special case.

## Parser

Two parsing strategies combined:

**Recursive descent** for declarations, clauses, and control structures.
IF/CASE/DO blocks have unambiguous keyword delimiters (FI/ESAC/OD), so there
is no dangling-else problem.

**Pratt parser (precedence climbing)** for expressions and formulas. This
handles:
- Built-in operator priorities (the Revised Report defines 10 priority levels)
- User-defined operators with `PRIO` declarations
- Monadic (prefix) and dyadic (infix) operator forms
- Implicit dereferencing context

The Pratt parser is essential because operator priorities are user-configurable.
An LALR/PEG grammar cannot accommodate this without regeneration.

## AST

The AST is a tree of Rust enums. Each node carries:
- A `Span` (source location for error messages)
- An optional `Mode` (filled in by the mode resolver)
- An optional `Coercion` chain (inserted by the mode resolver)

Key node types:

```
Unit           — the fundamental expression
  Denotation   — literal value (INT, REAL, BOOL, CHAR, STRING)
  Identifier   — name reference
  Formula      — operator application (monadic or dyadic)
  Call         — procedure call
  Slice        — array indexing / trimming
  Selection    — struct field access
  Assignment   — target := source
  Generator    — LOC mode / HEAP mode
  Cast         — mode(expr) explicit coercion
  Nihil        — NIL (null reference)

Clause         — compound expression
  Serial       — BEGIN ... END or ( ... )
  Conditional  — IF ... THEN ... ELSE ... FI
  Case         — CASE ... IN ... OUT ... ESAC
  Conformity   — CASE union IN (MODE x): ... ESAC
  Loop         — FOR ... FROM ... BY ... TO ... WHILE ... DO ... OD
  Parallel     — PAR ( ... )
  Collateral   — ( expr, expr, expr )

Declaration
  Identity     — MODE name = value
  Variable     — MODE name := value  (desugars to REF MODE name = LOC MODE := value)
  Procedure    — PROC name = (params) result: body
  Operator     — OP name = (params) result: body
  Priority     — PRIO op = n
  ModeDecl     — MODE NAME = mode-expression
```

## Mode System

Algol 68 calls types "modes." The mode system is the heart of the compiler.

### Mode Representation

```rust
enum Mode {
    // Primitives
    Int, Real, Bool, Char, Void,
    // Compound
    Ref(Box<Mode>),
    Row { flex: bool, dimensions: usize, element: Box<Mode> },
    Struct(Vec<(String, Mode)>),
    Union(Vec<Mode>),
    Proc { params: Vec<Mode>, result: Box<Mode> },
    // Named (for user-defined modes and recursion)
    Named(String),
}
```

### Structural Equivalence

Two modes are equivalent if they have the same structure, not the same name.
`MODE A = STRUCT(INT x)` and `MODE B = STRUCT(INT x)` are the same mode.
This requires a recursive comparison with cycle detection for recursive types
like `MODE LIST = STRUCT(INT head, REF LIST tail)`.

### Coercion Engine

The coercion engine is a function:

```
coerce(value, a_priori_mode, a_posteriori_mode, context_strength) -> CoercedValue
```

It applies a chain of coercions based on context strength:

| Context  | Allowed coercions                                    |
|----------|------------------------------------------------------|
| Soft     | deproceduring                                        |
| Weak     | soft + dereferencing (to REF, not through it)        |
| Meek     | soft + dereferencing (fully)                          |
| Firm     | meek + uniting                                       |
| Strong   | firm + widening + rowing + voiding                   |

Implementation strategy: **build a coercion chain at type-check time, store it
on the AST node, execute it at IR generation time.** This separates "what
coercions are needed" from "how to emit them."

Phase-in plan:
1. Dereferencing only (covers 70% of real code)
2. Add widening (INT->REAL, covers arithmetic)
3. Add voiding (expression-as-statement)
4. Add deproceduring (parameterless PROC calls)
5. Add uniting (UNION insertion)
6. Add rowing (scalar to array promotion — rare in practice)

## IR Lowering (LLVM)

### Value Representation

Unlike a tree-walking interpreter, a compiler maps modes directly to LLVM types:

| Algol 68 mode | LLVM type | Notes |
|---------------|-----------|-------|
| INT           | i64       | 64-bit signed |
| REAL          | double    | IEEE 754 |
| BOOL          | i1        | Single bit |
| CHAR          | i8        | ASCII/UTF-8 byte |
| VOID          | void      | Unit |
| REF m         | ptr       | Opaque pointer (LLVM 15+) |
| STRING        | {ptr, i64}| Pointer + length |
| STRUCT(...)   | {fields...}| LLVM struct type |
| UNION(...)    | {i32, [N x i8]} | Tag + payload (sized to largest variant) |
| PROC(...)     | {ptr, ptr}| Function pointer + environment pointer |
| [] m          | {ptr, i64, i64} | Data pointer + lower bound + upper bound |

### Memory

- `LOC m` → `alloca` in the current function's entry block
- `HEAP m` → call to runtime allocator (GC-managed)
- Assignment (`:=`) → `store` through a pointer
- Dereferencing → `load` from a pointer

### Procedures and Closures

Every PROC compiles to an LLVM function. If it captures variables from an
enclosing scope, it becomes a closure:

```llvm
; Closure = {function_ptr, env_ptr}
; env is a heap-allocated struct containing captured variables
%closure = type { ptr, ptr }
```

Calling a closure: extract the function pointer and environment pointer, pass
the environment as the first argument.

### Control Flow

Algol 68's expression-oriented control flow maps to LLVM's basic blocks with
phi nodes:

```llvm
; IF x > 0 THEN x ELSE -x FI
  %cmp = icmp sgt i64 %x, 0
  br i1 %cmp, label %then, label %else

then:
  br label %merge

else:
  %neg = sub i64 0, %x
  br label %merge

merge:
  %result = phi i64 [ %x, %then ], [ %neg, %else ]
```

### PAR (Parallel Clauses)

Each branch of a PAR clause compiles to a separate LLVM function. The runtime
spawns threads (or green threads), executes each branch, and joins.

SEMA maps to a runtime semaphore type with UP/DOWN operations.

## Runtime Library

A small Rust runtime library linked into every compiled program:

- **Memory**: Allocator + garbage collector (mark-sweep or Boehm GC via FFI)
- **Strings**: Allocation, concatenation, comparison, conversion
- **I/O**: `print`, `read`, `newline`, `newpage` (basic transput)
- **Concurrency**: Thread pool, semaphore, PAR join barrier
- **Panic**: Runtime error handler (bounds checks, nil dereference, scope
  violations)

The runtime should be minimal. Most operations compile to inline LLVM IR. The
runtime handles only what requires heap interaction or OS calls.

## Error Reporting

Every AST node and token carries source position information. Errors include:

- Source file, line, column
- The offending source text (underlined)
- Expected vs. actual mode (for type errors)
- Coercion context (for "cannot coerce X to Y in meek context")
- Suggestions where possible ("did you mean `:=` instead of `=`?")

Error philosophy: **fail fast with clear messages.** A compiler that says
"mode error at line 42" is useless. A compiler that says "cannot widen BOOL to
INT in assignment at line 42, column 12" is helpful.
