# Roadmap

Each phase produces a working compiler for a growing subset of Algol 68. Phases
marked with **Tutorial** include a diversion to write example-laden documentation
of the features built so far. The tutorials serve two audiences: people learning
Algol 68, and people learning compiler construction with Rust + LLVM.

---

## Phase 0: Project Skeleton

**Goal:** Cargo workspace, LLVM bindings, end-to-end "hello world" pipeline.

- [ ] Initialize Cargo workspace with crates:
  - `algol68c` — compiler binary (CLI driver)
  - `algol68-frontend` — lexer, parser, AST, mode resolver
  - `algol68-codegen` — LLVM IR generation
  - `algol68-runtime` — small Rust runtime library linked into compiled programs
- [ ] Set up `inkwell` (safe Rust wrapper for LLVM C API) or `llvm-sys`
- [ ] Hardcode a minimal IR module that prints "Hello, Algol 68" and verify the
  full pipeline works: emit IR → invoke LLVM → link with runtime → run binary
- [ ] Set up `cargo test` harness with integration tests: compile `.a68` file,
  run binary, assert stdout
- [ ] CI (GitHub Actions): build + test on Linux and macOS

**Deliverable:** `cargo run -- examples/hello.a68 -o hello && ./hello` prints
output, even if the "compilation" is entirely hardcoded at this stage.

---

## Phase 1: Lexer and Literals

**Goal:** Tokenize Algol 68 source into a positioned token stream.

- [ ] Token types: keywords (bold stropping), identifiers, integer denotations,
  real denotations, character denotations, string denotations, operator symbols,
  punctuation (`,` `;` `(` `)` `:=` `=`), comments
- [ ] Source position tracking: file, line, column on every token
- [ ] Error recovery: skip to next semicolon or keyword on malformed token
- [ ] Keyword set: `BEGIN` `END` `IF` `THEN` `ELSE` `ELIF` `FI` `CASE` `IN`
  `OUT` `OUSE` `ESAC` `FOR` `FROM` `BY` `TO` `WHILE` `DO` `OD` `PAR` `LOC`
  `HEAP` `REF` `PROC` `OP` `PRIO` `MODE` `STRUCT` `UNION` `FLEX` `NIL` `OF`
  `TRUE` `FALSE` `VOID` `INT` `REAL` `BOOL` `CHAR` `STRING` `BITS` `BYTES`
  `SKIP` `CO` `COMMENT` `COMPL` `LONG` `SHORT` `AND` `OR` `NOT`
- [ ] Tests: tokenize representative snippets, verify token types and positions

---

## Phase 2: Parser — Expressions and Simple Declarations

**Goal:** Parse expressions with correct operator precedence. Parse identity and
variable declarations.

- [ ] Pratt parser for expressions:
  - Numeric and string denotations
  - Identifier references
  - Parenthesized sub-expressions
  - Monadic operators: `NOT`, `-`, `ABS`
  - Dyadic operators at standard priorities: `+` `-` `*` `/` `%` `<` `>` `<=`
    `>=` `=` `/=` `AND` `OR`
  - Procedure calls: `name(args)`
- [ ] Serial clauses: `BEGIN ... END` and `( ... )`
- [ ] Identity declarations: `INT x = 42`
- [ ] Variable declarations: `INT x := 42`
- [ ] AST pretty-printer (for debugging: dump AST as indented text)
- [ ] Tests: parse → pretty-print round-trip on expression examples

---

## Phase 3: INT, REAL, BOOL — First Code Generation

**Goal:** Compile arithmetic expressions and simple declarations to LLVM IR.
Produce a running binary.

- [ ] Map INT → i64, REAL → double, BOOL → i1
- [ ] Emit LLVM IR for:
  - Integer arithmetic: `+` `-` `*` `/` `MOD`
  - Real arithmetic: `+` `-` `*` `/`
  - Comparison operators → i1
  - Boolean operators: `AND` `OR` `NOT`
  - `ABS` (monadic, for INT and REAL)
- [ ] Identity declarations → LLVM constants or SSA values
- [ ] Variable declarations → `alloca` + `store` + implicit REF
- [ ] Assignment `:=` → `store` through pointer
- [ ] Variable use → `load` from pointer (dereferencing)
- [ ] `print` for INT, REAL, BOOL → call runtime print functions
- [ ] Wrap program body in a `main` function
- [ ] Tests: compile and run programs, assert output

Example program this phase can compile:
```algol68
BEGIN
  INT x := 10;
  INT y := 3;
  print(x * y + 1)
END
```

### Tutorial 1: "Arithmetic and Types"

- [ ] Write `docs/tutorials/01-arithmetic-and-types.md`
- [ ] Topics: primitive modes, identity vs variable declarations, the reference
  model (why `INT x := 5` is really `REF INT x = LOC INT := 5`), how LLVM IR
  represents these concepts
- [ ] 10+ worked examples with source, expected output, and annotated LLVM IR
- [ ] Include "try it yourself" exercises

---

## Phase 4: Control Flow

**Goal:** IF/THEN/ELSE/FI, CASE/IN/OUT/ESAC, loops.

- [ ] Parse and compile conditional clauses:
  - `IF cond THEN expr FI` (void result)
  - `IF cond THEN expr ELSE expr FI` (expression-valued)
  - `ELIF` chains
- [ ] Parse and compile case clauses:
  - `CASE n IN val1, val2, ... OUT default ESAC`
- [ ] Parse and compile loop clauses:
  - `FOR i FROM start BY step TO stop DO body OD`
  - `WHILE cond DO body OD`
  - `DO body OD` (infinite loop, needs exit mechanism)
  - All parts optional except `DO ... OD`
  - Loop counter `i` is a fresh constant each iteration
- [ ] Expression-oriented: IF and CASE yield values, generate phi nodes
- [ ] Serial clauses with multiple units separated by `;`
- [ ] `SKIP` (yields undefined value of required mode)
- [ ] Tests: conditional programs, loop programs, nested control flow

Example program:
```algol68
BEGIN
  FOR i FROM 1 TO 20
  DO
    IF i MOD 15 = 0 THEN print("FizzBuzz")
    ELIF i MOD 3 = 0 THEN print("Fizz")
    ELIF i MOD 5 = 0 THEN print("Buzz")
    ELSE print(i)
    FI;
    print(newline)
  OD
END
```

### Tutorial 2: "Control Flow as Expressions"

- [ ] Write `docs/tutorials/02-control-flow.md`
- [ ] Topics: why every construct is an expression, how IF compiles to basic
  blocks + phi, the loop counter as immutable binding, comparison with C's
  statement-oriented design
- [ ] Annotated LLVM IR for FizzBuzz
- [ ] Exercises: rewrite C programs in Algol 68 idioms

---

## Phase 5: CHAR, STRING, and Basic I/O

**Goal:** Character and string handling. Read from stdin, write to stdout.

- [ ] CHAR → i8, STRING → runtime string type {ptr, len}
- [ ] String denotations (literals in source)
- [ ] String concatenation (`+`)
- [ ] String comparison
- [ ] Character operations
- [ ] Runtime string library: allocate, concatenate, compare, print
- [ ] `print(...)` — variadic, handles INT, REAL, BOOL, CHAR, STRING
- [ ] `read(...)` — read values from stdin
- [ ] `newline`, `newpage`, `space` — output formatting
- [ ] Tests: string manipulation programs, I/O programs

---

## Phase 6: PROC — Procedures and Recursion

**Goal:** First-class procedures, recursion, mutual recursion, higher-order
functions.

- [ ] Parse PROC declarations:
  - `PROC name = (params) result: body`
  - `PROC (INT) INT` as a mode (procedure type)
  - Anonymous procedure denotations (lambdas)
- [ ] Compile procedures to LLVM functions
- [ ] Calling convention: pass parameters, return result
- [ ] Recursion: procedure can call itself
- [ ] Mutual recursion: two procedures call each other (requires forward
  declaration or two-pass within a scope)
- [ ] Higher-order: pass PROC as argument, return PROC as result
- [ ] Procedures as values: store in variables, pass around
- [ ] Tests: factorial, fibonacci, map-over-array, function composition

Example program:
```algol68
BEGIN
  PROC gcd = (INT a, b) INT:
    IF b = 0 THEN a ELSE gcd(b, a MOD b) FI;

  PROC apply twice = (PROC (INT) INT f, INT x) INT:
    f(f(x));

  PROC double = (INT n) INT: n * 2;

  print(gcd(48, 18));
  print(newline);
  print(apply twice(double, 3))
END
```

### Tutorial 3: "Procedures as Values"

- [ ] Write `docs/tutorials/03-procedures.md`
- [ ] Topics: PROC as a mode, procedures are values not declarations, identity
  vs variable for procedures (`PROC f = ...` vs `PROC f := ...`), closures and
  environment capture, how LLVM represents function pointers + environments
- [ ] Walk through compilation of higher-order function example
- [ ] Exercises: implement standard library functions (map, filter, fold)

---

## Phase 7: The Coercion Engine

**Goal:** Implement implicit coercions. This is the defining feature of Algol 68
and the hardest part of the compiler.

- [ ] Define the five coercion contexts in the compiler:
  - Soft, Weak, Meek, Firm, Strong
- [ ] Annotate every AST position with its coercion context
- [ ] Implement coercion chain resolution:
  1. Dereferencing: REF M → M (already partially done for variables)
  2. Widening: INT → REAL
  3. Voiding: any → VOID (discard value in statement context)
  4. Deproceduring: PROC VOID → VOID (call parameterless procedure implicitly)
- [ ] Insert explicit coercion nodes into the typed AST
- [ ] Generate LLVM IR for coercion chains (load for deref, sitofp for widen,
  call for deprocedure, nothing for void)
- [ ] Tests: programs that rely on implicit coercions
  - `REAL x = 5` (widening INT to REAL)
  - `x` in expression context (dereferencing REF INT to INT)
  - Statement-position expressions (voiding)
  - Parameterless procedure used as value (deproceduring)
- [ ] Comprehensive error messages for failed coercions

Example demonstrating coercions:
```algol68
BEGIN
  INT n := 42;
  REAL x := n;          CO widening: INT to REAL CO
  REAL y := 3;          CO widening: INT literal to REAL CO
  x := x + y;          CO deref x, deref y, add, store CO

  PROC greeting = VOID: print("hello");
  greeting              CO deproceduring: call without () CO
END
```

### Tutorial 4: "The Coercion System Demystified"

- [ ] Write `docs/tutorials/04-coercions.md`
- [ ] Topics: why Algol 68 has implicit coercions, the six coercions explained
  with examples, the five contexts and where each applies, how the compiler
  resolves coercion chains, comparison with C's implicit conversions (simpler)
  and Rust's explicit conversions (stricter)
- [ ] Visual diagrams: coercion lattice, context strength hierarchy
- [ ] Annotated examples showing before/after coercion insertion
- [ ] Exercises: predict what coercions the compiler will insert

---

## Phase 8: STRUCT and MODE

**Goal:** User-defined record types and type aliases.

- [ ] Parse STRUCT modes: `STRUCT (INT x, REAL y)`
- [ ] Parse MODE declarations: `MODE POINT = STRUCT (REAL x, REAL y)`
- [ ] Field selection: `value OF struct` or `struct[field]`
- [ ] STRUCT denotations (literal construction)
- [ ] Compile STRUCT to LLVM struct types
- [ ] Field access → LLVM `getelementptr` + load
- [ ] Field assignment → `getelementptr` + store (through REF STRUCT)
- [ ] Recursive modes: `MODE LIST = STRUCT (INT head, REF LIST tail)`
  - Requires REF indirection for recursion
  - Named mode resolution with cycle detection
- [ ] Structural mode equivalence (two STRUCTs with same fields are the same
  mode, regardless of MODE name)
- [ ] Tests: linked lists, trees, points/rectangles, nested structs

Example program:
```algol68
BEGIN
  MODE POINT = STRUCT (REAL x, REAL y);

  PROC distance = (POINT a, POINT b) REAL:
  BEGIN
    REAL dx = x OF a - x OF b;
    REAL dy = y OF a - y OF b;
    sqrt(dx * dx + dy * dy)
  END;

  POINT p = (3.0, 4.0);
  POINT q = (0.0, 0.0);
  print(distance(p, q))
END
```

### Tutorial 5: "User-Defined Types"

- [ ] Write `docs/tutorials/05-structs-and-modes.md`
- [ ] Topics: STRUCT as product types, MODE as type-level naming, structural
  equivalence vs nominal typing, recursive types through REF, how STRUCTs map
  to LLVM struct types, the OF operator for field access
- [ ] Build up from POINT to a linked list to a binary tree
- [ ] Show the LLVM IR for struct operations
- [ ] Exercises: implement a small data structure library

---

## Phase 9: UNION and Conformity Clauses

**Goal:** Tagged unions and runtime type discrimination.

- [ ] Parse UNION modes: `UNION (INT, REAL, STRING)`
- [ ] Uniting coercion: inserting a value into a union (strong context)
- [ ] Conformity clause (pattern matching on unions):
  ```algol68
  CASE expr IN
    (INT i): ...,
    (REAL r): ...,
    (STRING s): ...
  ESAC
  ```
- [ ] UNION representation: `{i32 tag, [N x i8] payload}` where N is the size
  of the largest variant
- [ ] Tag assignment: each mode in the union gets a unique integer tag
- [ ] Conformity clause → switch on tag, bitcast payload to correct type
- [ ] VOID IN union (for optional-value patterns)
- [ ] Tests: heterogeneous data, option types, expression trees

Example program:
```algol68
BEGIN
  MODE ATOM = UNION (INT, REAL, STRING);

  PROC describe = (ATOM a) VOID:
    CASE a IN
      (INT i):    BEGIN print("integer: "); print(i) END,
      (REAL r):   BEGIN print("real: "); print(r) END,
      (STRING s): BEGIN print("string: "); print(s) END
    ESAC;

  ATOM x := 42;
  ATOM y := 3.14;
  ATOM z := "hello";
  describe(x); print(newline);
  describe(y); print(newline);
  describe(z); print(newline)
END
```

---

## Phase 10: Arrays (Rows)

**Goal:** Fixed and flexible arrays with bounds, slicing, and multi-dimensional
support.

- [ ] Parse row modes: `[n] INT`, `[1:10] INT`, `[,] REAL` (2D)
- [ ] Row denotations (literal arrays): `(1, 2, 3, 4, 5)`
- [ ] Array representation: `{ptr, lwb, upb}` per dimension
- [ ] Indexing: `a[i]` with bounds checking (runtime error on out-of-bounds)
- [ ] Slicing / trimming: `a[3:7]` yields a sub-array (shares storage)
- [ ] Multi-dimensional arrays: `[1:m, 1:n] REAL`
- [ ] `LWB` and `UPB` operators (query bounds)
- [ ] Array assignment (element-wise)
- [ ] FLEX arrays (resizable): `FLEX [1:0] INT` — requires heap allocation
- [ ] Rowing coercion: scalar → 1-element array (strong context)
- [ ] Tests: sorting algorithms, matrix operations, string-as-array-of-char

### Tutorial 6: "Rows, Slicing, and Multidimensional Arrays"

- [ ] Write `docs/tutorials/06-arrays.md`
- [ ] Topics: Algol 68 rows vs C arrays (bounds-checked, 1-indexed by default,
  sliceable, flexible), FLEX arrays, multi-dimensional layouts, how bounds are
  carried at runtime, LLVM IR for bounds-checked indexing
- [ ] Example: implement insertion sort with annotated LLVM IR
- [ ] Exercises: matrix multiplication, dynamic array builder

---

## Phase 11: HEAP Allocation and Garbage Collection

**Goal:** Heap-allocated values with automatic memory reclamation.

- [ ] `HEAP MODE` generator → runtime allocation call
- [ ] `NIL` → null pointer (typed per context)
- [ ] NIL check: runtime error on dereferencing NIL
- [ ] Garbage collector options (choose one):
  - Link against Boehm GC (simplest, production-quality, conservative)
  - Implement simple mark-sweep in the runtime (educational, more control)
- [ ] Scope checking (simplified): warn or error when a REF to a LOC value
  escapes its scope
- [ ] Tests: linked list construction, tree building and traversal, programs
  that allocate heavily

Example program:
```algol68
BEGIN
  MODE NODE = STRUCT (INT value, REF NODE next);

  PROC make list = (INT n) REF NODE:
    IF n = 0
    THEN NIL
    ELSE
      HEAP NODE := (n, make list(n - 1))
    FI;

  PROC print list = (REF NODE head) VOID:
    IF head IS NIL
    THEN SKIP
    ELSE
      print(value OF head);
      print(" ");
      print list(next OF head)
    FI;

  print list(make list(10))
END
```

### Tutorial 7: "Memory Management — LOC, HEAP, and the GC"

- [ ] Write `docs/tutorials/07-memory.md`
- [ ] Topics: stack vs heap allocation, when to use LOC vs HEAP, NIL and IS/ISNT
  for reference comparison, how the garbage collector works, scope rules and why
  they matter, comparison with Rust's ownership model and C's manual management
- [ ] Diagrams: stack frames, heap layout, GC mark phase
- [ ] Exercises: implement a simple allocator, build a persistent data structure

---

## Phase 12: Operator Definitions

**Goal:** User-defined operators with custom priority.

- [ ] Parse operator declarations:
  - `OP + = (POINT a, POINT b) POINT: ...` (overload existing)
  - `OP DIST = (POINT a, POINT b) REAL: ...` (new named operator)
  - `PRIO DIST = 7` (set priority)
- [ ] Operator identification: given operand modes, find matching OP declaration
  (after coercion in firm context)
- [ ] Monadic operator definitions: `OP - = (POINT p) POINT: ...`
- [ ] Integrate with Pratt parser: update precedence table from PRIO
  declarations
- [ ] Tests: vector arithmetic, complex number type with operators, custom
  comparison operators

Example program:
```algol68
BEGIN
  MODE VEC = STRUCT (REAL x, REAL y);
  OP + = (VEC a, VEC b) VEC: (x OF a + x OF b, y OF a + y OF b);
  OP * = (REAL s, VEC v) VEC: (s * x OF v, s * y OF v);
  OP ABS = (VEC v) REAL: sqrt(x OF v * x OF v + y OF v * y OF v);

  VEC a = (3.0, 4.0);
  VEC b = (1.0, 2.0);
  print(ABS (a + b));
  print(newline);
  print(ABS (2.0 * a))
END
```

---

## Phase 13: Closures and Environment Capture

**Goal:** Procedures that capture variables from enclosing scopes. Proper
closure representation in LLVM IR.

- [ ] Detect which variables a PROC body references from enclosing scopes
- [ ] Allocate a closure environment struct on the heap containing captured
  variables
- [ ] Closure representation: `{fn_ptr, env_ptr}` — same LLVM type for all
  PROC values
- [ ] Calling convention: compiler passes env_ptr as hidden first argument
- [ ] Scope violation detection: error if a closure captures a LOC variable
  that has been deallocated (simplified check: closures stored in HEAP or
  returned from a procedure must not capture LOC variables)
- [ ] Tests: currying, accumulator pattern, callback patterns

### Tutorial 8: "Closures, Scope, and Lifetime"

- [ ] Write `docs/tutorials/08-closures.md`
- [ ] Topics: how Algol 68 closures work (and how they differ from Scheme's),
  the scope rule, why returning a closure that captures local variables is
  illegal in Algol 68, closure representation in LLVM, comparison with Rust
  closures (Fn/FnMut/FnOnce) and C++ lambdas
- [ ] Annotated LLVM IR for a closure example
- [ ] Exercises: implement a simple callback system

---

## Phase 14: PAR and SEMA — Concurrency

**Goal:** Parallel clause execution with semaphore synchronization.

- [ ] Parse PAR clauses: `PAR (task1, task2, task3)`
- [ ] Each branch compiles to a separate LLVM function
- [ ] Runtime thread pool: spawn branches, join on completion
- [ ] SEMA mode: semaphore type in the runtime
  - `SEMA s = LEVEL 1` → create semaphore with initial value
  - `DOWN s` → decrement (block if zero)
  - `UP s` → increment (wake a waiter)
- [ ] Shared variable access: variables visible to multiple PAR branches must
  be protected (at minimum, document the danger)
- [ ] Tests: parallel computation, producer-consumer, dining philosophers

Example program:
```algol68
BEGIN
  SEMA mutex = LEVEL 1;
  INT shared := 0;

  PAR (
    BEGIN
      FOR i TO 1000 DO
        DOWN mutex;
        shared := shared + 1;
        UP mutex
      OD
    END,
    BEGIN
      FOR i TO 1000 DO
        DOWN mutex;
        shared := shared + 1;
        UP mutex
      OD
    END
  );

  print(shared)
END
```

### Tutorial 9: "Parallel Programming in Algol 68"

- [ ] Write `docs/tutorials/09-concurrency.md`
- [ ] Topics: PAR as structured concurrency, semaphores vs mutexes vs channels,
  collateral vs parallel elaboration, race conditions and how SEMA prevents
  them, comparison with Go's goroutines/channels and Rust's Send/Sync
- [ ] Classic examples: producer-consumer, bounded buffer, parallel merge sort
- [ ] Exercises: parallelize a computation, find and fix a race condition

---

## Phase 15: Expanded I/O (Transput)

**Goal:** Algol 68's transput system beyond basic print/read.

- [ ] Files and channels: `FILE`, `open`, `close`, `associate`
- [ ] Formatted output with `printf` and `writef`
- [ ] Formatted input with `readf`
- [ ] Basic FORMAT denotations:
  - `$d$` (decimal integer)
  - `$g$` (general — mode-dependent)
  - `$"text"$` (literal insertion)
  - `$l$` (newline)
  - `$n(...)$` (repetition)
- [ ] `put`, `get` for binary I/O
- [ ] `stand in`, `stand out`, `stand error` — standard channels
- [ ] Tests: formatted output programs, file I/O programs

Note: Full FORMAT is essentially a DSL within Algol 68 and is one of the most
complex parts of the language. This phase implements a practical subset.

---

## Phase 16: Standard Prelude and Library

**Goal:** Built-in mathematical functions, string operations, and environment
primitives that Algol 68 programmers expect.

- [ ] `sqrt`, `exp`, `ln`, `sin`, `cos`, `tan`, `arcsin`, `arccos`, `arctan`
- [ ] `entier` (floor), `round`, `sign`
- [ ] `max int`, `max real`, `small real` — environmental enquiries
- [ ] `bits width`, `bytes width`
- [ ] `random` — pseudo-random REAL in [0, 1)
- [ ] `whole`, `fixed`, `float` — number-to-string conversions
- [ ] `char in string` — character search
- [ ] `string to int`, `string to real` — parsing
- [ ] Tests: mathematical programs, string processing

### Tutorial 10: "The Standard Prelude — Algol 68's Standard Library"

- [ ] Write `docs/tutorials/10-standard-prelude.md`
- [ ] Topics: what the standard prelude provides, environmental enquiries
  (querying the platform), transput (I/O model), mathematical functions,
  comparison with C's stdlib and Rust's std
- [ ] Worked example: a complete small program using standard prelude features
- [ ] Exercises: implement programs from Lindsey & van der Meulen's textbook

---

## Future Phases (Stretch Goals)

These phases extend the compiler beyond a "quality subset" toward full Revised
Report compliance. Each is a significant project in its own right.

### Phase 17: LONG and SHORT Modes
- LONG INT, LONG REAL (extended precision)
- SHORT INT, SHORT REAL (reduced precision)
- LONG LONG INT (arbitrary precision — requires bignum library)
- LONG COMPL, SHORT COMPL

### Phase 18: COMPL (Complex Numbers)
- COMPL mode: `STRUCT (REAL re, im)`
- Complex arithmetic operators
- `RE`, `IM`, `CONJ`, `ARG`, `ABS` for complex numbers

### Phase 19: BITS and BYTES
- BITS mode: packed boolean array with bitwise operations
- BYTES mode: packed character array
- `ELEM`, `BIN`, `ABS` operators for BITS
- Efficient LLVM lowering using integer bitwise operations

### Phase 20: Full Formatted Transput
- Complete FORMAT pattern language
- Static format expressions with type checking
- Dynamic format expressions
- Associated files and channels with format binding

### Phase 21: GOTO and Labels
- Labels as denotable values
- GOTO across block boundaries with proper stack unwinding
- Non-local jump (requires setjmp/longjmp or LLVM personality functions)

### Phase 22: Partial Parametrization (Extension)
- Charles Lindsey's currying proposal
- Partially applied procedures as values
- Following a68g's implementation of this extension

---

## Milestone Summary

| Phase | Feature | Tutorial | Test Programs |
|-------|---------|----------|---------------|
| 0 | Project skeleton | — | hello world |
| 1 | Lexer | — | token verification |
| 2 | Parser | — | AST round-trip |
| 3 | INT, REAL, BOOL, codegen | Arithmetic & Types | calculator programs |
| 4 | Control flow | Control Flow as Expressions | FizzBuzz, loops |
| 5 | CHAR, STRING, I/O | — | string programs |
| 6 | PROC | Procedures as Values | recursion, HOFs |
| 7 | Coercion engine | Coercions Demystified | implicit conversion tests |
| 8 | STRUCT, MODE | User-Defined Types | linked lists, trees |
| 9 | UNION | — | tagged data, option types |
| 10 | Arrays | Rows and Arrays | sorting, matrices |
| 11 | HEAP, GC | Memory Management | heap data structures |
| 12 | OP definitions | — | vector math, DSLs |
| 13 | Closures | Closures and Scope | callbacks, currying |
| 14 | PAR, SEMA | Parallel Programming | concurrent programs |
| 15 | Transput | — | file I/O, formatting |
| 16 | Standard prelude | Standard Prelude | mathematical programs |

**After Phase 7**, you have a compiler that handles the core of the language
including the feature that makes Algol 68 uniquely Algol 68 — the coercion
system. Everything after Phase 7 is additive and each phase produces a more
capable but already-working compiler.

**After Phase 14**, you have a compiler that handles a substantial subset
comparable to ALGOL 68-R or ALGOL 68C, generating native code via LLVM that
runs on modern hardware. This is the "non-toy" threshold and would be, to our
knowledge, the only Algol 68 compiler written in Rust and targeting LLVM IR.
