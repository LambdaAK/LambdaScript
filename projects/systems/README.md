# systems: a machine emulator and OS simulations in Forge

A ~1500-line Forge program that stress-tests the language with classic
systems material:

- **Part 0: a persistent AVL map** (`avl.forge`). A generic, height-balanced
  `Tree<a>`. A map is `Map<k, v> = Tree<(k, v)>` with an `Ord k` constraint.
  It supports insert, find, remove (rebalancing with rotations), folds and
  `fmap`. Everything else stores its state in it.
- **Part 1: a register machine and its assembler** (`cpu.forge`, `asm.forge`,
  `programs.forge`).
  - The machine has eight registers, zero and negative flags, a stack
    pointer, and 256 words of data memory in an AVL map keyed by address.
    The stack grows down from 256 and overflows below 192.
  - Instructions: `mov ldi load store add sub mul div mod cmp jmp jz jnz jlt
    jgt jle jge call ret push pop out halt`.
  - The assembler makes two passes: labels in the first, instructions in the
    second. It supports `; comments`, `.equ NAME value` and
    `.data ADDR v1 v2 ...`, and reports every error with its line number.
  - Demo programs: sum 1..100, iterative Fibonacci, recursive Fibonacci for
    n = 0..15 (46k instructions, using call/ret and the stack), bubble sort
    in memory, gcd of five pairs, and recursive factorials.
  - Fault demos: division by zero, stack overflow, running off the end of
    the program, and a program with 11 kinds of assembly error.
  - Each run prints its output, instruction count, register and flag dump,
    maximum stack depth, and a per-mnemonic histogram. Some runs also print
    a disassembly listing.
- **Part 2: process scheduling** (`sched.forge`). FCFS, SJF, SRTF
  (preemptive), round robin (quantum 3) and non-preemptive priority, over six
  jobs that include an idle gap. Each policy prints a text Gantt chart,
  per-job completion, turnaround, waiting and response times, and averages.
  The policies are then ranked.
- **Part 3a: heap allocation** (`alloc.forge`). First fit, best fit and worst
  fit over an address-ordered free list of a 64-word heap, with splitting and
  coalescing. After each of 21 alloc/free requests it prints the heap map,
  free words, holes, the largest hole and external fragmentation. Some of
  these requests are failures and bad frees.
- **Part 3b: page replacement** (`paging.forge`). FIFO, LRU and OPT on the
  textbook reference string, as frame-by-frame traces. It also prints a
  fault table for 1 to 7 frames and shows Belady's anomaly (FIFO gives 9
  faults with 3 frames and 10 with 4).

`fmt.forge` holds list and string helpers: padding, joining, fixed-point
floats, `parse_int`, splitting, the `Named` trait and the `cat!` macro.

## Running

From the repository root:

```sh
# interpreter
./_build/default/bin/interpreter.exe projects/systems/main.forge

# native: the compiler writes .mir/.ll/.s next to the source, so use a copy
T=$(mktemp -d); cp projects/systems/*.forge $T/
./_build/default/bin/compile_forge.exe $T/main.forge $T/main.exe && $T/main.exe
```

Both back ends print exactly `main.out` (353 lines). Both have empty stderr
and exit status 0.

Run times (Apple Silicon laptop, wall clock):

| | time |
|---|---|
| interpreter | about 3.7 s |
| native compile | about 1.1 s |
| native executable | about 0.02 s |

## Forge features exercised

- **Data types:** sum types such as `Instr`, `Operand`, `Cond`, `Status`,
  `Policy`, `Request` and `Outcome`, and named records such as `Machine`,
  `Flags`, `Job`, `Slice`, `Heap` and `State`.
- **Records:** record update `{ m with pc = ... }`, including nested
  updates, and row-polymorphic field access.
- **Polymorphism:** the polymorphic recursive type `Tree<a>` and the alias
  `Map<k, v>`.
- **Traits:**
  - User `impl Show` for `Reg`, `Operand`, `Instr` (a disassembler) and
    `Status`.
  - User `impl Eq` and `impl Ord` for `Reg`, which key the register file in
    the AVL map through `compare`.
  - A user trait `Named` with a default method (`tag`), an instance for
    `List<a> requires Named<a>`, and instances declared inside modules.
- **Higher-kinded traits:** `Functor` and `Foldable` for `Tree`, and a
  user-defined `Res<a>` that is a `Functor`, `Applicative` and `Monad`. The
  assembler's operand parsers chain with `>>=`.
- **Modules and imports:** nested `mod ... where ... end`, `use`
  (including inside a module), and `import` across 9 files, with shared
  imports included once.
- **Macros:** the `macro_rules!` macro `cat!`, which uses `$( ... ),*`
  repetition.
- **Lists:** comprehensions with refutable generator patterns
  (`[p | Some p <- slots]`, `[m.steps | Some m <- runs]`) and ranges.
- **Functions:** closures, currying, operator sections like `(+)`,
  `let rec`, and pipelines.
- **Expressions:** blocks with statements, and nested pattern matching on
  tuples, lists, constructors and literals (`'-' :: rest`, `".equ" :: name
  :: value :: []`).
- **Long-running loops:** the machine's run loop is tail recursive and runs
  up to 46k steps.
- **Other:** structural comparison on tuples, `Option`, `Ordering`, and
  float formatting.

## Bugs and missing features found

Each one is reduced to a small program in `bugs/`. The comment at the top of
each file gives the expected and actual behaviour and how to run it.

| file | back end | problem | workaround in this project |
|---|---|---|---|
| `01-qualified-ctor-pattern.forge` | both | `M.A` works as an expression but is a syntax error in a pattern. | `use M` before matching. |
| `02-macro-repetition-application.forge` | both | Inside `$($x:expr),*`, an argument such as `f x` is split into the two fragments `f` and `x`. | Every application argument to `cat!` is parenthesised. |
| `03-impl-for-record-alias.forge` | both | `impl Show for Job`, where `Job` is a record type, is accepted, but the instance can never be used ("No instance"). | Jobs are ordered with key functions instead of `impl Ord`. |
| `04-no-char-code.forge` | both | There is no `char_code` and no string-to-int builtin. | `Fmt.digit_value` and `Fmt.parse_int`. |
| `05-local-let-in-constrained-fn-native.forge` | native only | Native fails ("specialised program does not type-check") when an `Ord`-constrained call sits in a local `let ... in`. | None needed. |
| `06-record-update-method-call-in-let.forge` | both | `let m2 = { m with n = lookup r m.env } in ...` fails with `eval_c_expr: EApp` in the interpreter and "dictionary used as a value" (or "unbound `__forge_method_Ord_compare`") natively. | In `Exec.execute`, the pushed value gets its own `let`. |
| `07-native-segfault-local-rec-closure.forge` | native only | Segfault when a local `let rec` loop captures a key function that is called inside a fold closure, and then reads a field of a record with 4 or more fields. | `Sched.np_loop` is a top-level function. |

Also observed: documented, so not filed as bugs.

- `fn` cannot be the right operand of an operator, so monadic chains need
  parentheses: `m >>= (fn x -> ...)`.
- `Show` has no instance for 4-tuples.
- The error text for a partial type application in an `impl` head says
  `Option<int>`, in lower case.
- The higher-kinded `Functor` cannot be implemented for a partially applied
  `Map<k>`, which is why the map is a tree of pairs.
