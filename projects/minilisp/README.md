# minilisp: a Lisp interpreter and a type inferencer in Forge

A Forge stress test made of two programs:

1. **A Scheme-like Lisp.** It has a tokenizer (numbers, symbols, strings with escapes, `#t`/`#f`,
   parentheses, `'` and `;` comments), a reader that builds s-expressions, a printer whose output
   reads back in, and an evaluator with lexical environments and closures. The special forms are
   `define`, `lambda` (fixed, `(a . rest)` and `args` parameter lists), `let`, `let*`, `if`, `cond`,
   `and`, `or`, `quote` and `begin`. A body can start with internal `define`s, which may recurse.
   There are 38 primitives: variadic `+ - * /`, `mod`, chained comparisons, the list primitives
   `cons car cdr list length append null? pair?`, string primitives, `equal?`/`eq?`/`compare`,
   `apply` and `error`. `map`, `filter`, `foldl`, `foldr`, `reverse`, `range`, `assoc` and
   `compose` are defined in Lisp, in a prelude. Forge has no mutation, so there is no `set!`: the
   global environment is a persistent binary search tree that is passed from one top-level form to
   the next. Every failure is a `Res` value rather than a crash: unbound symbols, wrong arity,
   type errors, division by zero, non-procedures, bad syntax, read errors and user `error`s.
   The suite has 13 programs: factorial, Fibonacci, closures and accumulators (a counter and a
   message-passing bank account that is rebuilt on every change), higher-order functions,
   quicksort and a generic sort, an association-list database, mutual recursion with recursion
   10000 deep, strings, special forms, 16 error cases and 3 read errors.
2. **Hindley–Milner type inference** for a small ML with ints, bools, functions, `let`
   polymorphism, `let rec`, `if`, pairs and lists. The parser is built from combinators over a
   user-defined `Parser` monad, with `Alternative` for backtracking and furthest-failure error
   messages. Unification has an occurs check and uses a triangular substitution. The inference
   runs in a state-and-error monad (`Infer`) that carries the fresh-variable counter and the
   substitution, and it generalises at `let`. Types are printed with the letters a, b, c. The
   program runs 26 expressions: 15 are well typed; 8 are type errors (mismatch, infinite type,
   unbound variable, lambda-bound monomorphism and others), each with a message that names the
   expression where it failed; 3 are parse errors.

## Files

| file | contents |
|---|---|
| `util.forge` | `Chars`, `Text` (join, `parse_int`, `of_chars`), `Map` (string-keyed BST), generic `mapM`/`foldM` for any `Monad` |
| `sexpr.forge` | `Value`/`Env` (mutually recursive), `LispError`, the `Res` monad, `impl Show`/`Eq`/`Ord` for `Value` |
| `reader.forge` | `Reader`: tokenizer and reader |
| `builtins.forge` | `Builtins`: the primitives, the `expect!` macro |
| `eval.forge` | `Eval`: the monadic evaluator, closures, `apply`, top-level `define` |
| `lisp.forge` | `Lisp`: the Lisp prelude, the runner, the `programs!` macro and the test suite |
| `types.forge` | `ML.Expr`, `Types.Ty`/`Scheme`/`Subst`, free variables, the type printer |
| `mlparse.forge` | `MLLex` lexer, the `Parser` monad + `Alternative`, the `MLParse` grammar |
| `infer.forge` | the `Infer` monad, `HM`: unification, instantiation, generalisation, inference |
| `main.forge` | runs both parts |
| `main.out` | the expected output (both back ends) |
| `bugs/` | minimal programs for the problems found (see below) |

## Running

From the repository root:

```sh
./_build/default/bin/interpreter.exe projects/minilisp/main.forge

# the native compiler writes .mir/.ll/.s next to its input, so compile a copy
T=$(mktemp -d); cp projects/minilisp/*.forge $T/
./_build/default/bin/compile_forge.exe $T/main.forge $T/main.exe && $T/main.exe
```

Both print exactly `main.out` (377 lines), with nothing on stderr and exit status 0.

Run times on the development machine (Apple Silicon, macOS):

| | time |
|---|---|
| interpreter | 7.5 s |
| native compile | 1.3 s |
| native run | 0.16 s |

Most of the interpreter's time goes to the Lisp quicksort on sorted input, a quadratic
benchmark: about 0.3 ms for each `filter` step of the Lisp program. The native build is about
45 times faster.

## Forge features used

- recursive and mutually recursive sum types (`type rec Value ... and Env`, `Ty`, `Expr`), an
  anonymous record as a constructor payload, record update `{ c with name = ... }`, record
  field access, a named record type for the inference state
- deep nested patterns (`List (Sym "define" :: List (Sym f :: params) :: body) :: rest`), string,
  char and tuple patterns, refutable comprehension generators (`Typed t <- verdicts`)
- traits: user `impl Show` for `Value`, `LispError`, `Res<a>` (with `requires Show<a>`), `Ty`
  and `Expr`; `impl Eq` and `impl Ord` for `Value`, with `compare` and `Ordering`
- higher-kinded traits: `Functor`/`Applicative`/`Monad` for `Res`, `Parser` and `Infer`, and
  `Alternative` for `Parser`; the evaluator, the reader, the parser and the inferencer are written
  with `>>=`; `mapM`/`foldM` are constrained over any `Monad` and used with `Res` and `Infer`
- modules (`mod ... where ... end`, nested qualified names, `use` inside a module and at top
  level), `import` across nine files
- `macro_rules!`: `expect!` takes a `pat` fragment and builds a `case`; `programs!` uses
  repetition with `literal` and `expr` fragments
- closures and currying, `let rec ... and ...` (the evaluator, the reader and the grammar),
  pipelines `|>`, blocks with statements, `let ... in`, `Option`, comprehensions, ranges, and
  recursion several thousand levels deep in both back ends

## Problems found (`bugs/`)

**Status: all fixed.** Every problem below has been fixed in Forge since; each
file in `bugs/` now runs correctly in both back ends (the comments in the
files keep the original report), and the workarounds described here are no
longer needed.

1. `01-rec-record-type.forge`: a named record type cannot be part of `type rec ... and ...`,
   and neither can a self-referential record type (syntax error). Workaround: an anonymous record
   as the constructor payload.
2. `02-functor-partial-application.forge`: `impl Functor for Result<e>` is rejected, so
   there is no `Either`-style monad over an arbitrary error type. The message also has no
   location. Workaround: the error type is fixed in `Res<a>`.
3. `03-type-error-no-location.forge`: a type error has no file or line, and the message
   prints the whole desugared definition.
4. `04-native-constrained-statement.forge`: **native only.** A constrained polymorphic
   function that calls a constrained function in statement position (`{ report x; ... }`) fails
   with "the specialised program does not type-check". The interpreter runs it. Workaround:
   type annotations on `Lisp.print_outcome`.
5. `05-qualified-constructor-pattern.forge`: `M.C x` works in expressions but is a syntax
   error in patterns. Workaround: `use ML`/`use Types`, and `Verdict` is declared at top level.
6. `06-missing-char-conversions.forge`: there is no char↔int conversion, no string→int and
   no `List<Char>`→`String`. All three are written by hand in `util.forge`.

Smaller friction, all documented: `fn` cannot be the operand of `>>=` without parentheses, so
monadic code is full of `(fn x -> ...)`. A `case` nested in an arm needs parentheses. There is
no expression type ascription `(e : T)`. `Bool` cannot be a constructor name because it is a
keyword.
