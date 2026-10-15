# tinyc: a compiler for a small imperative language, in Forge

`tinyc` compiles **Tiny**, a small C-like language, to bytecode for a stack
machine and runs it. It also runs every program a second way, with a direct
AST interpreter, and checks that the two print the same thing. The whole
program is written in Forge (about 1550 lines in 11 files) and is meant to
stress-test the language.

## The Tiny language

```
fn gcd(a: int, b: int): int {
  if (b == 0) { return a; }
  return gcd(b, a % b);
}
var i: int = 1;
while (i <= 10 && !(i == 7)) { print gcd(i, 12); i = i + 1; }
```

- Types: `int`, `bool`, and `void` for function results.
- Declarations `var x: T = e;`, assignment `x = e;`, blocks with lexical
  scope, `if (c) {..} else {..}` (also `else if`), `while (c) {..}`,
  `return e;` / `return;`, `print e;`, and expression statements `f(x);`.
- Functions `fn f(a: int, b: bool): int { ... }`, declared before the main
  statements; they can call each other and themselves. A function sees only
  its parameters and its own locals.
- Operators, loosest first: `||`, `&&`, `== !=`, `< <= > >=`, `+ -`,
  `* / %`, unary `- !`. Binary operators are left-associative; `&&` and `||`
  short-circuit. `//` comments.

## The pipeline

| Stage | File | What it does |
|---|---|---|
| lexer | `lexer.forge` | characters to tokens, each with its line number |
| parser | `parser.forge` | recursive descent plus precedence climbing, written in monadic style over a `Parser<a>` type that implements Functor/Applicative/Monad |
| AST | `ast.forge` | mutually recursive `Stmt`/`Block` types, a pretty printer (`impl Show`), and a `Measure` trait with a default method |
| type checker | `typecheck.forge` | int against bool, undeclared variables and functions, call arity and argument types, return types, missing returns, redeclarations, void misuse. It runs in the `Res` monad, and its messages give the line |
| optimiser | `optimize.forge` | constant folding, algebraic identities (`x*1`, `x+0`, `x*0` when pure, `x-x`, `--x`, `!!x`, `!(a<b)` becomes `a>=b`), short-circuit simplification, dead branches for constant conditions, removing `while(false)`, dead code after `return`, block splicing |
| code generator | `codegen.forge` | AST to bytecode with symbolic labels, a peephole pass (unreachable code after a jump or return, jumps to the next instruction), an assembler that resolves labels to addresses, and a disassembler |
| VM | `vm.forge` | a stack machine with an instruction counter, call frames and locals. Code is stored in an AVL map, so fetching an instruction is O(log n) |
| interpreter | `interp.forge` | a direct AST interpreter in a state-and-error monad `Eval<a>`, so output printed before a run-time error is kept |
| driver | `main.forge`, `samples.forge` | 9 sample programs and 16 programs that should not compile |

Bytecode: `push n`, `load s`, `store s`, `pop`, `add sub mul div mod neg not`,
`eq ne lt le gt ge`, `jump a`, `jumpz a`, `call a (n args)`, `ret`, `retv`,
`print.i`, `print.b`, `halt`.

Each good program runs **four ways**: the interpreter and the VM, each before
and after optimisation. All four must give the same output and the same
run-time error, if there is one. The report shows the code size and the
number of executed VM instructions before and after optimisation. It also
gives the full bytecode listing for two programs (factorial, and the
optimiser showcase, where code size goes from 98 to 34 instructions and
executed instructions from 86 to 36).

## Running it

From the repository root:

```
./_build/default/bin/interpreter.exe projects/tinyc/main.forge

# native: the compiler writes .mir/.ll/.s next to the source, so use a copy
d=$(mktemp -d); cp projects/tinyc/*.forge $d/
./_build/default/bin/compile_forge.exe $d/main.forge $d/main.exe && $d/main.exe
```

Both print exactly `main.out`, with nothing on stderr and exit status 0.

Run times on an Apple Silicon Mac:

| | time |
|---|---|
| interpreter | about 10.3 s |
| native compile | about 1.7 s |
| native run | about 0.11 s |

Most of the interpreter's time goes to the VM: about 500,000 bytecode
instructions in all, at roughly 20 µs each.

## Forge features used

- Recursive sum types (`Expr`, `Map.T`) and mutually recursive ones
  (`type rec Stmt ... and Block`). Constructors with 4- and 5-tuple payloads.
- Records: named record types, field access, record patterns, and record
  update `{ g with code = ..., next_slot = ... }` (the VM updates its state
  record this way on every instruction).
- Deep nested patterns, such as `(Some (ICall (a, n)), s)` and
  `Node (Node (ll, lk, lv, lr, _), k, v, r, _)`. String and char literal
  patterns, and refutable patterns in comprehension generators
  (`[x | SVar (x, _, _, _) <- ss]`).
- Traits: `impl Show` for `Ty`, `Expr`, `Stmt`, `Tok`, `Instr` and `Value`,
  and a user trait `Measure` with a default method, overridden for `Block`.
- Higher-kinded traits: three user types implement the prelude's
  Functor/Applicative/Monad. They are `Res` (a result type), `Parser` (which
  wraps a function) and `Eval` (state plus error). The parser, the type
  checker and the interpreter are written with `>>=`.
- A recursive `macro_rules!` macro, `mdo!(x <- m; y <- n; e)`, that gives
  do-notation over any monad.
- Modules (`mod ... where ... end`), `use`, qualified names, and `import`
  across 11 files.
- Comprehensions, closures, currying and partial application
  (`reduce_left (gen_expr env) g args`), `let rec ... and ...`, pipelines
  `|>`, blocks, `Option`, and long tail-recursive loops (the VM's `run`).

## Forge problems found (see `bugs/`)

**Status: all fixed.** Every problem below has been fixed in Forge since; each
file in `bugs/` now runs correctly in both back ends (the comments in the
files keep the original report), and the workarounds described here are no
longer needed.

1. `01-impl-for-record-alias.forge`: `impl Show for P`, where `P` is a named
   record type, is accepted, but every use fails with *No instance*. This
   happens in both back ends.
2. `02-annotated-pattern-in-case.forge`: an annotated pattern
   `Some (x : Int)` in a `case` arm is a syntax error, though the manual says
   patterns can be annotated.
3. `03-constrained-let-under-annotation.forge`: this one breaks both back
   ends. A function constrained by `Ord` is called inside `let ... in`, in a
   function whose parameter has a type annotation. The interpreter then
   crashes with `eval_c_expr: EApp`, and the native compiler rejects the
   program with "a type-class dictionary used as a value".
4. `04-slow-match-on-prelude-types.forge`: in the interpreter, building and
   matching the prelude's `Option` and `Ordering` is about 4x slower than an
   identical user-defined type.
5. `05-no-char-to-int.forge`: a missing feature. Forge has no way to turn a
   character into a number, or a string into an Int.

Workarounds are marked in the source. Func is printed by `show_func`. The
interpreter does not use annotated case patterns. The map orders its keys
with a local `Cmp` type built on the structural `<`, not with `compare`. The
VM's hot path avoids `Option`. Digits are decoded with a `case`.
