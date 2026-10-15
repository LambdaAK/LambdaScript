# textkit: a command-line text toolkit in Forge

Forge programs can only print. They can't read stdin, files or arguments. So
textkit simulates a whole command line. An in-memory filesystem holds five
embedded files: a 20-row CSV, a server log, a poem, a small source file and a
word list. A small shell tokenizes command strings, parses them into
pipelines and runs Unix-style tools over the files. `main.forge` is a script
of 50 command lines. Each one is printed as `$ command` and followed by its
output, as in a terminal session.

```
$ cat server.log | grep -i error | cut -d ' ' -f 4- | sort | uniq -c
      3 database connection refused
      1 timeout calling payment service
      1 user 42 not found
```

## Running it

From the repository root:

```sh
# interpreter
./_build/default/bin/interpreter.exe projects/textkit/main.forge

# native: the compiler writes .mir/.ll/.s next to the source, so compile a copy
d=$(mktemp -d); cp projects/textkit/*.forge $d/
./_build/default/bin/compile_forge.exe $d/main.forge $d/main.exe && $d/main.exe
```

Both back ends print exactly `main.out` (259 lines). Stderr is empty and the
exit status is 0.

| | time (wall, M-series Mac) |
|---|---|
| interpreter | ~0.37 s |
| native compile | ~0.9 s |
| native run | < 0.01 s |

## What it does

- **Shell** (`shell.forge`)
  - Tokenizer: single and double quotes, `\` escapes, `|`, `>` and `>>`. It
    is three mutually recursive lexer states.
  - Parser: turns tokens into a pipeline of commands with an optional
    redirection.
  - Aliases: `ll`, `count`, `upper`.
  - Command table: a list of records, each with a function field.
  - Executor: threads each stage's output into the next stage's input. A
    redirection returns a new filesystem.
- **Tools** (`tools.forge`):
  - `cat [-n]`, `echo`, `ls [-l]`
  - `wc [-l -w -c]`, with per-file rows and a total row
  - `grep [-i -v -n -c]`, using regular expressions
  - `head`/`tail -n N`, plus `-N`, `head -n -k` and `tail -n +k`
  - `sort [-r -n -u -k N -t C]`
  - `uniq [-c -d]`
  - `cut -d C -f LIST`: the list takes fields and ranges, e.g. `1,3-4`, `4-`
  - `tr [-d] SET1 [SET2]`, with ranges and escapes
  - `rev`, `nl`
  - `sed 's/re/text/[g]'`: any delimiter, `&` stands for the match
  - `column -t -s C`
  - `table`: a boxed table with numeric columns right-aligned
  - `stats -f N`: count, sum, min, max, mean and standard deviation
  - `freq -n N`: word frequencies, kept in a binary search tree
  - `help`
- **Flags** are parsed generically from each tool's spec. Combined flags
  (`-lw`, `-in`), attached values (`-d,`, `-f1,3`) and `--` all work.
- **Regex engine** (`regex.forge`): a backtracking matcher that supports
  `.`, `[a-z]`, `[^x]`, `* + ?`, `^ $` and `\` escapes. Repetition is greedy,
  and `s///g` handles empty matches the way sed does.
- **Errors are values.** Each of these comes back as a `Res` error and is
  printed. None of them crashes the program:
  - unknown commands, with a "did you mean" suggestion based on Levenshtein
    distance
  - unknown flags, with the list of valid flags
  - missing flag values
  - bad numbers
  - missing files
  - bad field lists
  - unterminated quotes, empty pipeline stages, a dangling `>`
  - malformed `sed` scripts and regexes

## Files

| file | contents |
|---|---|
| `main.forge` | the script, plus a token and parse dump of one tricky line |
| `shell.forge` | `Lexer`, `Parser` and `Shell` (command table, aliases, executor) |
| `tools.forge` | the `flags!` macro, `Flags` (argument parsing), the `Tree` and `Bag` types for `freq`, `Tools` |
| `regex.forge` | `Regex`: parser and backtracking matcher |
| `vfs.forge` | `Vfs`: the embedded files, plus reading and writing them |
| `result.forge` | `Error`, `Res` (Functor, Applicative, Monad), the `Report` trait, the `ensure!` macro, `Result.traverse` |
| `strings.forge` | `Chars` (character classes and case, via tables) and `Str` (split, join, trim, pad, replace, parse_int, ...) |
| `lists.forge` | `Lists`: take, drop, zip, merge sort, runs, assoc, ... |
| `bugs/` | small reproductions of the problems found while writing this |

There are about 1,240 lines of Forge, not counting `bugs/`.

## Forge features exercised

- **Types:** sum types (`Error`, `Token`, `Atom`, `Item`) and recursive
  types (`type rec Tree`). Named records, including a record with a function
  field (`Tool`), record update `{ f with body = ... }` and record patterns
  (`{ name: "help" }`). Row polymorphism: `.name` is used on both `File` and
  `Tool`.
- **Pattern matching:** deeply nested patterns, char and string literal
  patterns, `a :: '-' :: b :: rest`, and tuple patterns in lambdas.
- **Traits:**
  - user `impl Show` for `Error`, `Token` and `Res<a>`, the last one with
    `requires Show<a>`
  - a user trait `Report` with `requires Show<a>` and a default method
  - an explicit constraint: `describe [Show a]`
- **Higher-kinded traits:**
  - Functor, Applicative and Monad for the user type `Res`, used throughout
    through `>>=` and `fmap`
  - Foldable for the user `Tree`
  - Option's `ap` (in `cut`'s field ranges) and `<|>` (alias lookup)
  - a monad-generic `Result.traverse` (using `pure` and `>>=`), used with
    both `Res` and `Option`
- **Modules and files:** nested modules, with `use` both at top level and
  inside modules. Imports across 8 files, with a diamond (`result.forge` is
  reached through both `vfs.forge` and `regex.forge`).
- **Macros:**
  - `macro_rules!` with several arms and `$(...),*` repetition (`flags!`)
  - a macro defined in one file and used in another (`flags!` is defined in
    `tools.forge` and used in `shell.forge`)
- **Functions:** `let rec ... and ...` (the lexer states, and regex
  `match_here`/`match_star`), closures returned as values (field predicates),
  currying and partial application.
- **Expressions:** comprehensions, with filters and with tuple patterns,
  `|>` pipelines, `{ ...; ... }` blocks with statements, and `[a...b]`
  ranges.
- **Other:** `Option`, `Ordering` and `compare` (merge sort, BST insert),
  and Float arithmetic with `sqrt` (in `stats`).

## Problems found (`bugs/`)

**Status: all fixed.** Every problem below has been fixed in Forge since; each
file in `bugs/` now runs correctly in both back ends (the comments in the
files keep the original report), and the workarounds described here are no
longer needed.

1. `01-no-char-code.forge`: there is no `char_to_int`/`int_to_char` and no
   string-to-int conversion. Character classes, case conversion, digit values
   and `tr` ranges all go through lookup tables in `strings.forge`.
2. `02-no-ord-for-tuples.forge`: `compare` has no `Ord` instance for tuples,
   even though `<` and `==` work on them. The error message also gives no
   location. Worked around in `Shell.suggest`.
3. `03-native-stack-overflow-map.forge`: in native code, `map` or
   `reduce_right` over a list of 200,000 elements segfaults (exit 139) with
   no message. The output printed before the crash is lost because stdout is
   not flushed. The interpreter handles 1,000,000 elements. textkit's lists
   are far smaller, so it is not affected.
4. `04-interpreter-runs-before-typecheck.forge`: the interpreter runs
   top-level effects before it has type checked the rest of the file, which
   contradicts the manual. The native compiler rejects the same program up
   front.
