# learn — machine learning from scratch in Forge

A multi-layer perceptron with backpropagation, k-means clustering and a CART
decision tree, written in plain Forge (lists of floats, no arrays, no
built-in trigonometry), trained on seeded synthetic data. Every number in the
output is printed with a fixed-decimal formatter built from integers, so the
interpreter and the native compiler print exactly the same text
(`main.out`).

| File | Contents |
|---|---|
| `rng.forge` | `Rng`: a linear congruential generator modulo 2^31 (products stay below 2^62). `Rand<a>`: a state monad over the seed, with `Functor`, `Applicative` and `Monad` instances; `Gen`: uniform, integer, normal (sum of 12 uniforms), `replicate`, `traverse` |
| `util.forge` | `Fmt` (fixed decimals, padding, bars), `Lists` (zip, take/drop, chunks, argmax, ...), `Sort` (stable merge sort), `Lines` |
| `matrix.forge` | `Math` (π, **sin/cos by Taylor series** with range reduction, tanh, sigmoid), `Vec`, `Mat` (W x, Wᵀ d, outer products, column statistics) |
| `data.forge` | `Sample`/`Dataset` records; concentric rings, two interleaved spirals, three Gaussian blobs; shuffling and train/test split |
| `metrics.forge` | The `Classifier` trait (`predict`, plus default `describe`, `accuracy`, `confusion`), the `Confusion` type with a `Show` instance |
| `mlp.forge` | `Act` (tanh / ReLU / sigmoid / linear), dense layers with Glorot init, softmax + cross-entropy, recursive backpropagation, mini-batch SGD with momentum, training history |
| `kmeans.forge` | k-means++ seeding, Lloyd iterations with an inertia history, cluster sizes; `Centroids`, k-means as a classifier |
| `tree.forge` | `Tree<a>` with `Functor`, `Foldable` and `Show` instances; CART growth by Gini impurity (sorted sweep per feature), max depth, prediction |
| `main.forge` | The experiments; `main.out` is their output |
| `bugs/` | Minimal reproductions of the Forge problems found while writing this |

## Running it

From the repository root:

```bash
# interpreter (about 45 s)
./_build/default/bin/interpreter.exe projects/learn/main.forge

# native: the compiler writes .mir/.ll/.s next to the source, so compile a copy
D=$(mktemp -d); cp projects/learn/*.forge $D/
./_build/default/bin/compile_forge.exe $D/main.forge $D/main.exe && $D/main.exe
```

Measured on an Apple M3 Max: interpreter 44.8 s, native compile 0.9 s and
run 0.25 s (about 180x faster). Both print exactly `main.out`, with nothing
on stderr and exit status 0.

## Results

- **Building blocks.** The Taylor-series `sin`/`cos` (14 terms after range
  reduction to [-π, π]) give sin(π/6) = cos(π/3) = 0.500000000000 and
  |sin² + cos² − 1| < 1e-12 at 421 angles in [-30, 30]. 2000 "normal" draws
  have mean 0.023 and standard deviation 1.021.
- **Rings** (2 classes, 160 train / 40 test): without a hidden layer
  (softmax regression) the network stays at chance (35% test); with one
  hidden layer of 8 tanh units it reaches 100% train / 97.5% test within
  ~10 epochs.
- **Spirals** (2 classes, 150 / 50): one hidden layer of 16 tanh units gets
  stuck around 65% train / 74% test after 60 epochs; two hidden layers
  (12 ReLU, 8 tanh) reach 100% train and test, the loss dropping from 0.69 to
  0.0045.
- **Blobs** (3 classes, 180 / 60): a 2-6-3 sigmoid network gets 94.4% / 95.0%;
  the confusion matrix shows that the errors are the points of class 2 that
  overlap the other blobs.
- **k-means** (k = 3, labels ignored) converges in 4 iterations (inertia
  377.98 → 368.01) to clusters of sizes 80/81/79 whose centres are close to
  the true ones; purity 95%. The elbow table (k = 1..6) shows the bend at
  k = 3.
- **CART**: the printed depth-3 tree for the blobs gets 96.7% test. Test
  accuracy by depth shows that axis-aligned splits need depth ~4 for the
  rings and ~10 for the spirals (100% at depth 10, 88% at depth 6).
- **Comparison** (tree depth 6): the MLP wins on rings (97.5% vs 95.0%) and
  clearly on spirals (100% vs 88%); the tree wins slightly on the blobs
  (96.7% vs 95.0%).

## Forge features exercised

- Sum types (`Act`, `Tree<a>`, `Rand<a>`, `Confusion`, `Net`, `Cart`,
  `Centroids`), a recursive parameterised type (`type rec Tree<a>`), records
  with record update (`{ l with w = ..., b = ... }`, `{ ring_cfg with epochs = 30 }`,
  `{ d with samples = ... }`), record patterns and field access, type
  annotations on parameters.
- Nested pattern matching on tuples, lists, options and constructors
  (`(c :: rest, Some (bi, bd))`), refutable patterns in comprehension
  generators (`Some (score, thr) <- [...]`), char patterns (`'\n' :: rest`).
- Traits: a user trait with three default methods (`Classifier`) and
  instances that override some (`Net`, `Cart`) or none (`Centroids`); a
  constrained function with an explicit constraint (`compare_on [Classifier m]`);
  user `impl Show` (`Act`, `Confusion`, `Tree<a> requires Show<a>`).
- Higher-kinded traits: `Functor`/`Applicative`/`Monad` for the `Rand` state
  monad (all data generation and k-means++ seeding use `>>=`, `fmap`,
  `pure`), `Functor` and `Foldable` for `Tree` (the tree is grown with class
  counts in its leaves, then `fmap`ped to classes or to printable labels;
  leaves are counted with `fold_left`).
- Modules (`mod ... where ... end`, a module and a type both named `Tree`),
  `use Fmt`, imports across nine files (with shared imports included once).
- `macro_rules!` with repetition (`row!(10 => a, 12 => b, ...)`) and a macro
  expanding to a block (`section!`).
- Comprehensions (with several generators and guards), ranges, closures and
  currying (`Lists.update_at s.y (Lists.update_at p (fn c -> c + 1))`),
  operator sections (`(+.)`), pipelines, blocks with local definitions,
  `let rec ... and ...`, local recursive functions, deep non-tail
  recursion (monadic `replicate` of 2000 normals, list recursion everywhere).

## Forge problems found

Each has a minimal reproduction with expected and actual behaviour in
`bugs/`:

1. `bugs/01-alias-in-trait-signature.forge` — a named record type used in a
   trait method signature is not expanded, so `{ x: 2 }` is rejected against
   `P = { x: Int }` (both back ends). Worked around in `metrics.forge` by
   writing the record type out.
2. `bugs/02-interpreter-runs-before-typecheck.forge` — the interpreter checks
   and runs one top-level item at a time, so output is produced before a type
   error further down is reported (the manual says programs only run once
   all types check; the native compiler rejects the whole program).
3. `bugs/03-let-in-recursive-impl-method.forge` — in an instance method, a
   local `let ... in` breaks a recursive call of the same method
   (`cannot match a with μTree. Tree<a>`). Avoided in `tree.forge` by using a
   local recursive printer.
4. `bugs/04-native-let-in-constrained-recursion.forge` — native only: a
   constrained recursive function over a user recursive type with a local
   `let` in a case arm makes specialisation fail ("the specialised program
   does not type-check ... cannot match Int with Unit"); the interpreter runs
   it. Worked around in `tree.forge` (the `Show` instance builds each line
   inline).
5. `bugs/05-interpreter-slow-float-ops.forge` — float arithmetic is ~8x
   slower than integer arithmetic in the interpreter (curried built-in
   application with string dispatch per operation); this is most of the
   interpreter's 45 s here.
