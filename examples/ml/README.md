# Machine learning in Forge

A small machine-learning library written in Forge, and four experiments that use it.

| File | Contents |
|---|---|
| `linalg.forge` | `Vec` (vector operations), a `Matrix` type with `Mat` (products, transpose, identity, column statistics), and `Solve` (Gaussian elimination with partial pivoting) |
| `models.forge` | `Adam` (optimizer), `Scale` (feature standardisation), `Linear` (least squares: closed form or Adam), `Logistic` (logistic regression, Adam), `Kernel` (linear, polynomial, RBF; Gram matrices), `KernelRidge`, `KernelLogistic`, `Metrics` |
| `datasets.forge` | Seeded synthetic datasets: house prices, two Gaussian blobs, XOR, a noisy cubic |
| `util.forge` | A seeded random number generator and number formatting |
| `main.forge` | The experiments; `main.out` is their output |

Run it with the interpreter (about 25 s) or natively (under 2 s):

```bash
dune exec ./bin/interpreter.exe examples/ml/main.forge
dune exec ./bin/compile_forge.exe examples/ml/main.forge ml && ./ml
```

Both back ends print exactly `main.out` (`test/cli_tests.ml` checks it):

- **Linear regression** on 150 houses recovers the generating weights up to noise (test R² 0.9935). Adam on standardised features converges to the same weights as the normal equations.
- **Logistic regression** separates two Gaussian blobs: 97.0% test accuracy.
- **Kernels on XOR:** a linear classifier is at chance (47.5%); kernel logistic regression with an RBF or a degree-2 polynomial kernel reaches 97.5%.
- **Kernel ridge regression** on y = x³ − 2x + noise: test R² goes from 0.057 (linear) to 0.978 (RBF or cubic kernel). The test RMSE of 0.183 is at the noise level (0.2).
