# LambdaScript to JavaScript Compiler (Minimal)

## What This Is

The absolute bare minimum infrastructure to compile LambdaScript to JavaScript.

## What Works

- Integer literals: `42`
- Variables: `x`
- Addition: `x + y`
- Function calls: `println(int_to_str(x))`
- Variable bindings: `let x = expr`
- Side effects: `let () = expr`

That's it. Everything else throws `failwith "TODO: Not implemented"`.

## Usage

```bash
dune exec ./bin/js_compiler.exe input.ls output.js
node output.js
```

## Example

**Input** (`minimal.ls`):
```lambdascript
let x = 1 + 2
let () = println (int_to_str x)
```

**Output** (`minimal.js`):
```javascript
function println(str) { console.log(str); }
function int_to_str(n) { return String(n); }

const x = (1 + 2);
println(int_to_str(x));
```

**Run**:
```bash
$ node minimal.js
3
```

## Files

- `src/js_codegen.ml` - 30 lines of code generation
- `bin/js_compiler.ml` - 20 lines of compiler driver

## Extending

Add cases to `gen_expr` and `gen_defn` in `js_codegen.ml`.
