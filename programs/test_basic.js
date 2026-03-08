
// LambdaScript Runtime (Minimal)

function println(str) {
  console.log(str);
}

function print(str) {
  process.stdout.write(str);
}

function int_to_str(n) {
  return String(n);
}

function int_to_float(n) {
  return Number(n);
}

function float_to_int(f) {
  return Math.floor(f);
}

// TODO: Implement list operations, pattern matching, etc.


// Generated code

const x = 5;

const y = 10;

const z = (x + y);

(println((int_to_str(z))));
