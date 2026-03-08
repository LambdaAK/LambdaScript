
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

const add = (function(x) { return (function(y) { return (x + y); }); });

const result = ((add(10))(20));

(println((int_to_str(result))));

let factorial;
factorial = (function(n) { return ((n === 0) ? 1 : (n * (factorial((n - 1))))); });

const fact5 = (factorial(5));

(println((int_to_str(fact5))));
