
// Forge Runtime Helpers

// Cons cell representation for lists
class Cons {
  constructor(head, tail) {
    this.head = head;
    this.tail = tail;
  }
}

const nil = { type: 'nil' };

// Pattern matching helper
class PatternMatchFailure extends Error {
  constructor(value) {
    super(`Pattern match failure on value: ${JSON.stringify(value)}`);
    this.value = value;
  }
}

// Variant/Constructor representation
class Variant {
  constructor(tag, payload = null) {
    this.tag = tag;
    this.payload = payload;
  }
}

// List operations
function listToArray(lst) {
  const result = [];
  let current = lst;
  while (current instanceof Cons) {
    result.push(current.head);
    current = current.tail;
  }
  return result;
}

function arrayToList(arr) {
  let result = nil;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = new Cons(arr[i], result);
  }
  return result;
}

// List enumeration [start...end]
function listEnum(start, end) {
  const result = [];
  for (let i = start; i <= end; i++) {
    result.push(i);
  }
  return arrayToList(result);
}

// List comprehension helper
function listComprehension(expr, generators) {
  // generators is an array of { pattern, source }
  // This will be implemented per-comprehension in generated code
  throw new Error("List comprehension helper not yet implemented");
}

// Built-in functions
function println(str) {
  console.log(str);
  return undefined;
}

function print(str) {
  process.stdout.write(str);
  return undefined;
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

function string_to_list(s) {
  return arrayToList(s.split(''));
}

function list_of_string(s) {
  return string_to_list(s);
}

// Equality check (deep for structures)
function ls_equals(a, b) {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  
  // Check Cons
  if (a instanceof Cons && b instanceof Cons) {
    return ls_equals(a.head, b.head) && ls_equals(a.tail, b.tail);
  }
  
  // Check nil
  if (a?.type === 'nil' && b?.type === 'nil') return true;
  
  // Check Variant
  if (a instanceof Variant && b instanceof Variant) {
    if (a.tag !== b.tag) return false;
    return ls_equals(a.payload, b.payload);
  }
  
  // Check arrays (tuples/vectors)
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!ls_equals(a[i], b[i])) return false;
    }
    return true;
  }
  
  // Check objects (records)
  if (typeof a === 'object' && typeof b === 'object' && a !== null && b !== null) {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();
    if (keysA.length !== keysB.length) return false;
    for (let i = 0; i < keysA.length; i++) {
      if (keysA[i] !== keysB[i]) return false;
      if (!ls_equals(a[keysA[i]], b[keysB[i]])) return false;
    }
    return true;
  }
  
  return false;
}

function ls_not_equals(a, b) {
  return !ls_equals(a, b);
}


// Generated code

let factorial;
factorial = (function(n) { return (ls_equals(n, 0) ? 1 : (n * (factorial((n - 1))))); });

const result = (factorial(5));

(println((int_to_str(result))));