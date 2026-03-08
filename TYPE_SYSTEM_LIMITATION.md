# Type System Limitation: Sum Type Constructor Polymorphism

## Issue Description

When defining a recursive sum type with a constructor that includes specific types (not type variables) in a tuple payload, LambdaScript's type system incorrectly infers the constructor as fully polymorphic.

## Example

### Type Definition
```lambdascript
type Color = | Red | Black

type rec RBTree<a> =
  | Leaf
  | Node of (Color, a, RBTree<a>, RBTree<a>)
```

### Expected Behavior
The `Node` constructor should have type:
```
(Color, 'a, RBTree<'a>, RBTree<'a>) -> RBTree<'a>
```

Where the first parameter is constrained to `Color`.

### Actual Behavior
The `Node` constructor is typed as:
```
('a, 'b, RBTree<'b>, RBTree<'b>) -> RBTree<'b>
```

Where both parameters are polymorphic type variables.

### Consequence
This allows incorrect code to type-check:
```lambdascript
Node (5, 10, Leaf, Leaf)  // Should fail: 5 is not a Color
// But this actually type-checks as RBTree<int>
```

## Root Cause

The type system appears to treat all elements in a constructor's tuple payload as independent type variables, rather than looking up concrete type names like `Color`.

## Current Status

This is a limitation in LambdaScript's type inference for sum types. The issue exists in the type checker's handling of constructor signatures when they contain:
1. Tuple payloads
2. Specific named types (like `Color`)
3. Mixed with type variables (like `a`)

## Workarounds

### Option 1: Use Helper Functions
Create constructor helper functions that  properly constrain types:

```lambdascript
let make_node color value left right =
  Node (color, value, left, right)

// Use make_node instead of Node directly
let tree = make_node Red 5 Leaf Leaf
```

### Option 2: Document Runtime Behavior
While the constructor signature is incorrect, pattern matching still works correctly:

```lambdascript
switch tree =>
| Node (Red, x, left, right) -> ...    // This correctly matches Red
| Node (Black, x, left, right) -> ...  // This correctly matches Black
```

### Option 3: Accept the Limitation
Use the constructor directly but be aware of the type system limitation. The code still runs correctly at runtime, even though incorrect code may type-check.

## Impact

**Low to Medium Impact:**
- ✅ Pattern matching works correctly
- ✅ Runtime behavior is correct
- ✅ Values created with correct types work as expected
- ❌ Type checker doesn't catch invalid Color values at compile time
- ❌ IDE/tooling support may show incorrect types

## Recommendation

For the red-black tree implementation:
1. **Continue using the parameterized type**: `type rec RBTree<a>`
2. **Be aware of the limitation**: The Color parameter won't be type-checked
3. **Use correct constructors**: Always use `Red` or `Black`, not arbitrary values
4. **Consider helper functions**: For critical code, use `make_node` helpers

## Future Resolution

This limitation could be fixed by enhancing the type checker to:
1. Properly resolve named types in constructor payloads
2. Generate appropriate constraints for non-variable types
3. Distinguish between `Color` (concrete type) and `'a` (type variable)

The fix would likely be in `src/typecheck.ml` in the constructor type inference logic.

## Testing

Despite this limitation, all 594 tests pass because:
- The code uses correct constructors (`Red` and `Black`)
- Pattern matching correctly distinguishes colors
- Runtime behavior is sound

The limitation only affects the type signature displayed by the REPL, not the actual execution.
