export type PlaygroundExample = {
  id: string
  label: string
  /** Short blurb for the option title attribute */
  description: string
  code: string
}

export const PLAYGROUND_EXAMPLES: PlaygroundExample[] = [
  {
    id: 'quick',
    label: 'Quick arithmetic',
    description: 'A tiny expression with let-in.',
    code: 'let x = 40 in x + 2',
  },
  {
    id: 'lists',
    label: 'Lists & recursion',
    description: 'Sum a list with pattern matching on [] and ::.',
    code: `let rec sum lst =
  case lst do
  | [] -> 0
  | h :: t -> h + sum t

let _ = println (sum [3, 10, 5, 20, 40])`,
  },
  {
    id: 'option',
    label: 'Option variants',
    description: 'case on None / Some from the prelude.',
    code: `let pick a b use_first =
  case use_first do
  | true -> Some a
  | false -> Some b

let unwrap_or default o =
  case o do
  | None -> default
  | Some v -> v

let _ = println (unwrap_or 0 (pick 7 99 false))`,
  },
  {
    id: 'poly',
    label: 'Polymorphism',
    description: 'Generic identity and applying a function twice.',
    code: `let id x = x

let twice f x = f (f x)

let _ = println (twice (fn x -> x + 1) 5)`,
  },
  {
    id: 'tree',
    label: 'ADTs & nested patterns',
    description: 'Recursive tree type and Node (l, r) matching.',
    code: `type rec Tree = | Leaf of Int | Node of (Tree, Tree)

let rec sum_tree t =
  case t do
  | Leaf n -> n
  | Node (a, b) -> sum_tree a + sum_tree b

let _ = println (sum_tree (Node (Leaf 1, Node (Leaf 2, Leaf 3))))`,
  },
  {
    id: 'traits',
    label: 'Traits (Semigroup for Bool)',
    description: 'impl uses interfaces from the embedded prelude.',
    code: `impl Semigroup for Bool where
  let mappend x y = x
  let (++) x y = mappend x y
end

let _ = println (true ++ false)`,
  },
  {
    id: 'ordering',
    label: 'Ordering patterns',
    description: 'case on LT | EQ | GT from the prelude.',
    code: `let sign_name x =
  case compare x 0 do
  | LT -> "negative"
  | EQ -> "zero"
  | GT -> "positive"

let _ = println (sign_name (0 - 7))`,
  },
  {
    id: 'factorial',
    label: 'Classic recursion',
    description: 'Factorial and a bit of arithmetic.',
    code: `let rec factorial n =
  if n == 0 then 1 else n * factorial (n - 1)

let _ = println (factorial 6)`,
  },
]

export const DEFAULT_PLAYGROUND_EXAMPLE_ID = PLAYGROUND_EXAMPLES[0].id

export function findPlaygroundExample(id: string): PlaygroundExample | undefined {
  return PLAYGROUND_EXAMPLES.find((e) => e.id === id)
}
