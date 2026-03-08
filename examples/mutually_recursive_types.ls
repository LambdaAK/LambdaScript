(* Mutually Recursive Types Examples *)

(* Example 1: Even and Odd natural numbers *)
type rec Even = | Zero | SuccE of Odd
and Odd = | SuccO of Even

let rec even_to_int = fn e ->
  case e do
  | Zero -> 0
  | SuccE o -> 1 + odd_to_int o
and odd_to_int = fn o ->
  case o do
  | SuccO e -> 1 + even_to_int e

let four = SuccE (SuccO (SuccE (SuccO Zero)))
let result1 = even_to_int four  (* Should be 4 *)

(* Example 2: Tree and Forest *)
type rec Tree<a> = | Leaf of a | Node of (a, Forest<a>)
and Forest<a> = | Empty | Trees of (Tree<a>, Forest<a>)

let rec sum_tree = fn t ->
  case t do
  | Leaf x -> x
  | Node (x, f) -> x + sum_forest f
and sum_forest = fn f ->
  case f do
  | Empty -> 0
  | Trees (t, rest) -> sum_tree t + sum_forest rest

let tree = Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty)))
let result2 = sum_tree tree  (* Should be 6 *)

(* Example 3: Circular type references *)
type rec A = | AVal of int | ToB of B
and B = | BVal of int | ToC of C
and C = | CVal of int | ToA of A

let rec get_value = fn a ->
  case a do
  | AVal x -> x
  | ToB b ->
    case b do
    | BVal y -> y
    | ToC c ->
      case c do
      | CVal z -> z
      | ToA a2 -> get_value a2

let result3 = get_value (ToB (ToC (ToA (AVal 42))))  (* Should be 42 *)

(* Example 4: Rose trees *)
type rec Rose<a> = | RNode of (a, RoseList<a>)
and RoseList<a> = | RNil | RCons of (Rose<a>, RoseList<a>)

let rec count_rose = fn r ->
  case r do
  | RNode (_, children) -> 1 + count_list children
and count_list = fn rl ->
  case rl do
  | RNil -> 0
  | RCons (r, rest) -> count_rose r + count_list rest

let rose = RNode (1, RCons (RNode (2, RNil), RCons (RNode (3, RNil), RNil)))
let result4 = count_rose rose  (* Should be 3 *)
