
class Ex:
    input: str
    output: str

    def __init__(self, input: str, output: str):
        self.input = input
        self.output = output

    def __repr__(self):
        # formatted using Markdown

        return f"<br><br>\n_input_\n```ocaml\n{self.input}\n```\n_output_\n```ocaml\n{self.output}\n```\n<br><br>"


class Category:
    name: str
    examples: list[Ex]

    def __init__(self, name: str, examples: list[Ex]):
        self.name = name
        self.examples = examples

    def __repr__(self):
        return f'''## {self.name}\n{''.join(map(str, self.examples))}'''


basic = [
    ("17", "Int: 17"),
    ("true", "Bool: true"),
    ("hello world!", "String: hello world!"),
    ("()", "Unit: ()")
]

compound_types = [
    ("[]", "[a]: []"),
    ("1 :: 2 :: 3 :: 4 :: 5 :: []", "[Int]: [1, 2, 3, 4, 5]"),
    ("[1, 2, 3, 4, 5]", "[Int]: [1, 2, 3, 4, 5]"),
    ("(1, true)", "(Int, Bool): (1, true)"),
    ("\ x -> x", "a -> a: function"),
    ("\ x -> x + 1", "Int -> Int: function"),
]

let_expressions = [
    ('''let x = 1 in
let y = 2 in
x + y''', "Int: 3"),
    ("let f x y z = x (y + z) in f", "f : (Int -> a) -> Int -> Int -> a = function"),

]

ternary_expressions = [
    ("if true then 1 else 2", "Int: 1"),

]

switch_expressions = [
    ('''switch [] =>
  | [] -> true
  | _ :: _ -> false
end''', "Bool: true"),
    ('''switch [1, 2] =>
  | [] -> true
  | _ :: _ -> false
end''', "Bool: false"),
    ('''
type IntList =
  | Nil
  | Cons (Int, IntList)
 
let rec length l =
  switch l =>
  | Nil -> 0
  | Cons (_, t) -> 1 + length t
  end
 ''', "length : IntList -> Int = function")
]

higher_order_functions = [
    ('''let rec map f arr =
  switch arr =>
    | [] -> []
    | h :: t -> f h :: map f t
  end''', "(a -> b) -> [a] -> [b]: function"),
    ("map (\ x -> x * x) [1, 2, 3, 4, 5]", "[Int]: [1, 4, 9, 16, 25]"),
    ('''let rec filter f arr =
  switch arr =>
    | [] -> []
    | h :: t ->
      if f h then h :: filter t
      else filter t
    end''', "(a -> Bool) -> [a] -> [a]: function"),
    ('''let rec reduce_left op arr acc =
  switch arr =>
  | [] -> acc
  | h :: t -> reduce_left op t (op acc h)
  end''', "(a -> b -> a) -> [b] -> a -> a: function"),
    ('''let rec reduce_right op arr acc =
  switch arr =>
  | [] -> acc
  | h :: t -> op h (reduce_right op t acc)
  end''', "(a -> b -> b) -> [a] -> b -> b: function"),

]

algebraic_data_types = [
    ("type IntPair = (Int, Int)", "type IntPair : (Int, Int)"),
    ('''
type IntList =
  | Nil
  | Cons (Int, IntList)
''', "type IntList : Nil | Cons (Int, IntList)"),
    ('''
type IntBT =
  | Leaf
  | Node (Int, IntBT, IntBT)

''', "type IntBT : Leaf | Node (Int, IntBT, IntBT)"),


    ('''
type IntOpt =
  | None
  | Some (Int)
''', "type IntOpt : None | Some (Int)")
]

basic_cat = Category("Basic Types", [Ex(i, o) for i, o in basic])
compound_types_cat = Category(
    "Compound Types", [Ex(i, o) for i, o in compound_types])
let_expressions_cat = Category(
    "Let Expressions", [Ex(i, o) for i, o in let_expressions])
ternary_expressions_cat = Category(
    "Ternary Expressions", [Ex(i, o) for i, o in ternary_expressions])
switch_expressions_cat = Category(
    "Switch Expressions", [Ex(i, o) for i, o in switch_expressions])
higher_order_functions_cat = Category(
    "Higher Order Functions", [Ex(i, o) for i, o in higher_order_functions])
algebraic_data_types_cat = Category(
    "Algebraic Data Types", [Ex(i, o) for i, o in algebraic_data_types])


print(basic_cat)
print(compound_types_cat)
print(let_expressions_cat)
print(ternary_expressions_cat)
print(switch_expressions_cat)
print(higher_order_functions_cat)
print(algebraic_data_types_cat)
