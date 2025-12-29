open Cexpr

(* definitions of initial dynamic and static environments *)

let built_ins : (string * value * c_type) list =
  [
    ("println", BuiltInFunction Println, Mono (StringType => UnitType));
    ("print", BuiltInFunction Print, Mono (StringType => UnitType));
    ("int_to_str", BuiltInFunction IntToString, Mono (IntType => StringType));
    ("int_to_float", BuiltInFunction IntToFloat, Mono (IntType => FloatType));
    ("float_to_int", BuiltInFunction FloatToInt, Mono (FloatType => IntType));
    ( "string_to_list",
      BuiltInFunction StringToList,
      Mono (StringType => CListType CharType) );
    ( "list_of_string",
      BuiltInFunction StringToList,
      Mono (StringType => CListType CharType) );
    (* Built-in type constructors - these represent the types themselves when used as values *)
    (* Using a fresh type variable that will be unified during type checking *)
    ("bool", UnitValue, Mono BoolType);
    ("int", UnitValue, Mono IntType);
    ("string", UnitValue, Mono StringType);
    ("char", UnitValue, Mono CharType);
    ("float", UnitValue, Mono FloatType);
    ("unit", UnitValue, Mono UnitType);
    (* Arithmetic operators *)
    ("+", UnitValue, Mono (IntType => (IntType => IntType)));
    ("-", UnitValue, Mono (IntType => (IntType => IntType)));
    ("*", UnitValue, Mono (IntType => (IntType => IntType)));
    ("/", UnitValue, Mono (IntType => (IntType => IntType)));
    ("%", UnitValue, Mono (IntType => (IntType => IntType)));
    (* Comparison operators *)
    ("<", UnitValue, Mono (IntType => (IntType => BoolType)));
    (">", UnitValue, Mono (IntType => (IntType => BoolType)));
    ("<=", UnitValue, Mono (IntType => (IntType => BoolType)));
    (">=", UnitValue, Mono (IntType => (IntType => BoolType)));
    ("==", UnitValue, Mono (IntType => (IntType => BoolType)));
    ("!=", UnitValue, Mono (IntType => (IntType => BoolType)));
    ("<>", UnitValue, Mono (IntType => (IntType => BoolType)));
    (* Logical operators *)
    ("&&", UnitValue, Mono (BoolType => (BoolType => BoolType)));
    ("||", UnitValue, Mono (BoolType => (BoolType => BoolType)));
  ]

let built_ins_values : (string * value) list =
  List.map (fun (id, v, _) -> (id, v)) built_ins

let built_ins_types : (string * c_type) list =
  List.map (fun (id, _, t) -> (id, t)) built_ins

let code_mapping : (string * string) list =
  [
    ("not", {|
fn a -> if a then false else true
|});
    ( "map",
      {|let rec map f arr =
case arr do
  | [] -> []
  | h :: t -> f h :: map f t
in
map|}
    );
    ( "filter",
      {|let rec filter f arr =
case arr do
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t
in
filter|}
    );
    ( "reduce_left",
      {|let rec fold f acc lst =
      case lst do
      | [] -> acc
      | h :: t -> fold f (f acc h) t
      in fold
    |}
    );
    ( "reduce_right",
      {|let rec fold f lst acc =
      case lst do
      | [] -> acc
      | h :: t -> f h (fold f t acc)
      in fold|}
    );
    ("+", {|fn a -> fn b -> a + b|});
    ("-", {|fn a -> fn b -> a - b|});
    ("*", {|fn a -> fn b -> a * b|});
    ("/", {|fn a -> fn b -> a / b|});
    ("%", {|fn a -> fn b -> a % b|});
    ("<", {|fn a -> fn b -> a < b|});
    (">", {|fn a -> fn b -> a > b|});
    ("<=", {|fn a -> fn b -> a <= b|});
    (">=", {|fn a -> fn b -> a >= b|});
    ("==", {|fn a -> fn b -> a == b|});
    ("!=", {|fn a -> fn b -> if a == b then false else true|});
    ("<>", {|fn a -> fn b -> if a == b then false else true|});
    ("&&", {|fn a -> fn b -> a && b|});
    ("||", {|fn a -> fn b -> a || b|});
  ]

(* definitions of the REPL *)
