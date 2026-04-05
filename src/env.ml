open Cexpr

(* definitions of initial dynamic and static environments *)

let built_ins : (string * value * c_type) list =
  [
    ("print_string", BuiltInFunction Println, Mono (StringType => UnitType));
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
    ("str_length", BuiltInFunction StrLength, Mono (StringType => IntType));
    ( "str_concat",
      BuiltInFunction StrConcat,
      Mono (StringType => (StringType => StringType)) );
    ( "str_slice",
      BuiltInFunction StrSlice,
      Mono (StringType => (IntType => (IntType => StringType))) );
    (* Built-in type constructors - these represent the types themselves when used as values *)
    (* Using a fresh type variable that will be unified during type checking *)
    ("Bool", UnitValue, Mono BoolType);
    ("Int", UnitValue, Mono IntType);
    ("String", UnitValue, Mono StringType);
    ("Char", UnitValue, Mono CharType);
    ("Float", UnitValue, Mono FloatType);
    ("Unit", UnitValue, Mono UnitType);
    (* Equality primitives for Eq typeclass *)
    ("int_eq", BuiltInFunction GenericEq, Mono (IntType => (IntType => BoolType)));
    ( "float_eq",
      BuiltInFunction GenericEq,
      Mono (FloatType => (FloatType => BoolType)) );
    ( "str_eq",
      BuiltInFunction GenericEq,
      Mono (StringType => (StringType => BoolType)) );
    ( "char_eq",
      BuiltInFunction GenericEq,
      Mono (CharType => (CharType => BoolType)) );
    ( "bool_eq",
      BuiltInFunction GenericEq,
      Mono (BoolType => (BoolType => BoolType)) );
    ( "unit_eq",
      BuiltInFunction GenericEq,
      Mono (UnitType => (UnitType => BoolType)) );
    (* Compare primitives for Ord typeclass — return Ordering (LT|EQ|GT) *)
    ( "int_compare",
      BuiltInFunction GenericCompare,
      Mono (IntType => (IntType => CTypeApp ("Ordering", []))) );
    ( "float_compare",
      BuiltInFunction GenericCompare,
      Mono (FloatType => (FloatType => CTypeApp ("Ordering", []))) );
    ( "str_compare",
      BuiltInFunction GenericCompare,
      Mono (StringType => (StringType => CTypeApp ("Ordering", []))) );
    ( "char_compare",
      BuiltInFunction GenericCompare,
      Mono (CharType => (CharType => CTypeApp ("Ordering", []))) );
    ( "bool_compare",
      BuiltInFunction GenericCompare,
      Mono (BoolType => (BoolType => CTypeApp ("Ordering", []))) );
    ( "unit_compare",
      BuiltInFunction GenericCompare,
      Mono (UnitType => (UnitType => CTypeApp ("Ordering", []))) );
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
    ("::", {|fn a -> fn b -> a :: b|});
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
    ("&&", {|fn a -> fn b -> a && b|});
    ("||", {|fn a -> fn b -> a || b|});
    ( "list_length",
      {|let rec length lst = case lst do | [] -> 0 | _ :: t -> 1 + length t in length|}
    );
    ("list_head", {|let head = fn lst -> case lst do | h :: _ -> h in head|});
    ("list_tail", {|let tail = fn lst -> case lst do | _ :: t -> t in tail|});
    ( "list_nth",
      {|let rec nth lst n = case lst do | h :: t -> if n == 0 then h else nth t (n - 1) in nth|}
    );
    ("tuple_fst", {|fn p -> case p do | (a, b) -> a|});
    ("tuple_snd", {|fn p -> case p do | (a, b) -> b|});
  ]

(* definitions of the REPL *)
