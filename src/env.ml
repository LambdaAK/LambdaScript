open Cexpr

(* definitions of initial dynamic and static environments *)

let built_ins : (string * value * c_type) list =
  [
    ("println", BuiltInFunction Println, StringType => UnitType);
    ("print", BuiltInFunction Print, StringType => UnitType);
    ("int_to_str", BuiltInFunction IntToString, IntType => StringType);
    ("int_to_float", BuiltInFunction IntToFloat, IntType => FloatType);
    ("float_to_int", BuiltInFunction FloatToInt, FloatType => IntType);
  ]

let built_ins_values : (string * value) list =
  List.map (fun (id, v, _) -> (id, v)) built_ins

let built_ins_types : (string * c_type) list =
  List.map (fun (id, _, t) -> (id, t)) built_ins

let code_mapping : (string * string) list =
  [
    ("not", {|
\a -> if a then false else true
|});
    ( "map",
      {|let rec map f arr =
switch arr =>
  | [] -> []
  | h :: t -> f h :: map f t
end
in
map|}
    );
    ( "filter",
      {|
      let rec filter f arr =
        switch arr =>
          | [] -> []
          | h :: t -> if f h then h :: filter f t else filter f t
        end
        in filter
    
    |}
    );
    ( "reduce_left",
      {|let rec fold f acc lst =
      switch lst =>
      | [] -> acc
      | h :: t -> fold f (f acc h) t
      end
      in fold
    |}
    );
    ( "reduce_right",
      {|let rec fold f lst acc =
      switch lst =>
      | [] -> acc
      | h :: t -> f h (fold f t acc)
      end 
      in fold|}
    );
    ("+", {|\a -> \b -> a + b|});
    ("-", {|\a -> \b -> a - b|});
    ("*", {|\a -> \b -> a * b|});
    ("/", {|\a -> \b -> a / b|});
    ("%", {|\a -> \b -> a % b|});
  ]

(* definitions of the REPL *)
