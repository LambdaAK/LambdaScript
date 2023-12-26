open Cexpr

(* definitions of initial dynamic and static environments *)

let built_ins : (string * value * c_type) list =
  [
    ("println", BuiltInFunction Println, StringType => UnitType);
    ("print", BuiltInFunction Print, StringType => UnitType);
    ("int_to_str", BuiltInFunction IntToString, IntType => StringType);
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
      {|let rec map f arr <-
switch arr =>
  | [] -> []
  | h :: t -> f h :: map f t
end
in
map|}
    );
    ("filter", {|1|});
    ( "reduce_left",
      {|let rec fold op acc arr <-
      switch arr =>
      | [] -> acc
      | h :: t -> fold op (op acc h) t
      end
    in
    fold
    |}
    );
    ( "reduce_right",
      {|let rec fold op acc arr <-
      switch arr =>
      | [] -> acc
      | h :: t -> op (fold op acc t) h
      end
    in
    fold|}
    );
  ]

(* definitions of the REPL *)
