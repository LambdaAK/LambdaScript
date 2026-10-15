(** The editor analysis ([Language.Ide], the [forge_ide] tool): symbols,
    references and diagnostics with their positions. *)

open OUnit2
open Forge_test_util

let analyze ?(path = "t.forge") (source : string) : string =
  let dir = Filename.concat (repo ()) "test" in
  Language.Ide.analyze ~src_path:(Filename.concat dir path) ~source

let has (json : string) (fragment : string) = contains json fragment

let check name src fragments =
  name >:: fun _ ->
  let json = analyze src in
  List.iter
    (fun f -> if not (has json f) then assert_failure (Printf.sprintf "%s: expected %S in\n%s" name f json))
    fragments

let shapes =
  "// Shapes in the plane.\n\
   type Shape = | Circle of Int | Rect of (Int, Int)\n\
   \n\
   // Area, rounded down.\n\
   let area s =\n\
  \  case s do\n\
  \  | Circle r -> 3 * r * r\n\
  \  | Rect (w, h) -> w * h\n\
   \n\
   let total shapes = reduce_left (fn acc -> fn s -> acc + area s) 0 shapes\n\
   let bad = 1 + \"x\"\n\
   let () = println (total [Circle 1])\n"

let suite =
  "ide"
  >::: [
         check "top-level function: type and doc comment" shapes
           [ "\"qname\":\"area\",\"kind\":\"function\",\"type\":\"Shape -> Int\",\"doc\":\"Area, rounded down.\"" ];
         check "parameters and pattern variables, with their types" shapes
           [
             "\"qname\":\"s\",\"kind\":\"parameter\",\"type\":\"Shape\"";
             "\"qname\":\"r\",\"kind\":\"variable\",\"type\":\"Int\"";
             "\"qname\":\"shapes\",\"kind\":\"parameter\",\"type\":\"List<Shape>\"";
           ];
         check "types and constructors" shapes
           [
             "\"qname\":\"Shape\",\"kind\":\"type\"";
             "\"qname\":\"Circle\",\"kind\":\"constructor\",\"type\":\"Int -> Shape\"";
           ];
         check "references are located" shapes
           [ (* r in 3 * r * r *) "\"range\":[6,20,6,21]"; (* area in total *) "\"range\":[9,56,9,60]" ];
         check "a type error is reported on its definition, and checking goes on" shapes
           [ "\"message\":\"type error: cannot match String with Int\""; "\"range\":[10,4,10,5]"; "\"qname\":\"total\",\"kind\":\"function\",\"type\":\"List<Shape> -> Int\"" ];
         check "operator sections keep later positions" "let a = (+) 1 2\nlet b = (+.) 1.0 2.0\nlet c = a\n"
           [ "\"range\":[2,8,2,9]" ];
         check "syntax errors" "let f x =\n  x +\n\nlet g = 2\n" [ "\"severity\":1,\"message\":\"syntax error near `let`\"" ];
         check "exhaustiveness warnings" "type L = | R | A | G\nlet next l = case l do | R -> G | A -> R\n"
           [ "\"severity\":2,\"message\":\"this case is not exhaustive: `G` is not matched\"" ];
         ( "imports: definitions in other files are located" >:: fun _ ->
           let path = Filename.concat (repo ()) "examples/ml/main.forge" in
           let json = Language.Ide.analyze ~src_path:path ~source:(read_file path) in
           List.iter
             (fun f -> if not (has json f) then assert_failure (Printf.sprintf "expected %S" f))
             [ "\"qname\":\"Fmt.fixed\",\"kind\":\"function\",\"type\":\"Int -> Float -> String\""; "util.forge\",\"range\":[46,6,46,11]" ] );
       ]

let () = run_test_tt_main suite
