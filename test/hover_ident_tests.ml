open OUnit2

let hover ~prelude ~source ~line0 ~char0 =
  match
    Language.Hover_query.hover_type_for_identifier ~prelude
      ~src_path:"hover_fixture.ls" ~source ~line0 ~char0
  with
  | Ok s -> s
  | Error e -> assert_failure e

let suite =
  "hover_identifier_types"
  >::: [
         ( "let_binding_int_with_prelude" >:: fun _ ->
             (* [x] can match a trait method name in the condensed prelude; use a
                distinct binding name so the hover test is stable. *)
             let source = "let hover_binding = 1\n" in
             assert_equal ~printer:Fun.id "Int"
               (hover ~prelude:true ~source ~line0:0 ~char0:4)
         );
         ( "mutual_rec_second_binding_arrow_bool" >:: fun _ ->
             let source =
               {|let rec is_even n =
  if n == 0 then true
  else is_odd (n - 1)

and is_odd n =
  if n == 0 then false
  else is_even (n - 1)
|}
             in
             (* Line with [and is_odd n =] — 0-based line index and column on [is_odd]. *)
             assert_equal ~printer:Fun.id "Int -> Bool"
               (hover ~prelude:true ~source ~line0:4 ~char0:4) );
         ( "curried_let_map_func_and_list_param" >:: fun _ ->
             let source =
               {|let map func l =
  case l do
  | [] -> []
  | h :: t -> func h :: map func t
|}
             in
             assert_equal ~printer:Fun.id "a -> b"
               (hover ~prelude:true ~source ~line0:0 ~char0:8);
             assert_equal ~printer:Fun.id "List<a>"
               (hover ~prelude:true ~source ~line0:0 ~char0:13) );
         ( "paren_infix_let_pattern_operator" >:: fun _ ->
             (* [(>)] is a built-in operator; use [(^^)] so the binding is unambiguous. *)
             let source = "let (^^) a b = a\n" in
             (* Cursor on first [^] inside [(^^)]. *)
             assert_equal ~printer:Fun.id "a -> b -> a"
               (hover ~prelude:false ~source ~line0:0 ~char0:5) );
         ( "pattern_binding_common_name_prefers_user_scope" >:: fun _ ->
             let source =
               {|let s :: t = [1,2]
let _ = s
|}
             in
             assert_equal ~printer:Fun.id "Int"
               (hover ~prelude:true ~source ~line0:0 ~char0:4) );
         ( "prefix_operator_hover_parenthesized_builtin" >:: fun _ ->
             let source = "let _ = (+) 1 2\n" in
             assert_equal ~printer:Fun.id "Int -> Int -> Int"
               (hover ~prelude:false ~source ~line0:0 ~char0:9) );
         ( "type_name_hover_shows_type_definition" >:: fun _ ->
             let source =
               {|type Box<a> = a
let id (x : Box<Int>) = x
|}
             in
             assert_equal ~printer:Fun.id "type Box<a> = a"
               (hover ~prelude:false ~source ~line0:1 ~char0:12) );
         ( "hover_survives_unrelated_type_error_later" >:: fun _ ->
             let source =
               {|let x = 1
let broken = x + true
let y = x
|}
             in
             assert_equal ~printer:Fun.id "Int"
               (hover ~prelude:false ~source ~line0:2 ~char0:4) );
       ]

let () = run_test_tt_main suite
