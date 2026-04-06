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
         ( "let_x_is_int_with_prelude" >:: fun _ ->
             let source = "let x = 1\n" in
             assert_equal ~printer:Fun.id "Int" (hover ~prelude:true ~source ~line0:0 ~char0:4)
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
       ]

let () = run_test_tt_main suite
