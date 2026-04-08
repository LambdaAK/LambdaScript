open OUnit2

type repl_state =
  Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

let fresh_repl_state () : repl_state =
  Language.Repl_kernel.clear_prelude_condense_cache ();
  let static_env = Language.Build_env.build_full_static_env () in
  let dynamic_env =
    Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
  in
  (static_env, dynamic_env, [])

let run_input ((static_env, dynamic_env, type_env) : repl_state) (source : string)
    : Language.Repl_kernel.eval_outcome * repl_state =
  let outcome, static_env', dynamic_env', type_env' =
    Language.Repl_kernel.eval_user_input static_env dynamic_env type_env source
  in
  (outcome, (static_env', dynamic_env', type_env'))

let expect_expr ?typ ?value (outcome : Language.Repl_kernel.eval_outcome) : unit =
  match outcome with
  | Language.Repl_kernel.Ev_expr { typ = got_typ; value = got_value } ->
      (match typ with
      | Some t -> assert_equal ~printer:Fun.id t got_typ
      | None -> ());
      (match value with
      | Some v -> assert_equal ~printer:Fun.id v got_value
      | None -> ())
  | Language.Repl_kernel.Ev_error msg ->
      assert_failure ("Expected expression result, got error: " ^ msg)
  | Language.Repl_kernel.Ev_defs _ ->
      assert_failure "Expected expression result, got definitions"

let expect_defs (outcome : Language.Repl_kernel.eval_outcome) :
    (string * string * string) list =
  match outcome with
  | Language.Repl_kernel.Ev_defs { bindings } -> bindings
  | Language.Repl_kernel.Ev_error msg ->
      assert_failure ("Expected definitions result, got error: " ^ msg)
  | Language.Repl_kernel.Ev_expr _ ->
      assert_failure "Expected definitions result, got expression"

let expect_no_internal_bindings (bindings : (string * string * string) list) :
    unit =
  let is_internal (name, _, _) =
    String.starts_with ~prefix:"__forge_dict_" name
    || String.starts_with ~prefix:"__dict_" name
  in
  let leaked = List.filter is_internal bindings in
  if leaked <> [] then
    let names =
      leaked |> List.map (fun (name, _, _) -> name) |> String.concat ", "
    in
    assert_failure ("Internal REPL bindings leaked: " ^ names)

let suite =
  "repl_trait_impl_regressions"
  >::: [
         ( "constrained_def_call_rewritten_across_inputs" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Show <a> {
  val show : a -> String
}
impl Show for Int where
  let show x = int_to_str x
end
let render<Show a> x = show x
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 = run_input st1 "render 41" in
           expect_expr ~typ:"String" ~value:{|"41"|} out2 );
         ( "constrained_def_alias_call_rewrites_dictionary_argument"
           >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Show <a> {
  val show : a -> String
}
impl Show for Int where
  let show x = int_to_str x
end
let render<Show a> x = show x
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, st2 = run_input st1 "let render_alias = render" in
           ignore (expect_defs out2 : (string * string * string) list);
           let out3, _st3 = run_input st2 "render_alias 9" in
           expect_expr ~typ:"String" ~value:{|"9"|} out3 );
         ( "repl_keeps_trait_impl_state_across_inputs" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}
impl Monoid for [Int] where
  let rec mappend xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: mappend t ys
  let mempty = []
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, st2 = run_input st1 "let joined = mappend [1,2] [3,4]" in
           ignore (expect_defs out2 : (string * string * string) list);
           let out3, _st3 = run_input st2 "joined" in
           expect_expr ~typ:"List<Int>" ~value:"[1, 2, 3, 4]" out3 );
         ( "first_class_trait_method_dispatches_when_applied" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Semigroup <a> {
  val sappend : a -> a -> a
}
impl Semigroup for Int where
  let sappend x y = x + y
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 =
             run_input st1 "let apply op x y = op x y in apply sappend 3 4"
           in
           expect_expr ~typ:"Int" ~value:"7" out2 );
         ( "empty_list_dispatch_uses_list_dictionary_fallback" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Show <a> {
  val show : a -> String
}
impl Show for [a] where
  let show xs =
    case xs do
    | [] -> "empty"
    | _ :: _ -> "nonempty"
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 = run_input st1 "show []" in
           expect_expr ~typ:"String" ~value:{|"empty"|} out2 );
         ( "impl_processing_does_not_leak_internal_dictionary_bindings"
           >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
}
impl EqLike for Int where
  let (==) x y = x == y
end
let x = 1
|}
           in
           let out1, _st1 = run_input st0 setup in
           let bindings = expect_defs out1 in
           expect_no_internal_bindings bindings;
           assert_bool "expected user binding x"
             (List.exists (fun (name, _, _) -> String.equal name "x") bindings)
         );
         ( "recursive_impl_method_handles_local_shadowing_without_bad_rename"
           >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}
impl Monoid for [Int] where
  let rec mappend xs ys =
    let use_local mappend rest = mappend rest ys in
    case xs do
    | [] -> ys
    | h :: t -> h :: use_local mappend t
  let mempty = []
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 = run_input st1 "mappend [1,2] [3]" in
           expect_expr ~typ:"List<Int>" ~value:"[1, 2, 3]" out2 );
         ( "operator_methods_do_not_collide_in_repl_dispatch" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
}
impl EqLike for Int where
  let (==) x y = x == y
  let (!=) x y = not (x == y)
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 =
             run_input st1 "if (==) 2 2 && (!=) 2 3 then 1 else 0"
           in
           expect_expr ~typ:"Int" ~value:"1" out2 );
         ( "default_operator_method_survives_internal_name_sanitization"
           >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
  let (!=) x y = if (==) x y then false else true
}
impl EqLike for Int where
  let (==) x y = x == y
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, _st2 = run_input st1 "if (!=) 2 3 then 1 else 0" in
           expect_expr ~typ:"Int" ~value:"1" out2 );
         ( "operator_method_alias_still_dispatches_correctly" >:: fun _ ->
           let st0 = fresh_repl_state () in
           let setup =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
}
impl EqLike for Int where
  let (==) x y = x == y
  let (!=) x y = not (x == y)
end
|}
           in
           let out1, st1 = run_input st0 setup in
           ignore (expect_defs out1 : (string * string * string) list);
           let out2, st2 = run_input st1 "let neq = (!=)" in
           ignore (expect_defs out2 : (string * string * string) list);
           let out3, _st3 = run_input st2 "if neq 2 2 then 1 else 0" in
           expect_expr ~typ:"Int" ~value:"0" out3 );
       ]

let () = run_test_tt_main suite
