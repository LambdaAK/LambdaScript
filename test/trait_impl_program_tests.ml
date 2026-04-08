open OUnit2

let parse_program (source : string) : Language.Expr.defn list =
  let input = source |> String.to_seq |> List.of_seq in
  let tokens =
    Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
  in
  match Language.Parser.ProgramParser.program_parser tokens with
  | Some (program, []) -> program
  | Some (_, rem) ->
      failwith
        (Printf.sprintf "Program parse left %d trailing tokens" (List.length rem))
  | None -> failwith "Failed to parse program"

let parse_expr (source : string) : Language.Expr.expr =
  let input = source |> String.to_seq |> List.of_seq in
  let tokens =
    Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
  in
  match Language.Parser.ExprParser.expr_parser tokens with
  | Some (expr, []) -> expr
  | Some (_, rem) ->
      failwith
        (Printf.sprintf "Expression parse left %d trailing tokens"
           (List.length rem))
  | None -> failwith "Failed to parse expression"

let run_program_interpreter_style (program_src : string) :
    Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env
    =
  let program = parse_program program_src in
  let c_program = Language.Condense.condense_program program in
  let static_env = Language.Build_env.build_full_static_env () in
  let dynamic_env =
    Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
  in
  let type_env = [] in
  List.fold_left
    (fun (static_env, dynamic_env, type_env) defn ->
      match Language.Typecheck.generate_defn static_env type_env defn with
      | Language.Typecheck.Error e ->
          failwith
            ("Type error: " ^ Language.Typecheck.string_of_type_check_error e)
      | Language.Typecheck.Ok (new_bindings, new_type_bindings, _) ->
          let elaborated =
            Language.Typecheck.elaborate_defn ~rewrite_constrained_calls:true
              (new_bindings @ static_env)
              (new_type_bindings @ type_env) defn
          in
          let new_dynamic_bindings =
            match Language.Ceval.eval_defn elaborated dynamic_env with
            | Language.Ceval.Ok v -> v
            | Language.Ceval.Error e ->
                failwith
                  ("Evaluation error: " ^ Language.Ceval.string_of_eval_error e)
          in
          ( new_bindings @ static_env,
            new_dynamic_bindings @ dynamic_env,
            new_type_bindings @ type_env ))
    (static_env, dynamic_env, type_env) c_program

let eval_expr_in_state
    ((static_env, dynamic_env, type_env) :
      Language.Cexpr.static_env * Language.Cexpr.env *
      Language.Typecheck.type_env)
    (expr_src : string) : Language.Cexpr.value * Language.Cexpr.c_type =
  let expr = parse_expr expr_src |> Language.Condense.condense_expr in
  let inferred_type =
    match Language.Typecheck.type_of_c_expr static_env type_env expr with
    | Language.Typecheck.Ok t -> t
    | Language.Typecheck.Error e ->
        failwith
          ("Type error: " ^ Language.Typecheck.string_of_type_check_error e)
  in
  let elaborated =
    Language.Typecheck.elaborate_expr ~rewrite_constrained_calls:true static_env
      type_env expr
  in
  let value =
    match Language.Ceval.eval_c_expr elaborated dynamic_env with
    | Language.Ceval.Ok v -> v
    | Language.Ceval.Error e ->
        failwith ("Evaluation error: " ^ Language.Ceval.string_of_eval_error e)
  in
  (value, inferred_type)

let rec strip_outer_poly_for_test (t : Language.Cexpr.c_type) :
    Language.Cexpr.c_type =
  match t with
  | Language.Cexpr.PolyType (_, inner) -> strip_outer_poly_for_test inner
  | t -> t

let assert_expr_value ~program ~expr ~expected_value =
  let state = run_program_interpreter_style program in
  let value, _ = eval_expr_in_state state expr in
  let got = Language.Ceval.string_of_value value in
  assert_equal ~printer:Fun.id expected_value got

let assert_expr_type ~program ~expr ~expected_type =
  let state = run_program_interpreter_style program in
  let _, inferred_type = eval_expr_in_state state expr in
  let got =
    inferred_type |> strip_outer_poly_for_test |> Language.C_to_string.string_of_c_type
  in
  assert_equal ~printer:Fun.id expected_type got

let suite =
  "trait_impl_program_regressions"
  >::: [
         ( "interpreter_operator_methods_eqneq_dispatch" >:: fun _ ->
           let program =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
}
impl EqLike for Int where
  let (==) x y = x == y
  let (!=) x y = if x == y then false else true
end
|}
           in
           assert_expr_type ~program ~expr:"if (==) 5 5 && (!=) 5 4 then 1 else 0"
             ~expected_type:"Int";
           assert_expr_value ~program
             ~expr:"if (==) 5 5 && (!=) 5 4 then 1 else 0"
             ~expected_value:"1" );
         ( "interpreter_default_operator_method_dispatch" >:: fun _ ->
           let program =
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
           assert_expr_type ~program ~expr:"(!=) 2 3" ~expected_type:"Bool";
           assert_expr_value ~program ~expr:"if (!=) 2 3 then 1 else 0"
             ~expected_value:"1" );
         ( "interpreter_recursive_impl_method_with_local_shadow" >:: fun _ ->
           let program =
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
           assert_expr_type ~program ~expr:"mappend [1,2] [3]"
             ~expected_type:"List<Int>";
           assert_expr_value ~program ~expr:"mappend [1,2] [3]"
             ~expected_value:"[1, 2, 3]" );
         ( "interpreter_recursive_constrained_impl_supports_cross_type_calls"
           >:: fun _ ->
           let program =
             {|
inter Render <a> {
  val render : a -> String
}
impl Render for Int where
  let render x = int_to_str x
end

type Maybe<a> =
  | Nothing
  | Just of a

impl Render for Maybe<a> requires Render<a> where
  let render o =
    case o do
    | Nothing -> "Nothing"
    | Just v -> "Just(" ^ (render v) ^ ")"
end

type rec LinkedList<a> =
  | Nil
  | Cons of (a, LinkedList<a>)

impl Render for LinkedList<a> requires Render<a> where
  let rec render l =
    case l do
    | Nil -> "Nil"
    | Cons (h, t) -> "Cons " ^ (render h) ^ " " ^ (render t)
end
|}
           in
           assert_expr_type ~program
             ~expr:"render (Cons (1, Cons (2, Nil)))"
             ~expected_type:"String";
           assert_expr_value ~program
             ~expr:"render (Cons (1, Cons (2, Nil)))"
             ~expected_value:{|"Cons 1 Cons 2 Nil"|};
           assert_expr_value ~program ~expr:"render (Just 7)"
             ~expected_value:{|"Just(7)"|} );
         ( "interpreter_first_class_trait_method_argument" >:: fun _ ->
           let program =
             {|
inter Semigroup <a> {
  val sappend : a -> a -> a
}
impl Semigroup for Int where
  let sappend x y = x + y
end
|}
           in
           assert_expr_value ~program
             ~expr:"let apply op x y = op x y in apply sappend 4 5"
             ~expected_value:"9" );
         ( "interpreter_constrained_helper_dispatches_mappend" >:: fun _ ->
           let program =
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
let combine<Monoid a> x y = mappend x y
|}
           in
           assert_expr_type ~program ~expr:"combine [1,2] [3]"
             ~expected_type:"List<Int>";
           assert_expr_value ~program ~expr:"combine [1,2] [3]"
             ~expected_value:"[1, 2, 3]" );
         ( "interpreter_empty_list_show_dispatch" >:: fun _ ->
           let program =
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
           assert_expr_type ~program ~expr:"show []" ~expected_type:"String";
           assert_expr_value ~program ~expr:"show []" ~expected_value:{|"empty"|}
         );
         ( "interpreter_operator_method_alias_is_callable" >:: fun _ ->
           let program =
             {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
}
impl EqLike for Int where
  let (==) x y = x == y
  let (!=) x y = if x == y then false else true
end
|}
           in
           assert_expr_value ~program
             ~expr:"let neq = (!=) in if neq 1 2 then 1 else 0"
             ~expected_value:"1" );
       ]

let () = run_test_tt_main suite
