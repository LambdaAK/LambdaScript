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

let lookup_value_exn (env : Language.Cexpr.env) (name : string) : string =
  match List.assoc_opt name env with
  | Some v -> Language.Ceval.string_of_value v
  | None -> failwith ("Missing runtime binding: " ^ name)

let assert_runtime_value ~env ~name ~expected =
  assert_equal ~printer:Fun.id expected (lookup_value_exn env name)

let assert_static_binding_exists ~static_env ~name =
  assert_bool ("expected static binding " ^ name) (List.mem_assoc name static_env)

let assert_type_alias_exists (type_env : Language.Typecheck.type_env) (name : string)
    =
  assert_bool ("expected type alias " ^ name)
    (List.exists (fun (n, _, _) -> String.equal n name) type_env)

let string_contains_substring (s : string) (sub : string) : bool =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let rec loop i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0

let assert_failure_contains ~expected_substring ~f =
  match
    try
      f ();
      None
    with
    | Failure msg -> Some msg
  with
  | Some msg ->
      assert_bool
        (Printf.sprintf "expected failure containing [%s], got [%s]"
           expected_substring msg)
        (string_contains_substring msg expected_substring)
  | None ->
      assert_failure
        (Printf.sprintf "expected Failure containing [%s] but function succeeded"
           expected_substring)

let suite =
  "module_system"
  >::: [
         ( "namespaces_value_defs_and_internal_refs" >:: fun _ ->
           let program =
             {|
mod A where
  let x = 1
  let y = x + 2
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"A.x";
           assert_static_binding_exists ~static_env ~name:"A.y";
           assert_runtime_value ~env:dynamic_env ~name:"A.y" ~expected:"3" );
         ( "sibling_modules_with_same_local_names_do_not_collide" >:: fun _ ->
           let program =
             {|
mod A where
  let x = 1
end

mod B where
  let x = 2
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"A.x";
           assert_static_binding_exists ~static_env ~name:"B.x";
           assert_runtime_value ~env:dynamic_env ~name:"A.x" ~expected:"1";
           assert_runtime_value ~env:dynamic_env ~name:"B.x" ~expected:"2" );
         ( "top_level_and_module_bindings_can_coexist" >:: fun _ ->
           let program =
             {|
let x = 9
mod A where
  let x = 1
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"x";
           assert_static_binding_exists ~static_env ~name:"A.x";
           assert_runtime_value ~env:dynamic_env ~name:"x" ~expected:"9";
           assert_runtime_value ~env:dynamic_env ~name:"A.x" ~expected:"1" );
         ( "module_functions_can_call_module_helpers" >:: fun _ ->
           let program =
             {|
mod Math where
  let add1 x = x + 1
  let out = add1 41
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"Math.out" ~expected:"42" );
         ( "module_recursive_functions_work" >:: fun _ ->
           let program =
             {|
mod Rec where
  let rec fact n = if n == 0 then 1 else n * fact (n - 1)
  let out = fact 5
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"Rec.out" ~expected:"120" );
         ( "module_trait_impl_and_constrained_call_work" >:: fun _ ->
           let program =
             {|
mod M where
  trait Render<a> where
    val render : a -> String
  end

  impl Render for Int where
    render x = int_to_str x
  end

  let out <Render Int> x = render x
  let y = out 7
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"M.out";
           assert_runtime_value ~env:dynamic_env ~name:"M.y" ~expected:"\"7\"" );
         ( "nested_module_trait_impl_and_call_work" >:: fun _ ->
           let program =
             {|
mod A where
  mod B where
    trait Render<a> where
      val render : a -> String
    end

    impl Render for Int where
      render x = int_to_str x
    end

    let out <Render Int> x = render x
    let y = out 5
  end
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"A.B.out";
           assert_runtime_value ~env:dynamic_env ~name:"A.B.y"
             ~expected:"\"5\"" );
         ( "module_trait_default_method_works" >:: fun _ ->
           let program =
             {|
mod T where
  trait EqLike<a> where
    val (==) : a -> a -> Bool
    val (!=) : a -> a -> Bool
    let (!=) x y = if (==) x y then false else true
  end

  impl EqLike for Int where
    (==) x y = int_eq x y
  end

  let out = if (!=) 2 3 then 1 else 0
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"T.out" ~expected:"1" );
         ( "module_trait_impl_over_module_sum_type_works" >:: fun _ ->
           let program =
             {|
mod Boxed where
  type Box =
    | Box of Int

  trait Render<a> where
    val render : a -> String
  end

  impl Render for Box where
    render b =
      case b do
      | Box n -> int_to_str n
  end

  let out = render (Box 12)
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"Boxed.out"
             ~expected:"\"12\"" );
         ( "nested_modules_can_reference_parent_bindings" >:: fun _ ->
           let program =
             {|
mod A where
  let x = 1
  mod B where
    let y = x + 1
  end
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"A.B.y" ~expected:"2" );
         ( "deeply_nested_modules_can_reference_ancestors_and_parents" >:: fun _ ->
           let program =
             {|
mod A where
  let base = 10
  mod B where
    let plus1 x = x + 1
    mod C where
      let out = plus1 base
    end
  end
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"A.B.C.out"
             ~expected:"11" );
         ( "module_sum_type_constructors_are_namespaced_and_usable" >:: fun _ ->
           let program =
             {|
mod Opt where
  type Maybe<a> =
    | None
    | Some of a

  let from_default m d =
    case m do
    | None -> d
    | Some x -> x

  let out = from_default (Some 3) 9
end
|}
           in
           let static_env, dynamic_env, _ = run_program_interpreter_style program in
           assert_static_binding_exists ~static_env ~name:"Opt.None";
           assert_static_binding_exists ~static_env ~name:"Opt.Some";
           assert_runtime_value ~env:dynamic_env ~name:"Opt.out" ~expected:"3" );
         ( "module_recursive_sum_types_work" >:: fun _ ->
           let program =
             {|
mod L where
  type rec MyList =
    | Nil
    | Cons of (Int, MyList)

  let rec sum xs =
    case xs do
    | Nil -> 0
    | Cons (h, t) -> h + sum t

  let out = sum (Cons (1, Cons (2, Nil)))
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"L.out" ~expected:"3" );
         ( "module_type_aliases_are_namespaced_in_type_env" >:: fun _ ->
           let program =
             {|
mod Types where
  type MyInt = Int
  let x = 1
end
|}
           in
           let _, _, type_env = run_program_interpreter_style program in
           assert_type_alias_exists type_env "Types.MyInt" );
         ( "nested_module_type_aliases_are_namespaced_in_type_env" >:: fun _ ->
           let program =
             {|
mod A where
  mod B where
    type T = Int
  end
end
|}
           in
           let _, _, type_env = run_program_interpreter_style program in
           assert_type_alias_exists type_env "A.B.T" );
         ( "qualified_value_access_works_with_A_dot_x" >:: fun _ ->
           let program =
             {|
mod A where
  let x = 41
end

let y = A.x + 1
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"y" ~expected:"42" );
         ( "qualified_nested_value_access_works_with_A_dot_B_dot_x" >:: fun _ ->
           let program =
             {|
mod A where
  mod B where
    let x = 7
  end
end

let y = A.B.x + 3
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"y" ~expected:"10" );
         ( "qualified_type_access_works_with_A_dot_T" >:: fun _ ->
           let program =
             {|
mod A where
  type T = Int
  let x : T = 4
end

let y : A.T = A.x + 1
|}
           in
           let _, dynamic_env, type_env = run_program_interpreter_style program in
           assert_type_alias_exists type_env "A.T";
           assert_runtime_value ~env:dynamic_env ~name:"y" ~expected:"5" );
         ( "use_imports_values_and_types_into_scope" >:: fun _ ->
           let program =
             {|
mod A where
  type T = Int
  let one : T = 1
end

mod B where
  use A
  let two : T = one + 1
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"B.two" ~expected:"2" );
         ( "use_supports_nested_module_paths" >:: fun _ ->
           let program =
             {|
mod A where
  mod B where
    let x = 5
  end
end

use A.B
let y = x * 2
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"y" ~expected:"10" );
         ( "use_resolves_relative_modules_inside_module_scope" >:: fun _ ->
           let program =
             {|
mod Root where
  mod Shared where
    let base = 40
  end
  mod Worker where
    use Shared
    let out = base + 2
  end
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"Root.Worker.out"
             ~expected:"42" );
         ( "ambiguous_use_reports_clear_error" >:: fun _ ->
           let program =
             {|
mod A where
  let x = 1
end

mod B where
  let x = 2
end

use A
use B
let y = x
|}
           in
           assert_failure_contains ~expected_substring:"ambiguous value 'x'"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
         ( "unknown_use_module_reports_error" >:: fun _ ->
           let program =
             {|
use Missing
let y = 1
|}
           in
           assert_failure_contains ~expected_substring:"unknown module in use"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
         ( "use_brings_traits_into_scope_for_constraints" >:: fun _ ->
           let program =
             {|
mod M where
  trait Render<a> where
    val render : a -> String
  end

  impl Render for Int where
    render x = int_to_str x
  end
end

mod N where
  use M
  let out <Render Int> x = render x
  let y = out 8
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"N.y" ~expected:"\"8\"" );
         ( "qualified_trait_name_works_in_constraints" >:: fun _ ->
           let program =
             {|
mod M where
  trait Render<a> where
    val render : a -> String
  end

  impl Render for Int where
    render x = int_to_str x
  end
end

let out <M.Render Int> x = render x
let y = out 9
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"y" ~expected:"\"9\"" );
       ]

let () = run_test_tt_main suite
