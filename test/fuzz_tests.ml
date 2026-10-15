(** Differential fuzzing: a generator of random well-typed Forge programs; each
    program runs through the interpreter and the native compiler, and their
    stdout and exit status must agree.

    The generator is deterministic (seeded); set [FORGE_FUZZ_SEED] to explore a
    different region and [FORGE_FUZZ_COUNT] to change the number of programs.
    A failing case prints the full program so it can be minimised by hand. *)

open OUnit2
open Forge_test_util

type ty = TInt | TBool | TStr | TList

type env = (string * ty) list

let pick st xs = List.nth xs (Random.State.int st (List.length xs))

let vars_of env ty = List.filter_map (fun (v, t) -> if t = ty then Some v else None) env

type gen = { st : Random.State.t; mutable counter : int; funs : (string * ty * ty) list ref }

let fresh g prefix =
  g.counter <- g.counter + 1;
  Printf.sprintf "%s%d" prefix g.counter

(** Integer literal in Forge syntax (unary minus is [~-]). *)
let lit n = if n < 0 then Printf.sprintf "(~-%d)" (-n) else string_of_int n

let int_lit g =
  let st = g.st in
  match Random.State.int st 10 with
  | 0 -> string_of_int (2_147_483_648 + Random.State.int st 1_000_000_000) (* beyond int32 *)
  | 1 -> "~-" ^ string_of_int (Random.State.int st 1000)
  | 2 -> "0"
  | _ -> string_of_int (Random.State.int st 100)

let str_lit g =
  let st = g.st in
  let n = Random.State.int st 5 in
  "\"" ^ String.init n (fun _ -> Char.chr (Char.code 'a' + Random.State.int st 26)) ^ "\""

let rec gen_expr g (env : env) (ty : ty) (depth : int) : string =
  let st = g.st in
  let leaf () =
    let vs = vars_of env ty in
    if vs <> [] && Random.State.bool st then pick st vs
    else
      match ty with
      | TInt -> int_lit g
      | TBool -> if Random.State.bool st then "true" else "false"
      | TStr -> str_lit g
      | TList -> (
          match Random.State.int st 3 with
          | 0 -> "[]"
          | 1 -> Printf.sprintf "[%s, %s]" (int_lit g) (int_lit g)
          | _ -> Printf.sprintf "[1...%d]" (Random.State.int st 6))
  in
  if depth <= 0 then leaf ()
  else
    let sub ty' = gen_expr g env ty' (depth - 1) in
    let common () =
      match Random.State.int st 4 with
      | 0 -> Printf.sprintf "(if %s then %s else %s)" (sub TBool) (sub ty) (sub ty)
      | 1 ->
          let x = fresh g "v" in
          let t' = pick st [ TInt; TBool; TStr; TList ] in
          let e1 = sub t' in
          Printf.sprintf "(let %s = %s in %s)" x e1 (gen_expr g ((x, t') :: env) ty (depth - 1))
      | 2 ->
          let x = fresh g "p" in
          let t' = pick st [ TInt; TStr ] in
          let arg = sub t' in
          Printf.sprintf "((fn %s -> %s) %s)" x (gen_expr g ((x, t') :: env) ty (depth - 1)) arg
      | _ ->
          let h = fresh g "h" and t = fresh g "t" in
          Printf.sprintf "(case %s do | [] -> %s | %s :: %s -> %s)" (sub TList) (sub ty) h t
            (gen_expr g ((h, TInt) :: (t, TList) :: env) ty (depth - 1))
    in
    let funs_returning ty' = List.filter (fun (_, _, r) -> r = ty') !(g.funs) in
    match ty with
    | TInt -> (
        match Random.State.int st 14 with
        | 0 -> Printf.sprintf "(%s + %s)" (sub TInt) (sub TInt)
        | 1 -> Printf.sprintf "(%s - %s)" (sub TInt) (sub TInt)
        | 2 -> Printf.sprintf "(%s * %s)" (sub TInt) (lit (Random.State.int st 21 - 10))
        | 3 -> Printf.sprintf "(%s / %s)" (sub TInt) (lit (pick st [ 1; 2; 3; 7; ~-2; ~-5 ]))
        | 4 -> Printf.sprintf "(%s %% %s)" (sub TInt) (lit (pick st [ 2; 3; 10; ~-3 ]))
        | 5 -> Printf.sprintf "(list_length %s)" (sub TList)
        | 6 -> Printf.sprintf "(str_length %s)" (sub TStr)
        | 7 ->
            Printf.sprintf "(case %s do | 0 -> %s | 1 -> %s | _ -> %s)" (sub TInt) (sub TInt) (sub TInt)
              (sub TInt)
        | 8 -> Printf.sprintf "({fa: %s, fb: %s}.%s)" (sub TInt) (sub TStr) "fa"
        | 9 ->
            let a = fresh g "a" and b = fresh g "b" in
            Printf.sprintf "(case (%s, %s) do | (%s, %s) -> %s)" (sub TInt) (sub TBool) a b
              (gen_expr g ((a, TInt) :: (b, TBool) :: env) TInt (depth - 1))
        | 10 -> (
            match funs_returning TInt with
            | [] -> leaf ()
            | fs ->
                let name, arg_ty, _ = pick st fs in
                Printf.sprintf "(%s %s)" name (sub arg_ty))
        | 11 -> Printf.sprintf "(reduce_left (fn acc -> fn x -> acc + x) %s %s)" (sub TInt) (sub TList)
        | _ -> common ())
    | TBool -> (
        match Random.State.int st 10 with
        | 0 -> Printf.sprintf "(%s < %s)" (sub TInt) (sub TInt)
        | 1 -> Printf.sprintf "(%s == %s)" (sub TInt) (sub TInt)
        | 2 -> Printf.sprintf "(%s >= %s)" (sub TInt) (sub TInt)
        | 3 -> Printf.sprintf "(%s && %s)" (sub TBool) (sub TBool)
        | 4 -> Printf.sprintf "(%s || %s)" (sub TBool) (sub TBool)
        | 5 -> Printf.sprintf "(not %s)" (sub TBool)
        | 6 -> Printf.sprintf "(%s == %s)" (sub TStr) (sub TStr)
        | 7 -> Printf.sprintf "(%s < %s)" (sub TStr) (sub TStr)
        | 8 -> Printf.sprintf "(%s == %s)" (sub TList) (sub TList)
        | _ -> common ())
    | TStr -> (
        match Random.State.int st 8 with
        | 0 | 1 -> Printf.sprintf "(%s ^ %s)" (sub TStr) (sub TStr)
        | 2 | 3 -> Printf.sprintf "(int_to_str %s)" (sub TInt)
        | 4 -> Printf.sprintf "(str_slice %s %d %d)" (sub TStr) (Random.State.int st 4) (Random.State.int st 4)
        | 5 -> Printf.sprintf "(show %s)" (sub TList)
        | _ -> common ())
    | TList -> (
        match Random.State.int st 9 with
        | 0 -> Printf.sprintf "(%s :: %s)" (sub TInt) (sub TList)
        | 1 ->
            let x = fresh g "x" in
            Printf.sprintf "(map (fn %s -> %s) %s)" x (gen_expr g ((x, TInt) :: env) TInt (depth - 1)) (sub TList)
        | 2 ->
            let x = fresh g "x" in
            Printf.sprintf "(filter (fn %s -> %s) %s)" x (gen_expr g ((x, TInt) :: env) TBool (depth - 1))
              (sub TList)
        | 3 ->
            let x = fresh g "x" in
            Printf.sprintf "[%s | %s <- %s]" (gen_expr g ((x, TInt) :: env) TInt (depth - 1)) x (sub TList)
        | 4 -> Printf.sprintf "(%s ++ %s)" (sub TList) (sub TList)
        | 5 -> Printf.sprintf "[%s, %s, %s]" (sub TInt) (sub TInt) (sub TInt)
        | _ -> common ())

let ty_name = function TInt -> "Int" | TBool -> "Bool" | TStr -> "String" | TList -> "List<Int>"

let printer = function
  | TInt -> "int_to_str"
  | TBool -> "(fn b -> if b then \"T\" else \"F\")"
  | TStr -> "(fn s -> s)"
  | TList -> "show"

let gen_program (seed : int) : string =
  let g = { st = Random.State.make [| seed |]; counter = 0; funs = ref [] } in
  let st = g.st in
  let b = Buffer.create 2048 in
  (* A few top-level functions, some recursive with a bounded counter. *)
  for _ = 1 to 1 + Random.State.int st 3 do
    let name = fresh g "f" in
    let arg_ty = pick st [ TInt; TStr; TList ] in
    let ret_ty = pick st [ TInt; TBool; TStr; TList ] in
    let x = fresh g "x" in
    let body = gen_expr g [ (x, arg_ty) ] ret_ty 3 in
    Buffer.add_string b
      (Printf.sprintf "let %s (%s : %s) : %s = %s\n" name x (ty_name arg_ty) (ty_name ret_ty) body);
    g.funs := (name, arg_ty, ret_ty) :: !(g.funs)
  done;
  if Random.State.bool st then (
    let name = fresh g "loop" in
    let n = fresh g "n" and acc = fresh g "acc" in
    let step = gen_expr g [ (n, TInt); (acc, TInt) ] TInt 2 in
    Buffer.add_string b
      (Printf.sprintf "let rec %s %s %s = if %s <= 0 then %s else %s (%s - 1) ((%s) %% 1000003)\n" name n
         acc n acc name n step);
    Buffer.add_string b
      (Printf.sprintf "let () = print_string (int_to_str (%s %d 0))\n" name (Random.State.int st 50)));
  (* Top-level values, then prints. *)
  let env = ref [] in
  for _ = 1 to 2 + Random.State.int st 3 do
    let v = fresh g "g" in
    let t = pick st [ TInt; TBool; TStr; TList ] in
    Buffer.add_string b (Printf.sprintf "let %s = %s\n" v (gen_expr g !env t 3));
    env := (v, t) :: !env
  done;
  for _ = 1 to 3 + Random.State.int st 4 do
    let t = pick st [ TInt; TBool; TStr; TList ] in
    Buffer.add_string b
      (Printf.sprintf "let () = print_string (%s (%s))\n" (printer t) (gen_expr g !env t 4))
  done;
  Buffer.contents b

let base_seed =
  match Sys.getenv_opt "FORGE_FUZZ_SEED" with Some s -> int_of_string s | None -> 20261014

let count = match Sys.getenv_opt "FORGE_FUZZ_COUNT" with Some s -> int_of_string s | None -> 120

let fuzz_case i =
  let seed = base_seed + i in
  Printf.sprintf "seed %d" seed >:: fun _ ->
  let src = gen_program seed in
  (* The program must be accepted by the interpreter... *)
  let ri = interp src in
  (match ri.status with
  | Exited 0 -> ()
  | _ -> assert_failure ("generated program rejected or crashed in the interpreter: " ^ describe ri ^ show_src src));
  (* ...and the native executable must agree with it exactly. *)
  assert_both (Printf.sprintf "seed %d" seed) src

let generator_sanity =
  [
    ( "generator is deterministic" >:: fun _ ->
      assert_equal ~printer:Fun.id (gen_program 7) (gen_program 7) );
    ( "generator produces different programs" >:: fun _ ->
      assert_bool "seeds 1 and 2 differ" (gen_program 1 <> gen_program 2) );
  ]

let suite =
  "fuzz_differential" >::: [ "sanity" >::: generator_sanity; "programs" >::: List.init count fuzz_case ]

let () = run_test_tt_main suite
