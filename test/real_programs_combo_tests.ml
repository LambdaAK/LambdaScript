open OUnit2

module Harness = struct
  type state =
    Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

  let parse_program (source : string) : Language.Expr.defn list =
    let input = source |> String.to_seq |> List.of_seq in
    let tokens =
      Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
    in
    match Language.Parser.ProgramParser.program_parser tokens with
    | Some (program, []) -> program
    | Some (_, rem) ->
        failwith
          (Printf.sprintf "Program parse left %d trailing tokens"
             (List.length rem))
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

  let run_program_interpreter_style (program_src : string) : state =
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
                    ("Evaluation error: "
                   ^ Language.Ceval.string_of_eval_error e)
            in
            ( new_bindings @ static_env,
              new_dynamic_bindings @ dynamic_env,
              new_type_bindings @ type_env ))
      (static_env, dynamic_env, type_env) c_program

  let eval_expr_in_state
      ((static_env, dynamic_env, type_env) : state)
      (expr_src : string) : Language.Cexpr.value =
    let expr = parse_expr expr_src |> Language.Condense.condense_expr in
    let elaborated =
      Language.Typecheck.elaborate_expr ~rewrite_constrained_calls:true
        static_env type_env expr
    in
    match Language.Ceval.eval_c_expr elaborated dynamic_env with
    | Language.Ceval.Ok v -> v
    | Language.Ceval.Error e ->
        failwith ("Evaluation error: " ^ Language.Ceval.string_of_eval_error e)

  let assert_expr_int ~state ~(expr : string) ~(expected : int) : unit =
    let value = eval_expr_in_state state expr in
    let got = Language.Ceval.string_of_value value in
    assert_equal ~printer:Fun.id (string_of_int expected) got
end

let list_literal_int (xs : int list) : string =
  match xs with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"

let list_slug_int (xs : int list) : string =
  match xs with
  | [] -> "nil"
  | _ -> String.concat "_" (List.map string_of_int xs)

(* ========================================================================= *)
(* Program 1: Budget / Ledger *)
(* ========================================================================= *)

type txn = Deposit of int | Withdrawal of int | Fee of int

let txn_literal = function
  | Deposit n -> Printf.sprintf "Deposit %d" n
  | Withdrawal n -> Printf.sprintf "Withdrawal %d" n
  | Fee n -> Printf.sprintf "Fee %d" n

let txn_list_literal (txns : txn list) : string =
  match txns with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map txn_literal txns) ^ "]"

let txn_list_slug (txns : txn list) : string =
  if txns = [] then "nil"
  else
    txns
    |> List.map (function
         | Deposit n -> "d" ^ string_of_int n
         | Withdrawal n -> "w" ^ string_of_int n
         | Fee n -> "f" ^ string_of_int n)
    |> String.concat "_"

let eval_txn (balance : int) (tx : txn) : int =
  match tx with
  | Deposit n -> balance + n
  | Withdrawal n -> balance - n
  | Fee n -> balance - n

let apply_all_expected (start : int) (txns : txn list) : int =
  List.fold_left eval_txn start txns

let ledger_program =
  {|
type Txn =
  | Deposit of Int
  | Withdrawal of Int
  | Fee of Int

let apply_txn balance tx =
  case tx do
  | Deposit n -> balance + n
  | Withdrawal n -> balance - n
  | Fee n -> balance - n

let rec apply_all balance txns =
  case txns do
  | [] -> balance
  | h :: t -> apply_all (apply_txn balance h) t

let rec sum_ints xs =
  case xs do
  | [] -> 0
  | h :: t -> h + sum_ints t

let report_balance start txns =
  let final_balance = apply_all start txns in
  if final_balance < 0 then 0 else final_balance

let rec list_append xs ys =
  case xs do
  | [] -> ys
  | h :: t -> h :: list_append t ys

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t
|}

let ledger_starts = [ 0; 1; 5; 10; 25; 50; 100; 250 ]

let ledger_txn_batches =
  [
    [];
    [ Deposit 3 ];
    [ Withdrawal 2 ];
    [ Fee 1 ];
    [ Deposit 10; Withdrawal 3 ];
    [ Deposit 20; Fee 2; Withdrawal 5 ];
    [ Withdrawal 8; Deposit 13; Fee 1 ];
    [ Deposit 34; Deposit 55; Withdrawal 21 ];
    [ Withdrawal 144; Fee 8; Deposit 89 ];
    [ Deposit 233; Withdrawal 144; Withdrawal 13; Fee 5 ];
    [ Deposit 377; Fee 34; Withdrawal 55; Deposit 21 ];
    [ Withdrawal 610; Deposit 377; Fee 21; Deposit 55 ];
  ]

let ledger_int_lists =
  [
    [];
    [ 0 ];
    [ 1; 2 ];
    [ 3; 5; 8 ];
    [ 13; 21; 34 ];
    [ 55; 89 ];
    [ 1; 1; 2; 3; 5 ];
    [ 10; 20; 30; 40 ];
    [ 7; 7; 7; 7 ];
    [ 233; 377; 610 ];
  ]

let ledger_state = lazy (Harness.run_program_interpreter_style ledger_program)

let ledger_apply_all_tests : test list =
  List.concat
    (List.map
       (fun start ->
         List.map
           (fun txns ->
             (Printf.sprintf "ledger_apply_all_%d__%s" start
                (txn_list_slug txns))
             >:: fun _ ->
             let expr =
               Printf.sprintf "apply_all %d %s" start (txn_list_literal txns)
             in
             Harness.assert_expr_int ~state:(Lazy.force ledger_state) ~expr
               ~expected:(apply_all_expected start txns))
           ledger_txn_batches)
       ledger_starts)

let ledger_report_balance_tests : test list =
  List.concat
    (List.map
       (fun start ->
         List.map
           (fun txns ->
             (Printf.sprintf "ledger_report_balance_%d__%s" start
                (txn_list_slug txns))
             >:: fun _ ->
             let expr =
               Printf.sprintf "report_balance %d %s" start
                 (txn_list_literal txns)
             in
             let expected = max 0 (apply_all_expected start txns) in
             Harness.assert_expr_int ~state:(Lazy.force ledger_state) ~expr
               ~expected)
           ledger_txn_batches)
       ledger_starts)

let ledger_fold_sum_int_tests : test list =
  List.map
    (fun xs ->
      (Printf.sprintf "ledger_fold_sum_int_%s" (list_slug_int xs)) >:: fun _ ->
      let expr = Printf.sprintf "sum_ints %s" (list_literal_int xs) in
      let expected = List.fold_left ( + ) 0 xs in
      Harness.assert_expr_int ~state:(Lazy.force ledger_state) ~expr ~expected)
    ledger_int_lists

let ledger_append_len_tests : test list =
  List.concat
    (List.map
       (fun left ->
         List.map
           (fun right ->
             (Printf.sprintf "ledger_append_len_%s__%s" (list_slug_int left)
                (list_slug_int right))
             >:: fun _ ->
             let expr =
               Printf.sprintf "len (list_append %s %s)" (list_literal_int left)
                 (list_literal_int right)
             in
             let expected = List.length left + List.length right in
             Harness.assert_expr_int ~state:(Lazy.force ledger_state) ~expr
               ~expected)
           ledger_int_lists)
       ledger_int_lists)

(* ========================================================================= *)
(* Program 2: Inventory *)
(* ========================================================================= *)

type item = { name : string; qty : int; price : int }

let item_literal (it : item) : string =
  Printf.sprintf "Item (\"%s\", %d, %d)" it.name it.qty it.price

let item_list_literal (items : item list) : string =
  match items with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map item_literal items) ^ "]"

let item_list_slug (items : item list) : string =
  if items = [] then "nil"
  else
    items
    |> List.map (fun it -> Printf.sprintf "%s_%d_%d" it.name it.qty it.price)
    |> String.concat "__"

let inventory_total_qty (items : item list) : int =
  List.fold_left (fun acc it -> acc + it.qty) 0 items

let inventory_total_value (items : item list) : int =
  List.fold_left (fun acc it -> acc + (it.qty * it.price)) 0 items

let inventory_low_stock_count ~(threshold : int) (items : item list) : int =
  List.fold_left
    (fun acc it -> if it.qty <= threshold then acc + 1 else acc)
    0 items

let inventory_discounted_total ~(percent : int) (items : item list) : int =
  let discount_price p = (p * (100 - percent)) / 100 in
  List.fold_left (fun acc it -> acc + (it.qty * discount_price it.price)) 0 items

let inventory_program =
  {|
type Item = | Item of (String, Int, Int)

let qty item =
  case item do
  | Item (_, q, _) -> q

let price item =
  case item do
  | Item (_, _, p) -> p

let rec total_qty items =
  case items do
  | [] -> 0
  | h :: t -> qty h + total_qty t

let rec total_value items =
  case items do
  | [] -> 0
  | h :: t -> qty h * price h + total_value t

let rec low_stock_count threshold items =
  case items do
  | [] -> 0
  | h :: t ->
      if qty h <= threshold then 1 + low_stock_count threshold t
      else low_stock_count threshold t

let rec apply_discount percent items =
  case items do
  | [] -> []
  | Item (name, q, p) :: t ->
      Item (name, q, (p * (100 - percent)) / 100) :: apply_discount percent t
|}

let inventory_batches =
  [
    [];
    [ { name = "apple"; qty = 5; price = 120 } ];
    [ { name = "cable"; qty = 2; price = 350 } ];
    [
      { name = "apple"; qty = 5; price = 120 };
      { name = "cable"; qty = 2; price = 350 };
      { name = "pen"; qty = 8; price = 30 };
      { name = "notebook"; qty = 1; price = 450 };
    ];
    [
      { name = "mouse"; qty = 3; price = 900 };
      { name = "keyboard"; qty = 4; price = 1500 };
    ];
    [
      { name = "banana"; qty = 12; price = 40 };
      { name = "orange"; qty = 9; price = 60 };
      { name = "pear"; qty = 4; price = 70 };
    ];
    [
      { name = "monitor"; qty = 1; price = 18000 };
      { name = "dock"; qty = 2; price = 5200 };
      { name = "cable"; qty = 6; price = 300 };
    ];
    [
      { name = "ink"; qty = 10; price = 250 };
      { name = "paper"; qty = 20; price = 30 };
      { name = "folder"; qty = 15; price = 80 };
    ];
    [
      { name = "chair"; qty = 2; price = 8000 };
      { name = "desk"; qty = 1; price = 24000 };
      { name = "lamp"; qty = 3; price = 2200 };
    ];
    [
      { name = "ssd"; qty = 4; price = 11000 };
      { name = "ram"; qty = 7; price = 4500 };
      { name = "cpu"; qty = 2; price = 28000 };
      { name = "cooler"; qty = 5; price = 3200 };
    ];
  ]

let inventory_thresholds = [ 0; 1; 2; 3; 5; 10 ]
let inventory_discounts = [ 0; 10; 25; 50 ]
let inventory_state = lazy (Harness.run_program_interpreter_style inventory_program)

let inventory_total_qty_tests : test list =
  List.map
    (fun items ->
      (Printf.sprintf "inventory_total_qty_%s" (item_list_slug items))
      >:: fun _ ->
      let expr = Printf.sprintf "total_qty %s" (item_list_literal items) in
      let expected = inventory_total_qty items in
      Harness.assert_expr_int ~state:(Lazy.force inventory_state) ~expr ~expected)
    inventory_batches

let inventory_total_value_tests : test list =
  List.map
    (fun items ->
      (Printf.sprintf "inventory_total_value_%s" (item_list_slug items))
      >:: fun _ ->
      let expr = Printf.sprintf "total_value %s" (item_list_literal items) in
      let expected = inventory_total_value items in
      Harness.assert_expr_int ~state:(Lazy.force inventory_state) ~expr ~expected)
    inventory_batches

let inventory_low_stock_tests : test list =
  List.concat
    (List.map
       (fun threshold ->
         List.map
           (fun items ->
             (Printf.sprintf "inventory_low_stock_%d__%s" threshold
                (item_list_slug items))
             >:: fun _ ->
             let expr =
               Printf.sprintf "low_stock_count %d %s" threshold
                 (item_list_literal items)
             in
             let expected = inventory_low_stock_count ~threshold items in
             Harness.assert_expr_int ~state:(Lazy.force inventory_state) ~expr
               ~expected)
           inventory_batches)
       inventory_thresholds)

let inventory_discount_tests : test list =
  List.concat
    (List.map
       (fun percent ->
         List.map
           (fun items ->
             (Printf.sprintf "inventory_discount_total_%d__%s" percent
                (item_list_slug items))
             >:: fun _ ->
             let expr =
               Printf.sprintf "total_value (apply_discount %d %s)" percent
                 (item_list_literal items)
             in
             let expected = inventory_discounted_total ~percent items in
             Harness.assert_expr_int ~state:(Lazy.force inventory_state) ~expr
               ~expected)
           inventory_batches)
       inventory_discounts)

(* ========================================================================= *)
(* Program 3: Route Analytics *)
(* ========================================================================= *)

type stop = { label : string; dist : int }

let stop_literal (s : stop) : string =
  Printf.sprintf "Stop (\"%s\", %d)" s.label s.dist

let stop_list_literal (stops : stop list) : string =
  match stops with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map stop_literal stops) ^ "]"

let stop_list_slug (stops : stop list) : string =
  if stops = [] then "nil"
  else
    stops
    |> List.map (fun s -> Printf.sprintf "%s_%d" s.label s.dist)
    |> String.concat "__"

let route_distance_expected (stops : stop list) : int =
  List.fold_left (fun acc s -> acc + s.dist) 0 stops

let max_leg_expected (stops : stop list) : int =
  List.fold_left (fun acc s -> max acc s.dist) 0 stops

let count_over_expected ~(limit : int) (stops : stop list) : int =
  List.fold_left (fun acc s -> if s.dist > limit then acc + 1 else acc) 0 stops

let route_program =
  {|
type Stop = | Stop of (String, Int)

let leg stop =
  case stop do
  | Stop (_, d) -> d

let rec route_distance stops =
  case stops do
  | [] -> 0
  | h :: t -> leg h + route_distance t

let rec max_leg stops =
  case stops do
  | [] -> 0
  | h :: t ->
      let d = leg h in
      let m = max_leg t in
      if d > m then d else m

let rec count_over limit stops =
  case stops do
  | [] -> 0
  | h :: t ->
      if leg h > limit then 1 + count_over limit t
      else count_over limit t

let route_score stops =
  let d = route_distance stops in
  if d > 50 then 3 else if d > 20 then 2 else 1
|}

let route_batches =
  [
    [];
    [ { label = "A"; dist = 3 } ];
    [ { label = "A"; dist = 5 }; { label = "B"; dist = 12 } ];
    [
      { label = "A"; dist = 5 };
      { label = "B"; dist = 12 };
      { label = "C"; dist = 7 };
      { label = "D"; dist = 20 };
      { label = "E"; dist = 3 };
    ];
    [
      { label = "X"; dist = 1 };
      { label = "Y"; dist = 1 };
      { label = "Z"; dist = 1 };
      { label = "Q"; dist = 1 };
    ];
    [
      { label = "North"; dist = 34 };
      { label = "East"; dist = 21 };
      { label = "South"; dist = 13 };
    ];
    [
      { label = "R1"; dist = 8 };
      { label = "R2"; dist = 8 };
      { label = "R3"; dist = 8 };
      { label = "R4"; dist = 8 };
    ];
    [
      { label = "L1"; dist = 55 };
      { label = "L2"; dist = 13 };
      { label = "L3"; dist = 5 };
    ];
    [
      { label = "S1"; dist = 2 };
      { label = "S2"; dist = 3 };
      { label = "S3"; dist = 5 };
      { label = "S4"; dist = 8 };
      { label = "S5"; dist = 13 };
    ];
    [
      { label = "M1"; dist = 89 };
      { label = "M2"; dist = 34 };
      { label = "M3"; dist = 21 };
      { label = "M4"; dist = 13 };
    ];
  ]

let route_limits = [ 0; 3; 5; 8; 13; 21 ]
let route_state = lazy (Harness.run_program_interpreter_style route_program)

let route_distance_tests : test list =
  List.map
    (fun stops ->
      (Printf.sprintf "route_distance_%s" (stop_list_slug stops)) >:: fun _ ->
      let expr = Printf.sprintf "route_distance %s" (stop_list_literal stops) in
      let expected = route_distance_expected stops in
      Harness.assert_expr_int ~state:(Lazy.force route_state) ~expr ~expected)
    route_batches

let route_max_leg_tests : test list =
  List.map
    (fun stops ->
      (Printf.sprintf "route_max_leg_%s" (stop_list_slug stops)) >:: fun _ ->
      let expr = Printf.sprintf "max_leg %s" (stop_list_literal stops) in
      let expected = max_leg_expected stops in
      Harness.assert_expr_int ~state:(Lazy.force route_state) ~expr ~expected)
    route_batches

let route_count_over_tests : test list =
  List.concat
    (List.map
       (fun limit ->
         List.map
           (fun stops ->
             (Printf.sprintf "route_count_over_%d__%s" limit
                (stop_list_slug stops))
             >:: fun _ ->
             let expr =
               Printf.sprintf "count_over %d %s" limit
                 (stop_list_literal stops)
             in
             let expected = count_over_expected ~limit stops in
             Harness.assert_expr_int ~state:(Lazy.force route_state) ~expr
               ~expected)
           route_batches)
       route_limits)

let route_score_tests : test list =
  List.map
    (fun stops ->
      (Printf.sprintf "route_score_%s" (stop_list_slug stops)) >:: fun _ ->
      let expr = Printf.sprintf "route_score %s" (stop_list_literal stops) in
      let d = route_distance_expected stops in
      let expected = if d > 50 then 3 else if d > 20 then 2 else 1 in
      Harness.assert_expr_int ~state:(Lazy.force route_state) ~expr ~expected)
    route_batches

let suite =
  "real_programs_combo"
  >::: (ledger_apply_all_tests @ ledger_report_balance_tests
       @ ledger_fold_sum_int_tests @ ledger_append_len_tests
       @ inventory_total_qty_tests
       @ inventory_total_value_tests @ inventory_low_stock_tests
       @ inventory_discount_tests @ route_distance_tests @ route_max_leg_tests
       @ route_count_over_tests @ route_score_tests)

let () = run_test_tt_main suite
