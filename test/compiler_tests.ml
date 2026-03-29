open OUnit2

let run_exe_capture_stdout exe =
  let ic = Unix.open_process_args_in exe [| exe |] in
  let contents = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> contents
  | Unix.WEXITED n ->
      assert_failure
        (Printf.sprintf "executable %S exited %d; stdout: %S" exe n contents)
  | Unix.WSIGNALED s ->
      assert_failure
        (Printf.sprintf "executable %S signal %d; stdout: %S" exe s contents)
  | Unix.WSTOPPED s ->
      assert_failure
        (Printf.sprintf "executable %S stopped %d; stdout: %S" exe s contents)

let rm_rf dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)) : int)

let with_tmpdir f =
  (* Avoid [Unix.mkdtemp] for compatibility with older OCaml Unix bindings. *)
  let path = Filename.temp_file "ls_compile_" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

(** Programs paired with the exact bytes expected on stdout (including any
    final newline from [println] / [puts]). *)

let case name src expect = (name, src, expect)

let compiler_cases : (string * string * string) list =
  [
    (* Baseline *)
    case "println string" {|let () = println "hello"|} "hello\n";
    case "minimal let arith"
      {|
let x = 1 + 2
let () = println (int_to_str x)
|}
      "3\n";
    case "user fn add"
      {|
let add x y = x + y
let () = println (int_to_str (add 3 4))
|}
      "7\n";
    case "partial application"
      {|
let add x y = x + y
let g = add 3
let () = println (int_to_str (g 4))
|}
      "7\n";
    case "three-arg curry partial"
      {|
let f a b c = a + b + c
let p = f 1 2
let () = println (int_to_str (p 3))
|}
      "6\n";
    (* Arithmetic *)
    case "subtraction and multiply"
      {|
let () = println (int_to_str (10 - 3 * 2))
|}
      "4\n";
    case "integer divide"
      {|let () = println (int_to_str (7 / 2))|} "3\n";
    case "modulo" {|let () = println (int_to_str (17 % 5))|} "2\n";
    case "paren precedence"
      {|
let () = println (int_to_str ((1 + 2) * (3 + 4)))
|}
      "21\n";
    (* Comparisons and bool ops — && / || evaluate both sides *)
    case "if true branch"
      {|let () = println (int_to_str (if true then 42 else 0))|} "42\n";
    case "if false branch"
      {|let () = println (int_to_str (if false then 100 else 7))|}
      "7\n";
    case "int equality"
      {|let () = println (int_to_str (if 42 == 42 then 1 else 0))|} "1\n";
    case "inequality via =="
      {|let () = println (int_to_str (if 3 == 2 then 0 else 1))|} "1\n";
    case "less and greater"
      {|
let () = println (int_to_str (if (1 < 2) && (4 > 3) then 1 else 0))
|}
      "1\n";
    case "less-eq greater-eq"
      {|
let () =
  println (int_to_str (if (3 <= 3) && (2 >= 5) then 1 else 0))
|}
      "0\n";
    case "bool and or mix"
      {|
let () =
  println
    (int_to_str
       (if (1 <= 2) && (3 >= 3) && ((2 >= 5) || true) then 99 else 0))
|}
      "99\n";
    (* Blocks and nested let *)
    case "let-in for block-style value"
      {|
let () = println (int_to_str (let x = 10 in x + 5))
|}
      "15\n";
    case "brace block sequencing"
      {|
let () = println (int_to_str ({ 10; 11 + 1 }))
|}
      "12\n";
    case "nested let sequence"
      {|
let x =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  a + b + c + d
let () = println (int_to_str x)
|}
      "10\n";
    case "nested if println"
      {|
let () = if false then println "skip" else (
  if true then println "ok" else println "bad"
)
let () = println (int_to_str 0)
|}
      "ok\n0\n";
    (* print vs println *)
    case "print without newline then newline"
      {|
let () = print "x"
let () = print "y"
let () = println ""
|}
      "xy\n";
    (* Recursion *)
    case "factorial"
      {|
let fact n =
  let rec fact_helper n acc =
    if n == 0 then acc
    else fact_helper (n - 1) (n * acc)
  in
  fact_helper n 1

let () = println (int_to_str (fact 5))
|}
      "120\n";
    case "fibonacci"
      {|
let rec fib n =
  if n == 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2)

let () = println (int_to_str (fib 10))
|}
      "55\n";
    case "sum 1..n"
      {|
let rec sum n = if n == 0 then 0 else n + sum (n - 1)
let () = println (int_to_str (sum 100))
|}
      "5050\n";
    case "integer power"
      {|
let rec pow b e =
  if e == 0 then 1
  else b * pow b (e - 1)

let () = println (int_to_str (pow 2 10))
|}
      "1024\n";
    (* Helpers *)
    case "max function"
      {|
let max a b = if a > b then a else b
let () = println (int_to_str (max 3 10))
|}
      "10\n";
    case "abs with subtraction"
      {|
let abs x = if x < 0 then 0 - x else x
let () = println (int_to_str (abs (0 - 12)))
|}
      "12\n";
    case "gcd"
      {|
let rec gcd a b =
  if b == 0 then a
  else gcd b (a % b)

let () = println (int_to_str (gcd 378 273))
|}
      "21\n";
    case "zero"
      {|let () = println (int_to_str (0 + 0))|} "0\n";
    case "several top-level prints in order"
      {|
let () = println (int_to_str 1)
let () = println (int_to_str 2)
let () = println (int_to_str 3)
|}
      "1\n2\n3\n";
    (* Higher-order: typed params so the function argument has a concrete Min_ir type *)
    case "apply function argument"
      {|
let apply (f : int -> int) (x : int) : int = f x
let inc y = y + 1
let () = println (int_to_str (apply inc 41))
|}
      "42\n";
    case "twice higher-order"
      {|
let twice (f : int -> int) (x : int) : int = f (f x)
let add1 z = z + 1
let () = println (int_to_str (twice add1 5))
|}
      "7\n";
    case "immediate lambda call"
      {|
let () = println (int_to_str ((fn x -> x + 1) 8))
|}
      "9\n";
    case "function returned from unit -> (int -> int)"
      {|
let get_inc () : int -> int = fn x -> x + 1
let () = println (int_to_str ((get_inc ()) 40))
|}
      "41\n";
  ]

let test_one (name, program, expected_stdout) =
  name >:: fun _ ->
    with_tmpdir @@ fun dir ->
    let src = Filename.concat dir "prog.ls" in
    let exe = Filename.concat dir "prog_out" in
    let oc = open_out src in
    output_string oc program;
    close_out oc;
    match Language.Compile_pipeline.compile ~quiet:true src exe with
    | Error msg -> assert_failure ("compile failed: " ^ msg)
    | Ok () ->
        let actual = run_exe_capture_stdout exe in
        assert_equal ~printer:(fun s -> Printf.sprintf "%S" s) expected_stdout
          actual

let suite =
  "compiler_integration"
  >::: List.map test_one compiler_cases

let () = run_test_tt_main suite
