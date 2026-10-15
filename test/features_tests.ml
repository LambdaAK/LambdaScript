(** Traits/impls, macro_rules!, modules and imports — success paths through
    both back ends, and error paths through the interpreter executable. *)

open OUnit2
open Forge_test_util

let ok name src expected = name >:: fun _ -> assert_both ~expected name src
let bad ?(mentions = []) name src = name >:: fun _ -> assert_clean_error ~mentions name src (interp src)

(* ------------------------------------------------------------- traits --- *)

let size_trait = "trait Size<a> where\n  val size : a -> Int\nend\n"

let pretty_trait =
  "trait Pretty<a> where\n  val pretty : a -> String\nend\nimpl Pretty for Int where\n  pretty n = int_to_str n\nend\nimpl Pretty for String where\n  pretty s = s\nend\nimpl Pretty for List<a> requires Pretty<a> where\n  pretty xs = case xs do | [] -> \".\" | h :: t -> pretty h ^ \";\" ^ pretty t\nend\n"

let trait_tests =
  [
    ok "parametric instance used at several element types" (pretty_trait
     ^ "let () = println (pretty [1, 2])\nlet () = println (pretty [\"a\", \"b\"])\nlet () = println (pretty [[1], [2, 3]])")
      "1;2;.\na;b;.\n1;.;2;3;.;.\n";
    ok "constrained function passed a parametric instance" (pretty_trait
     ^ "let twice x = pretty x ^ pretty x\nlet () = println (twice [\"x\"])")
      "x;.x;.\n";
    ok "constrained local function used at two types" (pretty_trait
     ^ "let f u = let p x = pretty x in p 1 ^ p \"a\"\nlet () = println (f ())")
      "1a\n";
    ok "recursive method of an instance on a ground type"
      "trait Join<a> where\n  val join : a -> a -> a\nend\nimpl Join for List<Int> where\n  join xs ys = case xs do | [] -> ys | h :: t -> h :: join t ys\nend\nlet () = println (join [1, 2] [3])"
      "[1, 2, 3]\n";
    ok "instance on a recursive type calls itself"
      "type rec Chain<a> = | End | Link of (a, Chain<a>)\nimpl Show for Chain<a> requires Show<a> where\n  show c = case c do | End -> \"end\" | Link (x, rest) -> show x ^ \" > \" ^ show rest\nend\nlet () = println (Link (1, Link (2, End)))"
      "1 > 2 > end\n";
    ok "show of lists of strings and of an empty list"
      "let () = println [\"a\", \"b\"]\nlet () = println (show [])"
      "[a, b]\n[]\n";
    ok "simple trait and impl"
      (size_trait ^ "impl Size for String where\n  size s = str_length s\nend\nlet () = println (size \"abcd\")")
      "4\n";
    ok "two impls dispatch by type"
      (size_trait
     ^ "impl Size for Int where\n  size n = n\nend\nimpl Size for Bool where\n  size b = if b then 1 else 0\nend\nlet () = println (size 7 + size true)"
      )
      "8\n";
    ok "impl for list type"
      (size_trait ^ "impl Size for List<a> where\n  size l = list_length l\nend\nlet () = println (size [1, 2, 3])")
      "3\n";
    ok "impl for user adt"
      (size_trait
     ^ "type Shape = | Sq of Int | Rect of (Int, Int)\nimpl Size for Shape where\n  size s = case s do | Sq w -> w * w | Rect (w, h) -> w * h\nend\nlet () = println (size (Rect (2, 5)))"
      )
      "10\n";
    ok "constrained polymorphic function"
      (size_trait
     ^ "impl Size for Int where\n  size n = n\nend\nimpl Size for String where\n  size s = str_length s\nend\nlet double_size x = size x * 2\nlet () = println (double_size 4 + double_size \"ab\")"
      )
      "12\n";
    ok "explicit constraint annotation"
      (size_trait
     ^ "impl Size for Int where\n  size n = n\nend\nlet twice [Size a] (x : a) = size x + size x\nlet () = println (twice 21)"
      )
      "42\n";
    ok "default method"
      "trait Greet<a> where\n  val name : a -> String\n  val greet : a -> String\n  let greet x = \"hello \" ^ name x\nend\nimpl Greet for Int where\n  name n = int_to_str n\nend\nlet () = print_string (greet 5)"
      "hello 5\n";
    ok "overridden default method"
      "trait Greet<a> where\n  val name : a -> String\n  val greet : a -> String\n  let greet x = \"hello \" ^ name x\nend\nimpl Greet for Int where\n  name n = int_to_str n,\n  greet n = \"hi \" ^ name n\nend\nlet () = print_string (greet 5)"
      "hi 5\n";
    ok "operator method"
      "trait Combine<a> where\n  val (<+>) : a -> a -> a\nend\nimpl Combine for Int where\n  (<+>) a b = a * 10 + b\nend\nlet () = println (1 <+> 2)"
      "12\n";
    ok "superclass requires"
      "trait Named<a> where\n  val name : a -> String\nend\ntrait Loud<a> requires Named<a> where\n  val shout : a -> String\nend\nimpl Named for Int where\n  name n = int_to_str n\nend\nimpl Loud for Int where\n  shout n = name n ^ \"!\"\nend\nlet () = print_string (shout 3)"
      "3!\n";
    ok "impl with requires on element type"
      (size_trait
     ^ "impl Size for Int where\n  size n = n\nend\nimpl Size for List<a> requires Size<a> where\n  size l = reduce_left (fn acc -> fn x -> acc + size x) 0 l\nend\nlet () = println (size [1, 2, 3])"
      )
      "6\n";
    ok "recursive method"
      "trait Depth<a> where\n  val depth : a -> Int\nend\ntype rec T = | L | N of (T, T)\nimpl Depth for T where\n  depth t = case t do | L -> 0 | N (a, b) -> 1 + (if depth a > depth b then depth a else depth b)\nend\nlet () = println (depth (N (N (L, L), L)))"
      "2\n";
    ok "higher-kinded trait"
      "trait Container<f<_>> where\n  val cmap : (a -> b) -> f<a> -> f<b>\nend\ntype Box<a> = | Box of a\nimpl Container for Box where\n  cmap g b = case b do | Box x -> Box (g x)\nend\nlet unbox b = case b do | Box x -> x\nlet () = println (unbox (cmap (fn x -> x + 1) (Box 41)))"
      "42\n";
    ok "prelude Functor for Option"
      "let () = println (case fmap (fn x -> x * 2) (Some 21) do | Some v -> v | None -> 0)" "42\n";
    ok "prelude Monad bind for Option"
      "let half x = if x % 2 == 0 then Some (x / 2) else None\nlet () = println (case bind (Some 8) half do | Some v -> v | None -> 0)"
      "4\n";
    ok "prelude >>= chain"
      "let half x = if x % 2 == 0 then Some (x / 2) else None\nlet r = Some 16 >>= half >>= half\nlet () = println (case r do | Some v -> v | None -> 0)"
      "4\n";
    ok "prelude Alternative"
      "let r = None <|> Some 3\nlet () = println (case r do | Some v -> v | None -> 0)" "3\n";
    ok "user Show instance used by println"
      "type P = | P of (Int, Int)\nimpl Show for P where\n  show p = case p do | P (a, b) -> \"P(\" ^ int_to_str a ^ \",\" ^ int_to_str b ^ \")\"\nend\nlet () = println (P (1, 2))"
      "P(1,2)\n";
    ok "user Show instance inside list"
      "type P = | P of Int\nimpl Show for P where\n  show p = case p do | P a -> \"P\" ^ int_to_str a\nend\nlet () = println [P 1, P 2]"
      "[P1, P2]\n";
    (* [==] syntax is built-in structural equality; the Eq instance is used
       when the method is called as [(==)] (see BUGS.md open question Q2). *)
    ok "user Eq instance via method"
      "type M = | M of Int\nimpl Eq for M where\n  (==) a b = case (a, b) do | (M x, M y) -> x % 10 == y % 10\nend\nlet () = println ((==) (M 13) (M 23))"
      "true\n";
    ok "Semigroup for user type"
      "type Sum = | Sum of Int\nimpl Semigroup for Sum where\n  mappend a b = case (a, b) do | (Sum x, Sum y) -> Sum (x + y),\n  (++) a b = mappend a b\nend\nlet () = println (case Sum 1 ++ Sum 2 do | Sum v -> v)"
      "3\n";
    ok "Monoid empty for String" "let () = print_string (empty ^ \"x\")" "x\n";
    ok "inter brace syntax"
      "inter Size<a> {\n  val size : a -> Int\n}\nimpl Size for Int {\n  size n = n + 1\n}\nlet () = println (size 1)"
      "2\n";
  ]

let trait_errors =
  [
    bad ~mentions:[ "instance" ] "missing impl for type"
      (size_trait ^ "impl Size for Int where\n  size n = n\nend\nlet () = println (size true)");
    bad ~mentions:[ "String"; "Int" ] "an annotated pattern is checked"
      "let f o = case o do | Some (x : Int) -> x | None -> 0\nlet () = println (f (Some \"s\"))";
    bad ~mentions:[ "refers to itself" ] "a record type in a type rec group cannot refer to itself"
      "type rec Tree = { label: Int, kids: List<Tree> }";
    bad "impl of unknown trait" "impl Nope for Int where\n  f x = x\nend";
    bad "impl missing method"
      "trait T<a> where\n  val f : a -> Int\n  val g : a -> Int\nend\nimpl T for Int where\n  f x = x\nend";
    bad "impl unknown method" (size_trait ^ "impl Size for Int where\n  size n = n,\n  extra n = n\nend");
    bad ~mentions:[ "duplicate impl of Size for Int" ] "overlapping impls" (size_trait ^ "impl Size for Int where\n  size n = n\nend\nimpl Size for Int where\n  size n = 0\nend");
    bad "impl method with wrong type" (size_trait ^ "impl Size for Int where\n  size n = \"no\"\nend");
    bad "duplicate trait" (size_trait ^ size_trait);
    bad "trait without type parameter" "trait T where\n  val f : Int\nend";
    bad "impl before trait" ("impl Size for Int where\n  size n = n\nend\n" ^ size_trait);
    bad "requires unknown trait" "trait T<a> requires Nope<a> where\n  val f : a -> Int\nend";
    bad "default for undeclared method" "trait T<a> where\n  val f : a -> Int\n  let g x = 1\nend";
    bad "duplicate method in impl" (size_trait ^ "impl Size for Int where\n  size n = n,\n  size n = 0\nend");
    bad "superclass instance missing"
      "trait A<a> where\n  val fa : a -> Int\nend\ntrait B<a> requires A<a> where\n  val fb : a -> Int\nend\nimpl B for Int where\n  fb x = fa x\nend\nlet () = println (fb 1)";
    bad "redefining a prelude trait" "trait Show<a> where\n  val show : a -> String\nend";
  ]

(* ------------------------------------------------------------- macros --- *)

let macro_tests =
  [
    ok "simple macro" "macro_rules! twice where\n  ($e:expr) => $e + $e\nend\nlet () = println (twice!(21))" "42\n";
    ok "macro hygiene of precedence"
      "macro_rules! twice where\n  ($e:expr) => $e * 2\nend\nlet () = println (twice!(1 + 2))" "6\n";
    ok "macro with multiple arms" "macro_rules! choose where\n  () => 0;\n  ($x:expr) => $x\nend\nlet () = println (choose!() + choose!(5))"
      "5\n";
    ok "macro brackets and braces"
      "macro_rules! add where\n  ($a:expr, $b:expr) => $a + $b\nend\nlet () = println (add!(1, 2) + add![3, 4] + add!{5, 6})" "21\n";
    ok "repetition collects list" "macro_rules! collect where\n  ($($x:expr),*) => $x\nend\nlet () = println (list_length (collect!(1, 2, 3, 4)))"
      "4\n";
    ok "repetition transcribed" "macro_rules! sum where\n  ($($x:expr),*) => 0 $(+ $x)*\nend\nlet () = println (sum!(1, 2, 3))" "6\n";
    ok "count_args builtin" "let () = println (count_args!(a, b, c))" "3\n";
    ok "count_args empty" "let () = println (count_args!())" "0\n";
    ok "vec builtin" "let () = println (list_length (vec!(1, 2, 3)))" "3\n";
    (* README: stringify! yields "a string representation of argument
       expression ASTs" (not source text), so only check it is a string that
       mentions both operands. *)
    ( "stringify builtin" >:: fun _ ->
      let src = "let s = stringify!(1 + 2)\nlet () = println (str_length s > 0)" in
      assert_both ~expected:"true\n" "stringify" src;
      let r = interp "let () = print_string (stringify!(17 + 25))" in
      assert_bool ("mentions operands: " ^ r.out) (contains r.out "17" && contains r.out "25") );
    ok "concat builtin" "let () = print_string (concat!(\"a\", \"b\", \"c\"))" "abc\n";
    ok "macro using another macro" "macro_rules! one where\n  () => 1\nend\nmacro_rules! two where\n  () => one!() + one!()\nend\nlet () = println (two!())"
      "2\n";
    ok "macro ident fragment" "macro_rules! get where\n  ($x:ident) => $x\nend\nlet value = 9\nlet () = println (get!(value))" "9\n";
    ok "macro in module" "mod M where\n  macro_rules! inc where\n    ($x:expr) => $x + 1\n  end\n  let v = inc!(1)\nend\nlet () = println M.v"
      "2\n";
    ok "macro generating a lambda" "macro_rules! adder where\n  ($n:expr) => fn x -> x + $n\nend\nlet () = println ((adder!(10)) 5)" "15\n";
    ok "recursive macro" "macro_rules! sum where\n  () => 0;\n  ($x:expr) => $x;\n  ($x:expr, $($rest:expr),+) => $x + sum!($($rest),+)\nend\nlet () = println (sum!(1, 2, 3, 4))"
      "10\n";
    ok "macro argument evaluated once per use" "macro_rules! dup where\n  ($e:expr) => ($e, $e)\nend\nlet () = println (tuple_fst (dup!(3 * 3)))"
      "9\n";
  ]

let macro_errors =
  [
    bad "unknown macro" "let x = nope!(1)";
    bad "no matching arm" "macro_rules! one where\n  ($x:expr) => $x\nend\nlet x = one!(1, 2)";
    bad "macro with no arms" "macro_rules! m where\nend";
    bad "infinitely recursive macro" "macro_rules! loop where\n  ($x:expr) => loop!($x)\nend\nlet x = loop!(1)";
    bad "macro expanding to ill-typed code" "macro_rules! bad where\n  () => 1 + \"a\"\nend\nlet x = bad!()";
    bad "macro inside expression block" "let x = { macro_rules! m where () => 1 end; m!() }";
  ]

(* ------------------------------------------------------------ modules --- *)

let module_tests =
  [
    ok "qualified access" "mod M where\n  let x = 5\nend\nlet () = println M.x" "5\n";
    ok "qualified function call" "mod M where\n  let sq x = x * x\nend\nlet () = println (M.sq 7)" "49\n";
    ok "use brings names into scope" "mod M where\n  let sq x = x * x\nend\nuse M\nlet () = println (sq 3)" "9\n";
    ok "nested modules" "mod A where\n  mod B where\n    let v = 3\n  end\nend\nlet () = println A.B.v" "3\n";
    ok "use nested module" "mod A where\n  mod B where\n    let v = 3\n  end\nend\nuse A.B\nlet () = println v" "3\n";
    ok "module-local reference" "mod M where\n  let a = 2\n  let b = a * 10\nend\nlet () = println M.b" "20\n";
    ok "module types and constructors"
      "mod Shapes where\n  type S = | Sq of Int | Ci of Int\n  let area s = case s do | Sq w -> w * w | Ci r -> 3 * r * r\nend\nlet () = println (Shapes.area (Shapes.Sq 4))"
      "16\n";
    ok "same name in two modules" "mod A where\n  let x = 1\nend\nmod B where\n  let x = 2\nend\nlet () = println (A.x + B.x)" "3\n";
    ok "qualified constructors in patterns"
      "mod Geo where\n  type Shape = | Sq of Int | Ci of Int\n  mod Inner where\n    type T = | X | Y\n  end\nend\nlet area s = case s do | Geo.Sq w -> w * w | Geo.Ci r -> 3 * r * r\nlet t x = case x do | Geo.Inner.X -> 1 | Geo.Inner.Y -> 2\nlet () = println (area (Geo.Sq 3), area (Geo.Ci 2), t Geo.Inner.Y)"
      "(9, 12, 2)\n";
    ok "shadowed globals across many definitions"
      "let x = 1\nlet f () = x\nlet d0 = 0\nlet d1 = 1\nlet d2 = 2\nlet d3 = 3\nlet d4 = 4\nlet d5 = 5\nlet d6 = 6\nlet d7 = 7\nlet d8 = 8\nlet d9 = 9\nlet d10 = 10\nlet d11 = 11\nlet d12 = 12\nlet d13 = 13\nlet d14 = 14\nlet d15 = 15\nlet d16 = 16\nlet d17 = 17\nlet d18 = 18\nlet d19 = 19\nlet d20 = 20\nlet d21 = 21\nlet d22 = 22\nlet d23 = 23\nlet d24 = 24\nlet d25 = 25\nlet d26 = 26\nlet d27 = 27\nlet d28 = 28\nlet d29 = 29\nlet d30 = 30\nlet d31 = 31\nlet d32 = 32\nlet d33 = 33\nlet d34 = 34\nlet d35 = 35\nlet d36 = 36\nlet d37 = 37\nlet d38 = 38\nlet d39 = 39\nlet x = 2\nlet g () = x\nlet d0 = 0\nlet d1 = 1\nlet d2 = 2\nlet d3 = 3\nlet d4 = 4\nlet d5 = 5\nlet d6 = 6\nlet d7 = 7\nlet d8 = 8\nlet d9 = 9\nlet d10 = 10\nlet d11 = 11\nlet d12 = 12\nlet d13 = 13\nlet d14 = 14\nlet d15 = 15\nlet d16 = 16\nlet d17 = 17\nlet d18 = 18\nlet d19 = 19\nlet d20 = 20\nlet d21 = 21\nlet d22 = 22\nlet d23 = 23\nlet d24 = 24\nlet d25 = 25\nlet d26 = 26\nlet d27 = 27\nlet d28 = 28\nlet d29 = 29\nlet d30 = 30\nlet d31 = 31\nlet d32 = 32\nlet d33 = 33\nlet d34 = 34\nlet d35 = 35\nlet d36 = 36\nlet d37 = 37\nlet d38 = 38\nlet d39 = 39\nlet some = Some\nlet () = println (f (), g (), x, some 3, d39)"
      "(1, 2, 2, Some 3, 39)\n";
    ok "record types in a type rec group"
      "type rec Value = | Num of Int | Clo of Lam\nand Lam = { params: List<String>, env: List<(String, Value)> }\ntype rec Tree<a> = | Leaf | Node of Info<a>\nand Info<a> = { v: a, kids: List<Tree<a>> }\nlet v = Clo { params: [\"x\"], env: [(\"y\", Num 1)] }\nlet l : Lam = { params: [], env: [] }\nlet rec sum t = case t do | Leaf -> 0 | Node i -> i.v + reduce_left (fn a -> fn k -> a + sum k) 0 i.kids\nlet () = println (case v do | Clo r -> list_length r.params + list_length r.env | _ -> 0, list_length l.env, sum (Node { v: 1, kids: [Leaf, Node { v: 2, kids: [] }] }))"
      "(2, 0, 3)\n";
    ok "impl for a named record type"
      "type P = { x: Int, y: Int }\nimpl Show for P where\n  show p = \"P(\" ^ int_to_str p.x ^ \")\"\nend\nlet p : P = { x: 1, y: 2 }\nlet () = print_string (show p)"
      "P(1)\n";
    ok "annotated patterns in case arms, functions and lets"
      "let f o = case o do | Some (x : Int) -> x + 1 | None -> 0\nlet g = fn (a, (b : String)) -> a ^ b\nlet h p = let (x, (_ : Int)) = p in x\nlet () = println (f (Some 41), g (\"a\", \"b\"), h (true, 2))"
      "(42, ab, true)\n";
    ok "named record type in a trait signature"
      "type P = { x: Int }\ntrait Get<m> where\n  val get : m -> P -> Int\nend\nimpl Get for Int where\n  get n p = n + p.x\nend\nlet () = println (get 1 { x: 2 })"
      "3\n";
    ok "module shadows outer name" "let x = 100\nmod M where\n  let x = 1\n  let y = x + 1\nend\nlet () = println (M.y + x)" "102\n";
    ok "recursive function in module"
      "mod M where\n  let rec fact n = if n == 0 then 1 else n * fact (n - 1)\nend\nlet () = println (M.fact 5)" "120\n";
    ok "trait in module" "mod M where\n  trait R<a> where\n    val r : a -> Int\n  end\n  impl R for Int where\n    r x = x + 1\n  end\nend\nuse M\nlet () = println (r 1)"
      "2\n";
    ok "sibling module via use" "mod A where\n  let base = 40\nend\nmod B where\n  use A\n  let out = base + 2\nend\nlet () = println B.out" "42\n";
  ]

let module_errors =
  [
    bad "unknown module in use" "use Missing\nlet x = 1";
    bad "unknown qualified name" "mod M where\n  let x = 1\nend\nlet () = println M.y";
    bad "private name not in scope without use" "mod M where\n  let secret = 1\nend\nlet () = println secret";
    bad "ambiguous use" "mod A where\n  let x = 1\nend\nmod B where\n  let x = 2\nend\nuse A\nuse B\nlet () = println x";
  ]

(* ------------------------------------------------------------ imports --- *)

let mkdir_p dir =
  let rec go d =
    if d = "" || d = "." || d = "/" || Sys.file_exists d then ()
    else (
      go (Filename.dirname d);
      Unix.mkdir d 0o755)
  in
  go dir

(** Write [files] (relative path, contents) into a fresh directory and run the
    interpreter on the first one. *)
let run_files ?(native_too = false) files k =
  with_tmpdir @@ fun dir ->
  List.iter
    (fun (rel, contents) ->
      let path = Filename.concat dir rel in
      mkdir_p (Filename.dirname path);
      write_file path contents)
    files;
  let main = Filename.concat dir (fst (List.hd files)) in
  let ri = run_process ~cwd:(repo ()) (bin_exe "interpreter") [ main ] in
  let rn =
    if native_too then (
      let exe = Filename.concat dir "main.out" in
      let c = run_process ~timeout:120.0 ~cwd:(repo ()) (bin_exe "compile_forge") [ main; exe ] in
      match c.status with Exited 0 -> Some (run_process ~cwd:dir exe []) | _ -> Some c)
    else None
  in
  k ri rn

let import_ok name files expected =
  name >:: fun _ ->
  run_files ~native_too:true files (fun ri rn ->
      let src = String.concat "\n" (List.map (fun (f, c) -> "// " ^ f ^ "\n" ^ c) files) in
      assert_runs ~msg:"[interpreter] " ~expected name src ri;
      match rn with Some rn -> assert_runs ~msg:"[native] " ~expected name src rn | None -> ())

let import_bad ?(mentions = []) name files =
  name >:: fun _ ->
  run_files files (fun ri _ ->
      let src = String.concat "\n" (List.map (fun (f, c) -> "// " ^ f ^ "\n" ^ c) files) in
      assert_clean_error ~mentions name src ri)

let import_tests =
  [
    import_ok "import a sibling file"
      [ ("main.forge", "import \"lib.forge\"\nlet () = println (double 21)"); ("lib.forge", "let double x = x * 2") ]
      "42\n";
    import_ok "import without extension"
      [ ("main.forge", "import \"lib\"\nlet () = println (double 4)"); ("lib.forge", "let double x = x * 2") ]
      "8\n";
    import_ok "import from subdirectory"
      [ ("main.forge", "import \"util/math.forge\"\nlet () = println (sq 5)"); ("util/math.forge", "let sq x = x * x") ]
      "25\n";
    import_ok "transitive import"
      [
        ("main.forge", "import \"b.forge\"\nlet () = println (b_val + 1)");
        ("b.forge", "import \"c.forge\"\nlet b_val = c_val * 10");
        ("c.forge", "let c_val = 4");
      ]
      "41\n";
    import_ok "relative import resolved from importing file"
      [
        ("main.forge", "import \"sub/a.forge\"\nlet () = println a");
        ("sub/a.forge", "import \"b.forge\"\nlet a = b + 1");
        ("sub/b.forge", "let b = 1");
      ]
      "2\n";
    import_ok "diamond import is included once"
      [
        ("main.forge", "import \"l.forge\"\nimport \"r.forge\"\nlet () = println (l + r)");
        ("l.forge", "import \"base.forge\"\nlet l = base + 1");
        ("r.forge", "import \"base.forge\"\nlet r = base + 2");
        ("base.forge", "let base = 10");
      ]
      "23\n";
    import_ok "imported module"
      [ ("main.forge", "import \"m.forge\"\nlet () = println (M.f 2)"); ("m.forge", "mod M where\n  let f x = x + 40\nend") ]
      "42\n";
    import_ok "imported trait and impl"
      [
        ("main.forge", "import \"t.forge\"\nlet () = println (sz 5)");
        ("t.forge", "trait Sz<a> where\n  val sz : a -> Int\nend\nimpl Sz for Int where\n  sz x = x * 2\nend");
      ]
      "10\n";
    import_ok "imported macro"
      [
        ("main.forge", "import \"m.forge\"\nlet () = println (twice!(4))");
        ("m.forge", "macro_rules! twice where\n  ($e:expr) => $e + $e\nend");
      ]
      "8\n";
  ]

let import_errors =
  [
    import_bad ~mentions:[ "not found" ] "missing file" [ ("main.forge", "import \"nope.forge\"\nlet x = 1") ];
    import_bad ~mentions:[ "cycl" ] "import cycle"
      [ ("main.forge", "import \"a.forge\"\nlet x = 1"); ("a.forge", "import \"b.forge\"\nlet a = 1"); ("b.forge", "import \"a.forge\"\nlet b = 1") ];
    import_bad ~mentions:[ "cycl" ] "self import" [ ("main.forge", "import \"main.forge\"\nlet x = 1") ];
    import_bad "parse error in imported file" [ ("main.forge", "import \"bad.forge\"\nlet x = 1"); ("bad.forge", "let = =") ];
    import_bad "type error in imported file" [ ("main.forge", "import \"bad.forge\"\nlet x = 1"); ("bad.forge", "let y = 1 + true") ];
    import_bad "import a directory" [ ("main.forge", "import \"dir\"\nlet x = 1"); ("dir/keep.forge", "let k = 1") ];
  ]

let suite =
  "features"
  >::: [
         "traits" >::: trait_tests;
         "trait_errors" >::: trait_errors;
         "macros" >::: macro_tests;
         "macro_errors" >::: macro_errors;
         "modules" >::: module_tests;
         "module_errors" >::: module_errors;
         "imports" >::: import_tests;
         "import_errors" >::: import_errors;
       ]

let () = run_test_tt_main suite
