(** Typed, monomorphic programs: the input of the native lowering
    ({!Lower_typed}), produced by {!Mono}.

    Every expression and pattern carries its type, which is concrete: no type
    variables, records closed with their fields sorted by name, recursive
    types as [CTypeApp], lists as [CListType]. *)

open Cexpr

type ty = mono_type

type pat = { p : pdesc; pty : ty }

and pdesc =
  | PWild
  | PVar of string
  | PUnit
  | PNil
  | PInt of int
  | PBool of bool
  | PStr of string
  | PChar of char
  | PFloat of float
  | PCons of pat * pat
  | PTuple of pat list
  | PRecord of (string * pat) list  (** a subset of the record's fields *)
  | PCtor of string * int * pat option  (** constructor, tag, payload *)

type expr = { e : desc; ty : ty }

and desc =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Char of char
  | Unit
  | Nil
  | Var of string  (** a local variable *)
  | Global of string  (** a top-level function or value *)
  | Prim of string  (** a runtime primitive ([print_string], [str_slice], ...) *)
  | Ctor of string * int  (** a constructor and its tag *)
  | App of expr * expr
  | Lam of pat * expr
  | Let of pat * expr * expr
  | LetRec of (string * expr) list * expr
  | Case of expr * (pat * expr) list
  | If of expr * expr * expr
  | Bop of c_bop * expr * expr
  | Tuple of expr list
  | Range of expr * expr  (** [\[a...b\]] *)
  | Comp of expr * (pat * expr) list  (** [\[e | p1 <- l1, ...\]] *)
  | Record of (string * expr) list  (** all fields, sorted *)
  | Field of expr * string
  | Update of expr * (string * expr) list

type item =
  | Fun of string * expr  (** a top-level function: its body is a [Lam] *)
  | Val of pat * expr
      (** a top-level value, evaluated in program order; the variables of the
          pattern are globals *)

type program = item list

(** The fields of a record type, sorted by name. *)
let record_fields (t : ty) : (string * ty) list =
  match t with
  | RecordType fs -> List.sort (fun (a, _) (b, _) -> compare a b) fs
  | _ -> invalid_arg "Tast.record_fields"

(** {1 Printing (for debugging)} *)

let show_ty = C_to_string.string_of_mono_type

let rec show_pat (p : pat) : string =
  match p.p with
  | PWild -> "_"
  | PVar x -> x
  | PUnit -> "()"
  | PNil -> "[]"
  | PInt n -> string_of_int n
  | PBool b -> string_of_bool b
  | PStr s -> Printf.sprintf "%S" s
  | PChar c -> Printf.sprintf "%C" c
  | PFloat f -> string_of_float f
  | PCons (a, b) -> "(" ^ show_pat a ^ " :: " ^ show_pat b ^ ")"
  | PTuple ps -> "(" ^ String.concat ", " (List.map show_pat ps) ^ ")"
  | PRecord fs -> "{" ^ String.concat ", " (List.map (fun (f, q) -> f ^ ": " ^ show_pat q) fs) ^ "}"
  | PCtor (c, _, None) -> c
  | PCtor (c, _, Some q) -> "(" ^ c ^ " " ^ show_pat q ^ ")"

let rec show (e : expr) : string =
  match e.e with
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Str s -> Printf.sprintf "%S" s
  | Char c -> Printf.sprintf "%C" c
  | Unit -> "()"
  | Nil -> "[]:" ^ show_ty e.ty
  | Var x -> x
  | Global x -> "@" ^ x
  | Prim x -> "%" ^ x
  | Ctor (c, _) -> c
  | App (a, b) -> "(" ^ show a ^ " " ^ show b ^ ")"
  | Lam (p, b) -> "(fn " ^ show_pat p ^ " : " ^ show_ty p.pty ^ " -> " ^ show b ^ ")"
  | Let (p, a, b) -> "(let " ^ show_pat p ^ " = " ^ show a ^ " in " ^ show b ^ ")"
  | LetRec (bs, b) ->
      "(let rec " ^ String.concat " and " (List.map (fun (x, a) -> x ^ " = " ^ show a) bs) ^ " in " ^ show b ^ ")"
  | Case (s, brs) ->
      "(case " ^ show s ^ " of " ^ String.concat " | " (List.map (fun (p, b) -> show_pat p ^ " -> " ^ show b) brs) ^ ")"
  | If (c, a, b) -> "(if " ^ show c ^ " then " ^ show a ^ " else " ^ show b ^ ")"
  | Bop (_, a, b) -> "(" ^ show a ^ " <op> " ^ show b ^ ")"
  | Tuple es -> "(" ^ String.concat ", " (List.map show es) ^ ")"
  | Range (a, b) -> "[" ^ show a ^ "..." ^ show b ^ "]"
  | Comp (b, gs) -> "[" ^ show b ^ " | " ^ String.concat ", " (List.map (fun (p, s) -> show_pat p ^ " <- " ^ show s) gs) ^ "]"
  | Record fs -> "{" ^ String.concat ", " (List.map (fun (f, x) -> f ^ ": " ^ show x) fs) ^ "}"
  | Field (r, f) -> show r ^ "." ^ f
  | Update (r, fs) -> "{" ^ show r ^ " with " ^ String.concat ", " (List.map (fun (f, x) -> f ^ " = " ^ show x) fs) ^ "}"

let show_item = function
  | Fun (n, e) -> "fun " ^ n ^ " : " ^ show_ty e.ty ^ " = " ^ show e
  | Val (p, e) -> "val " ^ show_pat p ^ " : " ^ show_ty p.pty ^ " = " ^ show e
