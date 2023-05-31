open Expr



module TypeVar:
sig
  type var
  val fresh: unit -> var
end
=
struct
  type var = int
  let counter: int ref = ref 0
  let fresh (): var =
    counter := !counter + 1; !counter

end

open TypeVar


type t = 
  | IntType
  | BoolType
  | StringType
  | NothingType
  | FunctionType of t * t
  | Var of var

let (=>) (t1: t) (t2: t): t = FunctionType (t1, t2)


let rec string_of_type (t: t) =
  match t with
  | IntType -> "int"
  | BoolType -> "bool"
  | StringType -> "string"
  | NothingType -> "nothing"
  | FunctionType (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | Var _ -> "type_var"


type equation = t * t


module StaticEnv:
sig

  type static_env = (string * t) list
  val get_value: string -> static_env -> t


end
=
struct
  type static_env = (string * t) list

  let get_value (name: string) (env: static_env): t = 
    List.assoc name env

end

open StaticEnv


type type_constraint = t * t
type constraints = type_constraint list


let get_pat_type (p: pat) (env: static_env): t * constraints =
  ignore env;
  match p with
  | IdPat _ -> 
    let new_type: t = Var (fresh ()) in
    new_type, []
  | NothingPat -> (NothingType, [])



let rec get_type (e: expr) (env: static_env): t * constraints =
  ignore env;
  match e with
  | _ when expr_is_integer e -> IntType, []
  | _ when expr_is_boolean e -> BoolType, []
  | _ when expr_is_string e -> StringType, []



  | _ -> failwith "get_type: not implemented"



and expr_is_integer: expr -> bool =
  function
  | DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr (
            Term (
              Factor (
                Integer _
              )
            )
          )
        )
      )
    )
  ) -> true
  | _ -> false


  and expr_is_boolean: expr -> bool =
  function
  | DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr (
            Term (
              Factor (
                Boolean _
              )
            )
          )
        )
      )
    )
  ) -> true
  | _ -> false

and expr_is_string: expr -> bool =
  function
  | DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr (
            Term (
              Factor (
                String _
              )
            )
          )
        )
      )
    )
  ) -> true
  | _ -> false

and expr_is_app: expr -> bool =
  function
  | DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr (
            Term (
              Factor _
            )
          )
        )
      )
    )
  ) -> true
  | _ -> false


and expr_is_function: expr -> bool =
  function
  | Function _ -> true
  | _ -> false




