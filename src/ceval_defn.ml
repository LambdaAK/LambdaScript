open New_cexpr

let eval_defn (d : c_defn) (env : env) (static_env : (string * c_type) list) :
    (string * value) list * (string * c_type) list * string list =
  ignore (d, env, static_env);
  failwith "unimplemented: eval_defn"
