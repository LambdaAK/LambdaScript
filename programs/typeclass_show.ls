inter Show <'a> {
  val show : 'a -> string
}

impl Show for int {
  let show = int_to_str
}

impl Show for bool {
  let show = fn b -> if b then "true" else "false"
}

let () = println (__forge_dict_Show_int.show 42)
let () = println (__forge_dict_Show_bool.show true)
