inter Show <'a> {
  show : 'a -> string
}

impl Show = int {
  show = int_to_str
}

impl Show = bool {
  show = fn b -> if b then "true" else "false"
}

let () = println (__forge_dict_Show_int.show 42)
let () = println (__forge_dict_Show_bool.show true)
