inter Show <a> {
  val show : a -> string
}

impl Show for Int where
  show = int_to_str
end

impl Show for Bool where
  show = fn b -> if b then "true" else "false"
end

let () = println (__forge_dict_Show_int.show 42)
let () = println (__forge_dict_Show_bool.show true)
