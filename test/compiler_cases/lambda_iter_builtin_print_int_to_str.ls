Expected:
1
2
3

Source:
let rec iter f lst =
  case lst do
    | [] -> ()
    | h :: t ->
      let () = f h in
        iter f t

let my_list = [1, 2, 3]

let () = iter (fn x -> println (int_to_str x)) my_list
