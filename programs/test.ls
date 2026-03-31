let rec print_list lst =
  case lst do
    | [] -> ()
    | h :: t ->
      let () = println (int_to_str h) in
        print_list t


