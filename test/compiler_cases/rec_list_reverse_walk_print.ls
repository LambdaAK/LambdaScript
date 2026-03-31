Expected:
5
4
3
2
1

Source:
let rec print_list lst =
  case lst do
    | [] -> ()
    | h :: t ->
      let () = println (int_to_str h) in
        print_list t

let rec double_list lst =
  case lst do
    | [] -> []
    | h :: t -> h :: h :: double_list t

let rec append lst1 lst2 =
  case lst1 do
    | [] -> lst2
    | h :: t -> h :: append t lst2

let rec reverse lst =
  case lst do
    | [] -> []
    | h :: t -> append (reverse t) ([h])

let list = [1, 2, 3, 4, 5]

let reversed_list = reverse list

let () = print_list reversed_list
