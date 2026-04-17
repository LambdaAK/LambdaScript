Expected:
20
210
110
55
13
89
8
189
5050
3628800
1
1
0
1
0
110
144
6765
45

Source:
let rec len lst =
  case lst do
    | [] -> 0
    | _ :: t -> 1 + len t

let rec sum lst =
  case lst do
    | [] -> 0
    | h :: t -> h + sum t

let rec append a b =
  case a do
    | [] -> b
    | h :: t -> h :: append t b

let rec rev lst =
  case lst do
    | [] -> []
    | h :: t -> append (rev t) ([h])

let rec take n lst =
  if n == 0 then []
  else
    case lst do
      | [] -> []
      | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n == 0 then lst
  else
    case lst do
      | [] -> []
      | _ :: t -> drop (n - 1) t

let rec inc_map lst =
  case lst do
    | [] -> []
    | h :: t -> (h + 1) :: inc_map t

let rec evens lst =
  case lst do
    | [] -> []
    | h :: t ->
      if h % 2 == 0 then h :: evens t else evens t

let rec fact n = if n <= 1 then 1 else n * fact (n - 1)

let rec fib n =
  if n == 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2)

let rec gcd a b =
  if b == 0 then a else gcd b (a % b)

let rec sumrange lo hi acc =
  if lo > hi then acc else sumrange (lo + 1) hi (acc + lo)

let rec zeros n = if n == 0 then [] else 0 :: zeros (n - 1)

let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

let () = print_string (int_to_str (len numbers))
let () = print_string (int_to_str (sum (take 20 numbers)))
let () = print_string (int_to_str (sum (evens numbers)))
let () = print_string (int_to_str (fib 10))
let () = print_string (int_to_str (gcd 234 377))
let () = print_string (int_to_str (fib 11))
let () = print_string (int_to_str (len (inc_map (drop 12 numbers))))
let () = print_string (int_to_str (sum (take 14 (drop 6 numbers))))
let () = print_string (int_to_str (sumrange 1 100 0))
let () = print_string (int_to_str (fact 10))
let () = print_string (int_to_str (if len (zeros 0) == 0 then 1 else 0))
let () = print_string (int_to_str (if sum (zeros 0) == 0 then 1 else 0))
let () = print_string (int_to_str (if fib 0 == 0 then 0 else 1))
let () = print_string (int_to_str (if fib 1 == 1 then 1 else 0))
let () = print_string (int_to_str (if gcd 17 19 == 1 then 0 else 1))
let () = print_string (int_to_str (sum (evens (take 20 (rev numbers)))))
let () = print_string (int_to_str (fib 12))
let () = print_string (int_to_str (fib 20))
let () = print_string (int_to_str (sum (take 9 numbers)))
