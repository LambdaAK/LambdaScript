Expected:
82
0
91

Source:
type Txn =
  | Deposit of Int
  | Withdrawal of Int
  | Fee of Int

let apply_txn balance tx =
  case tx do
  | Deposit n -> balance + n
  | Withdrawal n -> balance - n
  | Fee n -> balance - n

let rec apply_all balance txs =
  case txs do
  | [] -> balance
  | h :: t -> apply_all (apply_txn balance h) t

let clamp_nonnegative n = if n < 0 then 0 else n

let account_a = [Deposit 25, Fee 3, Withdrawal 40]
let account_b = [Withdrawal 10, Fee 2]
let account_c = [Deposit 100, Withdrawal 5, Fee 4]

let () = print_string (int_to_str (apply_all 100 account_a))
let () = print_string (int_to_str (clamp_nonnegative (apply_all 5 account_b)))
let () = print_string (int_to_str (apply_all 0 account_c))
