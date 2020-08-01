module C = Core
module M = C.Map

open Sexplib.Std

type gas_accounts = int M.M(C.String).t [@@deriving sexp]

let create_account sender initial_config =
  let (tx, ch, gas_accs, types, config) = initial_config in
  let new_gas_accs =
    match M.find gas_accs sender with
        None -> M.add_exn gas_accs ~key:sender ~data:0
      | Some _ -> gas_accs
  in
  (tx, ch, new_gas_accs, types, config);;

let deposit_gas sender d initial_config =
  let (tx, ch, gas_accs, types, config) = initial_config in
  let new_gas_accs =
    match M.find gas_accs sender with
        None -> C.eprintf "%% account for txn sender %s does not exist!\n" sender; exit 1
      | Some bal ->
          M.set gas_accs ~key:sender ~data:(bal+d)
  in
  (tx, ch, new_gas_accs, types, config);;

type res = Insufficient of int | Balance of int;;

let deduct gas_accs sender gas =
  match M.find gas_accs sender with
      None -> C.eprintf "%% account for txn sender %s does not exist!\n" sender; exit 1
    | Some bal ->
        if gas > bal
        then (Insufficient bal, M.set gas_accs ~key:sender ~data:0)
        else (Balance (bal-gas), M.set gas_accs ~key:sender ~data:(bal-gas));;