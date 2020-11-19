module C = Core
module M = C.Map
module F = NomosFlags
module EM = ErrorMsg

open Sexplib.Std

type gas_accounts = int M.M(C.String).t [@@deriving sexp]

let error m = EM.error EM.GasAcct None m;;

let create_account gas_accs sender =
  match M.find gas_accs sender with
      None ->
        let () = if !F.verbosity >= 0 then print_string ("% account creation of " ^ sender ^ " successful!\n") in
        M.add_exn gas_accs ~key:sender ~data:0
    | Some _ -> error ("% account of " ^ sender ^ " already exists!");;

let deposit gas_accs sender d =
  match M.find gas_accs sender with
      None -> error ("% account for txn sender " ^ sender ^ " does not exist!")
    | Some bal ->
        let () = if !F.verbosity >= 0 then print_string ("% deposit of " ^ string_of_int d ^ " gas units successful!\n") in
        let () = if !F.verbosity >= 0 then print_string ("% account balance of " ^ sender ^ ": " ^ string_of_int (bal+d) ^"\n") in
        M.set gas_accs ~key:sender ~data:(bal+d);;

type res = Insufficient of int | Balance of int | NonBC;;

let deduct gas_accs sender gas =
  if !F.bc_mode
  then
    begin
    match M.find gas_accs sender with
        None -> error ("% account for txn sender " ^ sender ^ " does not exist!\n")
      | Some bal ->
          if gas > bal
          then (Insufficient bal, M.set gas_accs ~key:sender ~data:0)
          else (Balance (bal-gas), M.set gas_accs ~key:sender ~data:(bal-gas))
    end
  else
    (NonBC, gas_accs)