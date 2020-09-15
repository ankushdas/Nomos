module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags
module PP = Pprint
module EL = Elab
module A = Ast
module R = Arith

module C = Core
module J = Yojson.Basic

(******************)
(* JSON Functions *)
(******************)

let blockchain_state_of_string str =
  E.blockchain_state_of_sexp (C.Sexp.of_string str)

let blockchain_state_to_string conf =
  C.Sexp.to_string (E.sexp_of_blockchain_state conf)

let transaction_of_string str =
  TL.transaction_of_sexp (C.Sexp.of_string str)

let transaction_to_string txn =
  C.Sexp.to_string (TL.sexp_of_transaction txn)

let account_list state =
  let (_tx, _ch, gas_accs, _types, _config) = state in
  let f (acc,balance) =
    `Assoc [ ("account", `String acc)
           ; ("balance", `Int balance)]
  in
  `List (List.map f (C.Map.to_alist gas_accs))


(* fixme: we need to get the code and the gas of a contract*)
let contract_list state =
  let (_tx, _ch, _gas_accs, _types, config) = state in
  let chantps = config.E.types in
  let f (channel,typ) =
    `Assoc [ ("channel", `String (PP.pp_chan channel))
           ; ("type", `String (PP.pp_tp_simple typ))
           ; ("code", `String "")
           ; ("gas", `String "") ]
  in
  `List (List.map f (C.Map.to_alist chantps))

  
let report_error response msg details =
  let error =
    `Assoc [ ("msg",`String msg)
           ; ("details", `String details)]
  in
  `Assoc [("response",`String response)
         ;("status", `String "error")
         ;("error", error)]
    

  
let create_account initial_state account_name balance =
  try
    let () = F.verbosity := -1 in
    let state = TL.create_account account_name initial_state in
    let state = TL.deposit_gas account_name balance state in
    let str_state = blockchain_state_to_string state in
    let body =
    `Assoc [("state",`String str_state)
          ;("acclist", account_list state)]
    in
    `Assoc [("response",`String "create")
          ;("status", `String "success")
          ;("body", body)]
  with
    | EM.LexError m
    | EM.ParseError m
    | EM.TypeError m
    | EM.PragmaError m
    | EM.RuntimeError m
    | EM.GasAcctError m ->
       report_error "account creation" m "";;

let eval pot = match pot with
    A.Star -> raise (EM.TypeError "impossible: star potential found after inference!")
  | A.Arith p -> R.evaluate p;;

let type_check _state txn =
  try
    let () = F.verbosity := -1 in
    let raw_env = TL.read_txn txn in
    let inferred_txn = TL.infer raw_env in
    let TL.Transaction env = inferred_txn in
    let f = EL.get_one_exec env 0 "" in
    let pot = A.get_pot env f in
    let gas = eval pot in
    let body =
      `Assoc [("transaction", `String (transaction_to_string inferred_txn))
             ;("transcode",`String (PP.pp_prog env))
             ;("gasbound", `Int gas)]
    in
    `Assoc [("response",`String "typecheck")
           ;("status", `String "success")
           ;("body", body)]
  with
    | EM.LexError m
    | EM.ParseError m
    | EM.TypeError m
    | EM.PragmaError m
    | EM.RuntimeError m
    | EM.GasAcctError m ->
       report_error "typecheck" m ""

let submit state txn account_name =
  try
    let () = F.verbosity := -1 in
    let () = TL.set_sender account_name in
    let initial_gas =
      let TL.Transaction env = txn in
      let f = EL.get_one_exec env 0 "" in
      let pot = A.get_pot env f in
      eval pot
    in
    let (state, leftover_gas) = TL.run txn state in
    let str_state = blockchain_state_to_string state in
    let body =
      `Assoc [("state",`String str_state)
             ;("contlist", contract_list state)
             ;("acclist", account_list state)
             ;("gascost", `Int (initial_gas - leftover_gas))]
    in
    `Assoc [("response",`String "submit")
           ;("status", `String "success")
           ;("body", body)]
  with
      EM.LexError m | EM.ParseError m
    | EM.TypeError m | EM.PragmaError m | EM.RuntimeError m
    | EM.GasAcctError m | EM.FileError m ->
                       report_error "submit" m ""

exception Json_error of string
                      
let main =
  let stdin = C.In_channel.stdin in
  let stdout = C.Out_channel.stdout in  
  let json_input = J.from_channel ?fname:(Some "json-query") stdin in
  let request = json_input |> J.Util.member "request" |> J.Util.to_string in
  let json_body = J.Util.member "body" json_input in
  let initial_state =
    let str_state = json_body |> J.Util.member "state" |> J.Util.to_string in
    blockchain_state_of_string str_state
  in
  let json_response = 
    match request with
    | "create" ->
       let account = json_body |> J.Util.member "account" |> J.Util.to_string in
       let balance = json_body |> J.Util.member "balance" |> J.Util.to_int in
       create_account initial_state account balance
    | "typecheck" ->
       let txn = json_body |> J.Util.member "transaction" |> J.Util.to_string in
       type_check initial_state txn
    | "submit" ->
       let txn_string = json_body |> J.Util.member "transaction" |> J.Util.to_string in
       let txn = transaction_of_string txn_string in
       let account = json_body |> J.Util.member "account" |> J.Util.to_string in
       submit initial_state txn account
    | _ ->
       raise (Json_error ("Unknown request: " ^ request))
  in
  J.to_channel ?std:(Some true) stdout json_response
  
