module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags

module C = Core
module J = Yojson.Basic

(******************)
(* JSON Functions *)
(******************)

let blockchain_state_of_string str =
  E.blockchain_state_of_sexp (C.Sexp.of_string str)

let blockchain_state_to_string conf =
  C.Sexp.to_string (E.sexp_of_blockchain_state conf)


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
    `Assoc [ ("channel", `String (Pprint.pp_chan channel))
           ; ("type", `String (Pprint.pp_tp_simple typ))
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

(*Jan: I need to print a transaction at XX below but don't know how.*)

let type_check _state txn =
  try
    let () = F.verbosity := -1 in
    let TL.RawTransaction _txn' = TL.read_txn txn in
    let body =
      `Assoc [("transaction",`String "XX: I need to pring txn' here")
             ;("gasbound", `Int (-99))]
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
    let raw_env = TL.read_txn txn in
    let env = TL.infer raw_env in
    let state = TL.run env state in
    let str_state = blockchain_state_to_string state in
    let body =
      `Assoc [("state",`String str_state)
             ;("contlist", contract_list state)
             ;("acclist", account_list state)]
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
       let txn = json_body |> J.Util.member "transaction" |> J.Util.to_string in
       let account = json_body |> J.Util.member "account" |> J.Util.to_string in
       submit initial_state txn account
    | _ ->
       raise (Json_error ("Unknown request: " ^ request))
  in
  J.to_channel ?std:(Some true) stdout json_response
  
