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

type output_state = BSuccess of E.blockchain_state | BFailure of string;;
  
let account_list state =
  let (_tx, _ch, gas_accs, _types, _config) = state in
  let f (acc,balance) =
    `Assoc [ ("account", `String acc)
           ; ("balance", `Int balance)]
  in
  `List (List.map f (C.Map.to_alist gas_accs))

let create_account initial_state account_name balance =
  let state = TL.create_account account_name initial_state in
  let state = TL.deposit_gas account_name balance state in
  let str_state = blockchain_state_to_string state in
  let body =
  `Assoc [("state",`String str_state)
         ;("accList", account_list state)]
  in
  `Assoc [("response",`String "create")
         ;("body", body)]


let contract_list state =
  let (_tx, _ch, _gas_accs, _types, config) = state in
  let chantps = config.E.types in
  chantps;;

type elaboratedTxn = TSuccess of TL.environment | TFailure of string;;

let type_check txn =
  try
    match TL.read_txn txn with
        TL.RawTransaction env -> TSuccess(env)
  with
    EM.LexError m | EM.ParseError m | EM.TypeError m | EM.PragmaError m | EM.RuntimeError m ->
      TFailure(m);;



let submit state txn account_name =
  try
    let () = F.verbosity := -1 in
    let () = TL.set_sender account_name in
    let raw_env = TL.read_txn txn in
    let env = TL.infer raw_env in
    BSuccess(TL.run env state)
  with
      EM.LexError m | EM.ParseError m
    | EM.TypeError m | EM.PragmaError m | EM.RuntimeError m
    | EM.GasAcctError m | EM.FileError m ->
                       BFailure(m);;

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
       raise (EM.LexError "")
    | "submit" ->
       raise (EM.LexError "")
    | _ ->
       raise (Json_error ("Unknown request: " ^ request))
  in
  J.to_channel ?std:(Some true) stdout json_response
  

  (* need 

  - main function

    * parse json string input to json
    * either error or one of the three valid cases
    * parse state from sexp string
    * call create_account, submit, or type_check
    * create response json
    * convert response json to string

  - create_account (state:(ocaml type), account_name, balance) -> new_sate:(ocaml type)

  - type_check (transaction:string) -> (elaborated_transaction:string)

  - submit (state, elaborated_transaction, account) -> new_state

  - account_list (state) -> json_acouunt_list

  - contract_list (state) -> json_contract_list

 *)
