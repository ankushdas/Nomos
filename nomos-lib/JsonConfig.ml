module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags

module C = Core
module J = Yojson.Basic

(******************)
(* JSON Functions *)
(******************)

let create_account initial_state account_name balance =
  let state = initial_state in
  let state = TL.create_account account_name state in
  let state = TL.deposit_gas account_name balance state in
  state;;

let account_list state =
  let (_tx, _ch, gas_accs, _types, _config) = state in
  gas_accs;;

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

type output_state = BSuccess of E.blockchain_state | BFailure of string;;

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


let main =
  let stdin = C.In_channel.stdin in
  let stdout = C.Out_channel.stdout in  
  let json_input = J.from_channel ?fname:(Some "json-query") stdin in
  (* let request = json_input |> J.Util.member "request" |> J.Util.to_string in *)

  let json_body = J.Util.member "body" json_input in
  J.to_channel ?std:(Some true) stdout json_body

  

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
