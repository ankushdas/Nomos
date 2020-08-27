module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags
module C = Core         

val create_account :
  TL.E.blockchain_state -> string -> int -> TL.E.blockchain_state

val account_list : 'a * 'b * 'c * 'd * 'e -> 'c

val contract_list : 'a * 'b * 'c * 'd * E.configuration -> E.map_chan_tp

type elaboratedTxn = TSuccess of TL.environment | TFailure of string

val type_check : string -> elaboratedTxn

type output_state = BSuccess of E.blockchain_state | BFailure of string

val submit : TL.E.blockchain_state -> string -> string -> output_state

val json_command : C.Command.t  
