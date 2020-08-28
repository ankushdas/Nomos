module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags

val create_account :
  TL.E.blockchain_state -> string -> int -> TL.E.blockchain_state

val account_list : 'a * 'b * 'c * 'd * 'e -> 'c

val contract_list : 'a * 'b * 'c * 'd * E.configuration -> E.map_chan_tp

type elaboratedTxn = TSuccess of TL.environment | TFailure of string

val type_check :  Stdlib.in_channel -> elaboratedTxn

type output_state = BSuccess of E.blockchain_state | BFailure of string

val submit : TL.E.blockchain_state -> Stdlib.in_channel -> string -> output_state

val main : unit
