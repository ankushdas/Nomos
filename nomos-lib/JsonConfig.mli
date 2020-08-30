module TL = TopLevel
module E = Exec
module EM = ErrorMsg
module F = NomosFlags

type output_state = BSuccess of E.blockchain_state | BFailure of string

val contract_list : 'a * 'b * 'c * 'd * E.configuration -> E.map_chan_tp

type elaboratedTxn = TSuccess of TL.environment | TFailure of string

val type_check : string -> elaboratedTxn

val submit : TL.E.blockchain_state -> string -> string -> output_state

val main : unit
