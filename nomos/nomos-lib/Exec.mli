module A = Ast
module C = Core
module M = C.Map
module G = GasAcct

type sem =
    (* Proc(chan, in_use, time, (work, pot), P) *)
    Proc of string * A.chan * A.chan list * int * (int * int) * A.ext A.st_expr
    (* Msg(chan, time, (work, pot), M) *)
  | Msg of A.chan * int * (int * int) * A.ext A.msg

module Chan :
  sig
    module T :
      sig
        type t = A.chan
        val compare : t -> t -> int
        val sexp_of_t : t -> Base.Sexp.t
        val t_of_sexp : Base.Sexp.t -> t
      end
    module Map :
      sig
        module Key :
          sig
            type t = A.chan
            type comparator_witness = C.Comparable.Make(T).comparator_witness
          end
        type 'a t = (Key.t, 'a, Key.comparator_witness) M.t
      end
  end

type map_chan_sem = sem Chan.Map.t
type map_chan_chan = A.chan Chan.Map.t
type map_chan_tp = A.stype Chan.Map.t
type configuration =
  { conf   : map_chan_sem;
    conts  : map_chan_chan;
    shared : map_chan_chan;
    types  : map_chan_tp;
  }

type blockchain_state = int * int * G.gas_accounts * A.decl list * configuration [@@deriving sexp]
val empty_blockchain_state : blockchain_state

val pp_sem : sem -> string

val exec :
     (A.decl * 'a) list
  -> blockchain_state
  -> (string * (A.ext A.arg list))
  -> blockchain_state

val txnSender : string ref

val leftover_gas : unit -> int