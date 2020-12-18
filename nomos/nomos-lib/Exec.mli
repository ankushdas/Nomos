module A = Ast
module C = Core
module M = C.Map
module G = GasAcct

type key =
    IntK of int
  | BoolK of bool
  | AddrK of string
  | StringK of string
  [@@deriving sexp];;

module Key :
  sig
    module T :
      sig
        type t = key
        val compare : t -> t -> int
        val sexp_of_t : t -> Base.Sexp.t
        val t_of_sexp : Base.Sexp.t -> t
      end
      module Map :
      sig
        module Key :
          sig
            type t = key
            type comparator_witness = C.Comparable.Make(T).comparator_witness
          end
        type 'a t = (Key.t, 'a, Key.comparator_witness) M.t
      end
  end

(* map from key to session-typed channel *)
type map_key_chan = A.chan Key.Map.t [@@deriving sexp]

(* map from  *)
type map_key_value = (A.ext A.value) Key.Map.t [@@deriving sexp]

type sem =
  (* Proc(procname, chan, in_use, time, (work, pot), P) *)
  Proc of string * A.chan * A.chan list * int * (int * int) * A.ext A.st_expr
  (* MapFProc(chan, in_use, time, (work, pot), map) *)
| MapFProc of A.chan * A.chan list * int * (int * int) * map_key_value
  (* MapSTProc(chan, in_use, time, (work, pot), map) *)
| MapSTProc of A.chan * A.chan list * int * (int * int) * map_key_chan
  (* Msg(chan, time, (work, pot), M) *)
| Msg of A.chan * int * (int * int) * A.ext A.msg
[@@deriving sexp];;

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
    types  : map_chan_tp
  }

val get_exec_msgs : unit -> string

val subconfig : A.chan -> configuration -> string * string

type blockchain_state = int * int * G.gas_accounts * A.decl list * configuration [@@deriving sexp]
val empty_blockchain_state : blockchain_state

val pp_config : configuration -> string

val exec :
     (A.decl * 'a) list
  -> blockchain_state
  -> (string * (A.ext A.arg list))
  -> blockchain_state

val txnSender : string ref

val leftover_gas : unit -> int