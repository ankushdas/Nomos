module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module M = C.Map

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
type configuration = map_chan_sem * map_chan_chan * map_chan_chan

type full_configuration = int * int * configuration [@@deriving sexp]
val empty_full_configuration : full_configuration


val pp_sem : sem -> string

val get_pot : (A.decl * 'a) list -> string -> A.potential

exception RuntimeError

exception ProgressError

val pp_maps : (A.chan * A.chan) list -> string

val exec :
     (A.decl * 'a) list
  -> full_configuration
  -> (string * (A.ext A.arg list))
  -> full_configuration
