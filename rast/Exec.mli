module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module M = C.Map

type sem =
    Proc of A.chan * int * (int * int) * A.expression
  | Msg of A.chan * int * (int * int) * A.msg

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

type map_chan_sem = sem Chan.Map.t;;
type map_chan_chan = A.chan Chan.Map.t;;
type configuration = map_chan_sem * map_chan_chan * map_chan_chan;;

val pp_sem : sem -> string

val get_pot : A.decl_ext list -> A.expname -> A.potential

exception RuntimeError

exception ProgressError

val pp_maps : (string * string) list -> string

val exec :
  A.decl_ext list ->
  A.expname ->
  configuration

