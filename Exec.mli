module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module M = C.Map

type sem =
    Proc of A.chan * int * (int * int) * A.expression
  | Msg of A.chan * int * (int * int) * A.msg

val pp_sem : sem -> string

val get_pot : A.decl_ext list -> A.expname -> A.potential

exception RuntimeError

val exec :
  A.decl_ext list ->
  sem ->
  (A.chan, sem, C.String.comparator_witness) M.t * (A.chan, A.chan, C.String.comparator_witness) M.t
