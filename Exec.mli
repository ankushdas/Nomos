module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module M = C.Map

type sem =
    Proc of A.chan * int * (int * int) * A.expression
  | Msg of A.chan * int * (int * int) * A.msg

type map_string_sem = sem C.String.Map.t;;
type map_string_string = string C.String.Map.t;;
type configuration = map_string_sem * map_string_string * map_string_string;;

val pp_sem : sem -> string

val get_pot : A.decl_ext list -> A.expname -> A.potential

exception RuntimeError

exception ProgressError

val pp_maps : (string * string) list -> string

val exec :
  A.decl_ext list ->
  A.expname ->
  configuration

