(* Interface for pretty printer *)

module R = Arith
module A = Ast

val pp_arith : R.arith -> string
type opr = Or | And | Implies | Not | None
val pp_tp : 'a -> A.stype -> A.tpname
val pp_tp_compact : 'a -> A.stype -> A.tpname
val pp_lsctx : 'a -> (string * A.stype) list -> string
val pp_tpj_compact :
  'a -> A.context -> A.R.arith -> string * A.stype -> string
val pp_exp_prefix : A.expression -> string
val pp_decl : 'a -> A.decl -> string
val pp_config : 'a -> 'b -> A.config -> string
val pp_exp : 'a -> A.expression -> string
