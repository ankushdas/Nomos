(* Interface for pretty printer *)

module R = Arith
module A = Ast

val pp_arith : R.arith -> string
val pp_tp : 'a -> A.stype -> A.tpname
val pp_tp_compact : 'a -> A.stype -> A.tpname
val pp_lsctx : 'a -> (A.chan * A.stype) list -> string
val pp_tpj_compact :
  'a -> A.context -> A.potential -> A.chan * A.stype -> string
val pp_exp_prefix : A.expression -> string
val pp_decl : 'a -> A.decl -> string
val pp_exp : 'a -> A.expression -> string
val pp_msg : A.msg -> string
val pp_chan : A.chan -> string
val pp_mode : A.mode -> string