module R = Arith
module A = Ast
val pp_arith : R.arith -> string
val pp_pot : A.potential -> string
val pp_mode : A.mode -> string
val pp_ftp_simple : A.func_tp -> string
val pp_tp_simple : A.stype -> A.tpname
val pp_structure : A.str -> string
val pp_chan : A.str * string * A.mode -> string
val pp_channames : (A.str * string * A.mode) list -> string
val pp_tp : 'a -> A.stype -> A.tpname
val pp_tp_compact : 'a -> A.stype -> A.tpname
val pp_lsctx : 'a -> ((A.str * string * A.mode) * A.stype) list -> string
val pp_tpj_compact :
  'a ->
  A.context -> A.potential -> (A.str * string * A.mode) * A.stype -> string
val pp_fexp : 'a -> int -> 'b A.func_expr -> string
val pp_exp_prefix : 'a A.st_expr -> string
val pp_msg : 'a A.msg -> string
val pp_decl : 'a -> A.decl -> string
val pp_prog : (A.decl * 'a) list -> string
val pp_val : 'a A.value -> string