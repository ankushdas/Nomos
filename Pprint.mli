module R = Arith
module A = Ast
val parens : 'a -> 'a -> string -> string
val pp_arith_prec : int -> R.arith -> string
val pp_arith : R.arith -> string
type opr = Or | And | Implies | Not | None
val parens_opr : opr -> opr -> string -> string
val pp_pot : A.R.arith -> string
val pp_potpos : A.R.arith -> string
val spaces : int -> string
val len : string -> int
val pp_tp_indent : int -> A.stype -> string
val pp_tp_after : int -> string -> A.stype -> string
val pp_choice : int -> A.choices -> string
val pp_choice_indent : int -> A.choices -> string
val pp_tp : 'a -> A.stype -> A.tpname
val pp_tp_compact : 'a -> A.stype -> A.tpname
val pp_lsctx : 'a -> (string * A.stype) list -> string
val pp_ctx : 'a -> A.context -> string
val pp_tpj : 'a -> A.context -> A.R.arith -> string * A.stype -> string
val pp_tpj_compact :
  'a -> A.context -> A.R.arith -> string * A.stype -> string
val atomic : A.expression -> bool
val long : A.expression -> bool
val pp_cut : 'a -> R.arith -> A.stype -> string
val pp_exp_indent : 'a -> int -> A.expression -> string
val pp_exp_after : 'a -> int -> string -> A.expression -> string
val pp_branches : 'a -> int -> A.branches -> string
val pp_branches_indent : 'a -> int -> A.branches -> string
val pp_exp_prefix : A.expression -> string
exception Unsupported
val pp_decl : 'a -> A.decl -> string
val pp_config : 'a -> 'b -> A.config -> string
val pp_exp : 'a -> A.expression -> string
