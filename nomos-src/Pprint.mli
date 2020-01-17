module R = Arith
module A = Ast
val parens : 'a -> 'a -> string -> string
val pp_arith_prec : int -> R.arith -> string
val pp_arith : R.arith -> string
val pp_pot : A.potential -> string
val pp_potpos : A.potential -> string
val pp_arith_opr : A.arith_operator -> string
val pp_comp_opr : A.comp_operator -> string
val pp_rel_opr : A.rel_operator -> string
val spaces : int -> string
val len : string -> int
val pp_mode : A.mode -> string
val pp_ftp_simple : A.func_tp -> string
val pp_tp_simple : A.stype -> A.tpname
val pp_choice_simple : A.choices -> string
exception ImpossMode
val pp_outer_mode : A.mode -> string
val pp_structure : A.str -> string
val pp_chan : A.str * string * A.mode -> string
val pp_chan_tp : (A.str * string * A.mode) * A.stype -> string
val pp_channames : (A.str * string * A.mode) list -> string
val pp_tp_after : int -> string -> A.stype -> string
val pp_choice : int -> A.choices -> string
val pp_choice_indent : int -> A.choices -> string
val pp_tp : 'a -> A.stype -> A.tpname
val pp_tp_compact : 'a -> A.stype -> A.tpname
val pp_args : A.arglist -> string
val pp_lsctx : 'a -> ((A.str * string * A.mode) * A.stype) list -> string
val pp_arg : 'a -> A.argument -> string
val pp_arglist : 'a -> A.argument list -> string
val pp_ctx : 'a -> A.context -> string
val pp_tpj_compact :
  'a ->
  A.context -> A.potential -> (A.str * string * A.mode) * A.stype -> string
val pp_exp_indent : 'a -> int -> 'b A.st_aug_expr -> string
val pp_exp_after : 'a -> int -> string -> 'b A.st_expr -> string
val pp_then : int -> string
val pp_else : int -> string
val pp_branches : 'a -> int -> 'b A.branches -> string
val pp_branches_indent : 'a -> int -> 'b A.branches -> string
val pp_fexp_list : 'a -> int -> 'b A.func_aug_expr list -> string
val pp_fexp : 'a -> int -> 'b A.func_expr -> string
val pp_fexp_indent : 'a -> int -> 'b A.func_expr -> string
val pp_argname : 'a -> 'b A.arg -> string
val pp_argnames : 'a -> 'b A.arg list -> string
val pp_exp_prefix : 'a A.st_expr -> string
val pp_val_list : Ast.value list -> string
val pp_val : Ast.value -> string
val pp_msg : A.msg -> string
exception Unsupported
val pp_decl : 'a -> A.decl -> string
val pp_exp : 'a -> 'b A.st_expr -> string
val pp_program : 'a -> A.decl list -> string
