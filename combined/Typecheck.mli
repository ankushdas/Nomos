module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
module I = Infer
val error : string -> 'a
val esync :
  A.decl list ->
  A.tpname list -> PP.A.stype -> PP.A.stype -> 'a -> bool -> unit
val esync_choices :
  A.decl list ->
  A.tpname list -> A.choices -> PP.A.stype -> 'a -> bool -> unit
val esync_tp : A.decl list -> PP.A.stype -> 'a -> unit
val contractive : A.stype -> bool
val zero : A.potential
val eq : A.potential -> A.potential -> bool
val ge : A.potential -> A.potential -> bool
val minus : A.potential -> A.potential -> A.potential
val plus : A.potential -> A.potential -> A.potential
val pp_uneq : A.potential -> A.potential -> string
val pp_lt : A.potential -> A.potential -> string
val mode_L : 'a * 'b * A.mode -> bool
val mode_S : 'a * 'b * A.mode -> bool
val mode_P : 'a * 'b * A.mode -> bool
val mode_T : 'a * 'b * A.mode -> bool
val mode_lin : 'a * 'b * A.mode -> bool
val eqmode : A.mode -> A.mode -> bool
val mode_spawn : A.mode -> A.mode -> bool
val mode_recv : A.mode -> A.mode -> bool
val mem_seen : 'a -> ('b * 'b) list -> 'b -> 'b -> bool
val eq_tp' :
  A.decl list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> bool
val eq_tp :
  A.decl list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> bool
val eq_choice :
  A.decl list -> (A.tpname * A.tpname) list -> A.choices -> A.choices -> bool
val eq_name_name :
  A.decl list -> (A.tpname * A.tpname) list -> A.tpname -> A.tpname -> bool
val eqtp : A.decl list -> PP.A.stype -> PP.A.stype -> bool
val eq_ftp : A.func_tp -> A.func_tp -> bool
exception UnknownTypeError
val chan_of : 'a * 'b -> 'a
val tp_of : 'a * 'b -> 'b
val name_of : 'a * 'b * 'c -> 'b
val eq_str : 'a * 'b * 'c -> 'a * 'd * 'e -> bool
val eq_name : 'a * 'b * 'c -> 'd * 'b * 'e -> bool
val eq_mode : 'a * 'b * A.mode -> 'c * 'd * A.mode -> bool
val eq_chan : 'a * 'b * A.mode -> 'a * 'b * A.mode -> bool
val checktp : 'a * 'b * 'c -> (('d * 'b * 'e) * 'f) list -> bool
val checkftp : string -> A.argument list -> bool
val check_ftp : string -> A.context -> bool
val check_tp : 'a * string * 'b -> A.context -> bool
val check_stp : 'a * string * 'b -> A.context -> bool
val check_ltp : 'a * string * 'b -> A.context -> bool
val purelin : A.context -> bool
val findtp : 'a * 'b * A.mode -> (('a * 'b * A.mode) * 'c) list -> 'd -> 'c
val findftp : string -> A.argument list -> 'a -> A.func_tp
val find_ftp : string -> A.context -> 'a -> A.func_tp
val find_stp : PP.A.str * string * A.mode -> A.context -> 'a -> A.stype
val find_ltp : E.PP.A.str * string * A.mode -> A.context -> 'a -> A.stype
val removetp :
  'a * 'b * 'c -> (('d * 'b * 'e) * 'f) list -> (('d * 'b * 'e) * 'f) list
val removeotp : 'a * string * 'b -> A.argument list -> A.argument list
val remove_tp : 'a * string * 'b -> A.context -> A.context
val add_chan : A.decl list -> A.chan * A.stype -> A.context -> A.context
val add_var : string * A.func_tp -> A.context -> A.context
val update_tp : A.decl list -> A.chan -> A.stype -> A.context -> A.context
val match_ctx :
  A.decl list ->
  A.argument list -> A.arg list -> A.context -> int -> int -> 'a -> A.context
val join : A.context -> A.argument list
val lookup_var : string -> A.argument list -> A.func_tp option
val lookup_ftp : string -> A.context -> A.func_tp
val min_potential : A.potential -> A.potential -> A.potential
val min_tp : A.func_tp -> A.func_tp -> A.func_tp
val min_delta : A.argument list -> A.argument list -> A.argument list
val min_pot :
  A.context * A.potential ->
  A.context * A.potential -> A.context * A.potential
val removevar : string -> A.argument list -> A.argument list
val remove_var : string -> A.context -> A.context
val consume_pot : A.func_tp -> A.func_tp
val consumevar : string -> A.argument list -> A.argument list
val consume : string -> A.context -> A.context
val consify : 'a A.func_aug_expr list -> 'a A.func_expr
exception SplitError
val split_last : 'a list -> 'a list * 'a
val check_fexp_simple' :
  bool ->
  A.decl list ->
  A.context ->
  A.potential ->
  'a PP.A.func_expr ->
  PP.A.func_tp -> 'b -> PP.A.mode -> bool -> A.context * A.potential
val synth_fexp_simple' :
  bool ->
  A.decl list ->
  A.context ->
  A.potential ->
  'a PP.A.func_expr ->
  'b -> PP.A.mode -> bool -> A.context * A.potential * A.func_tp
val check_fexp_simple :
  bool ->
  A.decl list ->
  A.context ->
  A.potential ->
  'a PP.A.func_expr ->
  PP.A.func_tp -> 'b -> PP.A.mode -> bool -> A.context * A.potential
val synth_fexp_simple :
  bool ->
  A.decl list ->
  A.context ->
  A.potential ->
  'a PP.A.func_expr ->
  'b -> PP.A.mode -> bool -> A.context * A.potential * A.func_tp
val checkfexp :
  bool ->
  A.decl list ->
  PP.A.context ->
  PP.A.potential ->
  'a A.func_aug_expr ->
  (PP.A.str * string * PP.A.mode) * PP.A.stype -> 'b -> PP.A.mode -> unit
val check_exp' :
  bool ->
  A.decl list ->
  PP.A.context ->
  PP.A.potential ->
  'a A.st_expr ->
  (PP.A.str * string * PP.A.mode) * PP.A.stype -> 'b -> PP.A.mode -> unit
val check_exp :
  bool ->
  A.decl list ->
  PP.A.context ->
  PP.A.potential ->
  'a A.st_expr ->
  (PP.A.str * string * PP.A.mode) * PP.A.stype -> 'b -> PP.A.mode -> unit
val check_branchesR :
  bool ->
  A.decl list ->
  PP.A.context ->
  PP.A.potential ->
  'a A.branches ->
  PP.A.str * string * PP.A.mode -> A.choices -> 'b -> PP.A.mode -> unit
val check_branchesL :
  bool ->
  A.decl list ->
  PP.A.context ->
  A.chan ->
  A.choices ->
  PP.A.potential ->
  'a A.branches ->
  (PP.A.str * string * PP.A.mode) * PP.A.stype -> 'b -> PP.A.mode -> unit
val checkexp :
  bool ->
  A.decl list ->
  PP.A.context ->
  PP.A.potential ->
  'a A.st_expr ->
  (PP.A.str * string * PP.A.mode) * PP.A.stype -> 'b -> PP.A.mode -> unit
val find_tp :
  'a * 'b * 'c ->
  (('d * 'b * 'e) * 'f) list -> (('d * 'b * 'e) * 'g) list -> 'd * 'b * 'e
val consistent_mode :
  'a ->
  ((E.PP.A.str * string * E.PP.A.mode) * 'b) list ->
  ((E.PP.A.str * string * E.PP.A.mode) * 'c) list ->
  A.argument list -> 'd -> unit
val mode_P_list : (('a * 'b * A.mode) * 'c) list -> bool
val pure :
  'a -> string -> A.context -> PP.A.str * string * PP.A.mode -> 'b -> unit
val mode_S_list : (('a * 'b * A.mode) * 'c) list -> bool
val shared :
  'a -> string -> A.context -> PP.A.str * string * PP.A.mode -> 'b -> unit
val mode_lin_list : (('a * 'b * A.mode) * 'c) list -> bool
val transaction :
  'a -> string -> A.context -> PP.A.str * string * PP.A.mode -> 'b -> unit
