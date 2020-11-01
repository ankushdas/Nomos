module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
module I = Infer
module F = NomosFlags
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val esync :
  (A.decl * 'a) list ->
  A.tpname list ->
  PP.A.stype ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> bool -> unit
val esync_pchoices :
  (A.decl * 'a) list ->
  A.tpname list ->
  A.pchoices ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> bool -> unit
val esync_choices :
  (A.decl * 'a) list ->
  A.tpname list ->
  A.choices ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> bool -> unit
val esync_tp :
  (A.decl * 'a) list ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> unit
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
val eq_ftp : A.func_tp -> A.func_tp -> bool
type probmode = Prob | NonProb
val prob : probmode -> bool
val mem_seen : 'a -> ('b * 'b) list -> 'b -> 'b -> bool
val eq_tp' :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val eq_tp :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val eq_pchoice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.pchoices -> A.pchoices -> probmode -> bool
val eq_choice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.choices -> A.choices -> probmode -> bool
val eq_name_name :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.tpname -> A.tpname -> probmode -> bool
val eqtp : (A.decl * 'a) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val sub_tp' :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val sub_tp :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val sub_pichoice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.pchoices -> A.pchoices -> probmode -> bool
val sub_ichoice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.choices -> A.choices -> probmode -> bool
val sub_pechoice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.pchoices -> A.pchoices -> probmode -> bool
val sub_echoice :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.choices -> A.choices -> probmode -> bool
val sub_name_name :
  (A.decl * 'a) list ->
  (A.tpname * A.tpname) list -> A.tpname -> A.tpname -> probmode -> bool
val subtp :
  (A.decl * 'a) list -> PP.A.stype -> PP.A.stype -> probmode -> bool
val ssync :
  (A.decl * 'a) list ->
  A.tpname list ->
  PP.A.stype ->
  PP.A.stype option -> ((int * int) * (int * int) * string) option -> unit
val ssync_pchoices :
  (A.decl * 'a) list ->
  A.tpname list ->
  A.pchoices ->
  PP.A.stype option -> ((int * int) * (int * int) * string) option -> unit
val ssync_choices :
  (A.decl * 'a) list ->
  A.tpname list ->
  A.choices ->
  PP.A.stype option -> ((int * int) * (int * int) * string) option -> unit
val ssync_tp :
  (A.decl * 'a) list ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> unit
exception UnknownTypeError
val chan_of : 'a * 'b -> 'a
val tp_of : 'a * 'b -> 'b
val name_of : 'a * 'b * 'c -> 'b
val eq_str : 'a * 'b * 'c -> 'a * 'd * 'e -> bool
val eq_name : 'a * 'b * 'c -> 'd * 'b * 'e -> bool
val eq_mode : 'a * 'b * A.mode -> 'c * 'd * A.mode -> bool
val eq_chan : 'a * 'b * A.mode -> 'c * 'b * A.mode -> bool
val checktp : 'a * 'b * 'c -> (('d * 'b * 'e) * 'f) list -> bool
val checkftp : string -> A.argument list -> bool
val check_ftp : string -> A.context -> bool
val check_tp : 'a * string * 'b -> A.context -> bool
val check_stp : 'a * string * 'b -> A.context -> bool
val check_ltp : 'a * string * 'b -> A.context -> bool
val purelin : A.context -> bool
val findtp : 'a * 'b * A.mode -> (('c * 'b * A.mode) * 'd) list -> 'e -> 'd
val findftp : string -> A.argument list -> A.func_tp
val find_ftp : string -> A.context -> A.func_tp
val find_stp :
  PP.A.str * string * A.mode ->
  A.context -> ((int * int) * (int * int) * string) option -> A.stype
val find_ltp :
  E.PP.A.str * string * A.mode ->
  A.context -> ((int * int) * (int * int) * string) option -> A.stype
val removetp :
  'a * 'b * 'c -> (('d * 'b * 'e) * 'f) list -> (('d * 'b * 'e) * 'f) list
val removeotp : 'a * string * 'b -> A.argument list -> A.argument list
val remove_tp : 'a * string * 'b -> A.context -> A.context
val add_chan :
  (A.decl * 'a) list -> A.chan * A.stype -> A.context -> A.context
val add_var : string * A.func_tp -> A.context -> A.context
val update_tp :
  (A.decl * 'a) list -> A.chan -> A.stype -> A.context -> A.context
val match_ctx :
  (A.decl * 'a) list ->
  A.argument list ->
  'b A.arg list ->
  A.context ->
  int ->
  int -> ((int * int) * (int * int) * string) option -> probmode -> A.context
val join : A.context -> A.argument list
val lookup_var : string -> A.argument list -> A.func_tp option
val lookup_ftp :
  string ->
  A.context -> ((int * int) * (int * int) * string) option -> A.func_tp
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
val tc_printable_arg :
  'a A.arg ->
  A.func_tp option ->
  A.context -> ((int * int) * (int * int) * string) option -> unit
val check_printable_list :
  A.context ->
  ((int * int) * (int * int) * string) option ->
  A.printable list -> 'a A.arg list -> int -> int -> unit
val is_argtype : A.printable -> bool
val filter_args : A.printable list -> A.printable list
val pr_zero : A.potential
val pr_one : A.potential
val plabprob :
  string ->
  (string * A.potential * 'a) list ->
  ((int * int) * (int * int) * string) option -> unit
val in_ctx : 'a * string * 'b -> 'c A.arg list -> bool
val find_sigtp :
  A.argument list -> 'a A.arg list -> 'b * string * 'c -> A.stype
val get_typeL :
  (A.decl * 'a) list ->
  A.expname -> 'b A.arg list -> 'c * string * 'd -> A.stype
val get_typeR : (A.decl * 'a) list -> A.expname -> A.stype
val gen_pchoices : ('a * 'b * 'c) list -> 'a -> ('a * A.potential * 'c) list
val gen_tp : A.label -> A.stype -> A.stype
val add : A.potential -> A.potential -> A.potential
val mult : A.potential -> A.potential -> A.potential
val add_choices :
  (A.decl * 'a) list ->
  PP.A.str * string * PP.A.mode ->
  (string * A.potential * PP.A.stype) list ->
  (string * A.potential * PP.A.stype) list ->
  ((int * int) * (int * int) * string) option ->
  (string * A.potential * PP.A.stype) list
val add_ptypes :
  (A.decl * 'a) list ->
  PP.A.str * string * PP.A.mode ->
  A.stype ->
  A.stype -> ((int * int) * (int * int) * string) option -> A.stype
val mult_choices :
  'a ->
  A.potential -> ('b * A.potential * 'c) list -> ('b * A.potential * 'c) list
val mult_ptype : (A.decl * 'a) list -> A.potential -> A.stype -> A.stype
val weighted_psum :
  (A.decl * 'a) list ->
  PP.A.str * string * PP.A.mode ->
  ('b * A.potential * A.stype) list ->
  ((int * int) * (int * int) * string) option -> A.stype
val comp : R.arith -> R.arith
val comp_star : A.potential -> A.potential
val action :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.st_aug_expr ->
  PP.A.str * string * PP.A.mode -> A.stype -> A.stype
val action_pbranches :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.pbranches ->
  PP.A.str * string * PP.A.mode ->
  A.stype -> (A.label * A.potential * A.stype) list
val action_branches :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.branches ->
  PP.A.str * string * PP.A.mode -> A.stype -> A.stype
val gen_prob_tpL :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.st_aug_expr ->
  PP.A.str * string * PP.A.mode -> A.stype -> A.stype
val gen_prob_tpR :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.st_aug_expr ->
  PP.A.str * string * PP.A.mode -> A.stype -> A.stype
val eq_name_opt : ('a * 'b * 'c) option -> 'd * 'b * 'e -> bool
val gen_prob_ctx :
  (A.decl * 'a) list ->
  A.context ->
  ((int * int) * (int * int) * string) option A.st_aug_expr ->
  ('b * string * 'c) option -> A.context
val label_errormsg :
  'a -> (string * PP.A.probability * PP.A.stype) list -> string
val prob_error :
  'a ->
  PP.A.str * string * PP.A.mode ->
  PP.A.stype ->
  PP.A.stype ->
  (string * PP.A.probability * PP.A.stype) list ->
  ((int * int) * (int * int) * string) option -> 'b
val match_probs :
  (A.decl * 'a) list ->
  PP.A.str * string * PP.A.mode ->
  PP.A.stype ->
  (string * PP.A.probability * PP.A.stype) list ->
  ((int * int) * (int * int) * string) option -> unit
val extract : 'a * 'b * 'c -> (('d * 'b * 'e) * 'f) list -> 'f
val extract_ord : 'a * string * 'b -> A.argument list -> A.stype
val sextract : 'a * string * 'b -> A.context -> A.stype
val lextract : 'a * string * 'b -> A.context -> A.stype
val oextract : 'a * string * 'b -> A.context -> A.stype
val sextractl :
  'a * string * 'b -> ('c * 'd * A.context) list -> ('c * 'd * A.stype) list
val lextractl :
  'a * string * 'b -> ('c * 'd * A.context) list -> ('c * 'd * A.stype) list
val oextractl :
  'a * string * 'b -> ('c * 'd * A.context) list -> ('c * 'd * A.stype) list
val match_probs_ctx :
  (A.decl * 'a) list ->
  A.context ->
  (string * PP.A.probability * A.context) list ->
  ((int * int) * (int * int) * string) option -> unit
val faug : 'a A.func_expr -> 'a -> 'a A.func_aug_expr
val staug : 'a A.st_expr -> 'a -> 'a A.st_aug_expr
val create_app : 'a A.func_aug_expr -> 'a A.func_aug_expr -> 'a A.func_expr
val pot_branch : A.potential -> A.potential
val check_fexp_simple' :
  bool ->
  (A.decl * 'a) list ->
  A.context ->
  A.potential ->
  A.parsed_expr ->
  PP.A.func_tp ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> bool -> probmode -> A.context * A.potential * A.parsed_expr
val synth_fexp_simple' :
  bool ->
  (A.decl * 'a) list ->
  A.context ->
  A.potential ->
  A.parsed_expr ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode ->
  bool ->
  probmode -> A.context * A.potential * A.func_tp * A.ext A.func_aug_expr
val check_fexp_simple :
  bool ->
  (A.decl * 'a) list ->
  A.context ->
  A.potential ->
  A.parsed_expr ->
  PP.A.func_tp ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> bool -> probmode -> A.context * A.potential * A.parsed_expr
val synth_fexp_simple :
  bool ->
  (A.decl * 'a) list ->
  A.context ->
  A.potential ->
  A.parsed_expr ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode ->
  bool ->
  probmode -> A.context * A.potential * A.func_tp * A.ext A.func_aug_expr
val checkfexp :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  PP.A.potential ->
  A.ext A.func_aug_expr ->
  A.chan * PP.A.stype ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> probmode -> A.ext A.func_aug_expr
val check_exp' :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  PP.A.potential ->
  A.ext A.st_aug_expr ->
  A.chan * PP.A.stype ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> probmode -> A.ext A.st_aug_expr
val check_exp :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  PP.A.potential ->
  A.ext A.st_aug_expr ->
  A.chan * PP.A.stype ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> probmode -> A.ext A.st_aug_expr
val check_pbranchesR :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  ((int * int) * (int * int) * string) option A.pbranches ->
  A.chan ->
  A.pchoices ->
  A.ext ->
  PP.A.mode ->
  probmode ->
  (A.label * PP.A.probability * A.context) list * A.potential *
  A.ext A.pbranches
val check_pbranchesL :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  A.chan ->
  A.pchoices ->
  ((int * int) * (int * int) * string) option A.pbranches ->
  A.chan * PP.A.stype ->
  A.ext ->
  PP.A.mode ->
  probmode ->
  (A.label * PP.A.probability * A.context) list * A.potential *
  (A.label * PP.A.probability * PP.A.stype) list * A.ext A.pbranches
val check_branchesR :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  PP.A.potential ->
  ((int * int) * (int * int) * string) option A.branches ->
  A.chan -> A.choices -> A.ext -> PP.A.mode -> probmode -> A.ext A.branches
val check_branchesL :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  A.chan ->
  A.choices ->
  PP.A.potential ->
  ((int * int) * (int * int) * string) option A.branches ->
  A.chan * PP.A.stype -> A.ext -> PP.A.mode -> probmode -> A.ext A.branches
val checkexp :
  bool ->
  (A.decl * 'a) list ->
  PP.A.context ->
  PP.A.potential ->
  A.ext A.st_aug_expr ->
  A.chan * PP.A.stype ->
  ((int * int) * (int * int) * string) option ->
  PP.A.mode -> probmode -> A.ext A.st_aug_expr
val find_tp :
  'a * 'b * 'c ->
  (('d * 'b * 'e) * 'f) list -> (('d * 'b * 'e) * 'g) list -> 'd * 'b * 'e
val consistent_mode :
  'a ->
  ((E.PP.A.str * string * E.PP.A.mode) * 'b) list ->
  ((E.PP.A.str * string * E.PP.A.mode) * 'c) list ->
  A.argument list -> ((int * int) * (int * int) * string) option -> unit
val mode_P_list : (('a * 'b * A.mode) * 'c) list -> bool
val pure :
  'a ->
  string ->
  A.context ->
  PP.A.str * string * PP.A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val mode_S_list : (('a * 'b * A.mode) * 'c) list -> bool
val shared :
  'a ->
  string ->
  A.context ->
  PP.A.str * string * PP.A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val mode_lin_list : (('a * 'b * A.mode) * 'c) list -> bool
val transaction :
  'a ->
  string ->
  A.context ->
  PP.A.str * string * PP.A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val sum_prob : A.stype -> unit
val sum_prob_choices : A.choices -> unit
val sum_prob_pchoices : A.pchoices -> A.potential
