module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val esync : A.decl_ext list -> A.tpname list -> A.stype -> 'a -> bool
val esync_choices :
  A.decl_ext list -> A.tpname list -> A.choices -> 'a -> bool
type polarity = Pos | Neg | Zero
val valid :
  A.decl_ext list ->
  polarity -> A.stype -> ((int * int) * (int * int) * string) option -> unit
val valid_choice :
  A.decl_ext list ->
  polarity ->
  A.choices -> ((int * int) * (int * int) * string) option -> unit
val contractive : A.stype -> bool
val mem_env : A.decl_ext list -> A.tpname -> A.tpname -> bool
val mem_seen :
  A.decl_ext list ->
  (A.tpname * A.tpname) list -> A.tpname -> A.tpname -> bool
val eq_tp' :
  A.decl_ext list -> (A.tpname * A.tpname) list -> A.stype -> A.stype -> bool
val eq_tp :
  A.decl_ext list -> (A.tpname * A.tpname) list -> A.stype -> A.stype -> bool
val eq_choice :
  A.decl_ext list ->
  (A.tpname * A.tpname) list -> A.choices -> A.choices -> bool
val eq_name_name :
  A.decl_ext list ->
  (A.tpname * A.tpname) list -> A.tpname -> A.tpname -> bool
val eqtp : A.decl_ext list -> A.stype -> A.stype -> bool
exception UnknownTypeError
val is_tpname : A.stype -> bool
val zero : R.arith
val chan_of : 'a * 'b -> 'a
val tp_of : 'a * 'b -> 'b
val checktp : 'a -> ('a * 'b) list -> bool
val check_tp : A.chan -> A.context -> bool
val check_stp : A.chan -> A.context -> bool
val check_ltp : A.chan -> A.context -> bool
val findtp : 'a -> ('a * 'b) list -> 'b
val find_stp : A.chan -> A.context -> A.stype
val find_ltp : A.chan -> A.context -> A.stype
val removetp : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_tp : A.chan -> A.context -> A.context
val match_ctx :
  A.decl_ext list ->
  ('a * A.stype) list ->
  A.chan list ->
  A.context ->
  int -> int -> ((int * int) * (int * int) * string) option -> A.context
val join : A.context -> A.chan_tp list
val add_chan : A.decl_ext list -> A.chan * A.stype -> A.context -> A.context
val check_exp' :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
val check_exp :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
val check_branchesR :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  PP.A.R.arith ->
  A.branches ->
  string -> A.choices -> ((int * int) * (int * int) * string) option -> unit
val check_branchesL :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  A.chan ->
  A.choices ->
  PP.A.R.arith ->
  A.branches ->
  string * PP.A.stype -> ((int * int) * (int * int) * string) option -> unit
val checkexp :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
