module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
val error : ((int * int) * (int * int) * string) option -> string -> 'a
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
val check_tp : 'a -> ('a * 'b) list -> bool
val find_tp : 'a -> ('a * 'b) list -> 'b
val match_ctx :
  A.decl_ext list ->
  ('a * A.stype) list ->
  string list ->
  (string * A.stype) list ->
  int -> int -> ((int * int) * (int * int) * string) option -> unit
val remove_tp : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_tps : 'a list -> ('a * 'b) list -> ('a * 'b) list
val consume_chans :
  string list ->
  (string * 'a) list -> ((int * int) * (int * int) * string) option -> unit
val no_dups :
  string list -> ((int * int) * (int * int) * string) option -> unit
val check_exp' :
  bool ->
  A.decl_ext list ->
  (string * PP.A.stype) list ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
val check_exp :
  bool ->
  A.decl_ext list ->
  (string * PP.A.stype) list ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
val check_branchesR :
  bool ->
  A.decl_ext list ->
  (string * PP.A.stype) list ->
  PP.A.R.arith ->
  A.branches ->
  string -> A.choices -> ((int * int) * (int * int) * string) option -> unit
val check_branchesL :
  bool ->
  A.decl_ext list ->
  (string * PP.A.stype) list ->
  A.chan ->
  A.choices ->
  PP.A.R.arith ->
  A.branches ->
  string * PP.A.stype -> ((int * int) * (int * int) * string) option -> unit
val checkexp :
  bool ->
  A.decl_ext list ->
  (string * PP.A.stype) list ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
