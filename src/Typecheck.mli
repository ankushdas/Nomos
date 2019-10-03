module R = Arith
module A = Ast
module PP = Pprint
module E = TpError

val esync :
  A.decl_ext list ->
  A.tpname list ->
  PP.A.stype ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> bool -> unit
val esync_tp :
  A.decl_ext list ->
  PP.A.stype -> ((int * int) * (int * int) * string) option -> unit
type polarity = Pos | Neg | Zero
val valid :
  A.decl_ext list ->
  polarity -> A.stype -> ((int * int) * (int * int) * string) option -> unit
val contractive : A.stype -> bool
val eqtp : A.decl_ext list -> A.stype -> A.stype -> bool
exception UnknownTypeError
val checkexp :
  bool ->
  A.decl_ext list ->
  PP.A.context ->
  PP.A.R.arith ->
  PP.A.expression -> string * PP.A.stype -> Mark.ext option -> unit
