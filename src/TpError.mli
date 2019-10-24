(* Interface for TpError *)

module A = Ast
module PP = Pprint
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val error_unknown_var :
  A.chan * ((int * int) * (int * int) * string) option -> 'a
val error_unknown_var_right :
  A.chan * ((int * int) * (int * int) * string) option -> 'a
val error_unknown_var_ctx :
  A.chan * ((int * int) * (int * int) * string) option -> 'a
val error_undeclared :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_implicit : 'a * ((int * int) * (int * int) * string) option -> 'b
val error_label_missing_alt :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_label_invalid :
  'a ->
  string * PP.A.stype * A.chan * ((int * int) * (int * int) * string) option ->
  'b
val error_label_mismatch :
  string * string * ((int * int) * (int * int) * string) option -> 'a
val error_label_missing_branch :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_potstar :
  ((int * int) * (int * int) * string) option -> 'a
val error_mode_mismatch :
  A.chan * A.chan * ((int * int) * (int * int) * string) option -> 'a
val error_mode_shared_comm :
  A.chan * ((int * int) * (int * int) * string) option -> 'a