module A = Ast
module PP = Pprint
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val error_unknown_var :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_unknown_var_right :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_unknown_var_ctx :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_undeclared :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_implicit : 'a * ((int * int) * (int * int) * string) option -> 'b
val error_label_missing_alt :
  string * ((int * int) * (int * int) * string) option -> 'a
val error_label_invalid :
  'a ->
  string * PP.A.stype * string * ((int * int) * (int * int) * string) option ->
  'b
val error_label_mismatch :
  string * string * ((int * int) * (int * int) * string) option -> 'a
val error_label_missing_branch :
  string * ((int * int) * (int * int) * string) option -> 'a
