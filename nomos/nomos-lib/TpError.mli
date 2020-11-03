module A = Ast
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val error_unknown_var :
  A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_unknown_var_right :
  A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_unknown_var_ctx :
  A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_undeclared :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_implicit : 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_label_missing_alt :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_label_invalid :
  'a ->
  string * A.stype * (A.str * string * A.mode) ->
  ((int * int) * (int * int) * string) option -> 'b
val error_label_mismatch :
  string * string -> ((int * int) * (int * int) * string) option -> 'a
val error_label_missing_branch :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_potstar : unit -> ((int * int) * (int * int) * string) option -> 'a
val error_mode_mismatch :
  (A.str * string * A.mode) * (A.str * string * A.mode) ->
  ((int * int) * (int * int) * string) option -> 'a
val error_mode_shared_comm :
  A.str * string * A.mode ->
  ((int * int) * (int * int) * string) option -> 'a
