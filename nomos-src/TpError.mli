module A = Ast
module PP = Pprint
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val error_unknown_var :
  PP.A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_unknown_var_right :
  PP.A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_unknown_var_ctx :
  PP.A.str * string * 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_undeclared :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_implicit : 'a -> ((int * int) * (int * int) * string) option -> 'b
val error_label_missing_alt :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_label_invalid :
  'a ->
  string * PP.A.stype * (PP.A.str * string * PP.A.mode) ->
  ((int * int) * (int * int) * string) option -> 'b
val error_label_mismatch :
  string * string -> ((int * int) * (int * int) * string) option -> 'a
val error_label_missing_branch :
  string -> ((int * int) * (int * int) * string) option -> 'a
val error_potstar : unit -> ((int * int) * (int * int) * string) option -> 'a
val error_mode_mismatch :
  (PP.A.str * string * PP.A.mode) * (PP.A.str * string * PP.A.mode) ->
  ((int * int) * (int * int) * string) option -> 'a
val error_mode_shared_comm :
  PP.A.str * string * PP.A.mode ->
  ((int * int) * (int * int) * string) option -> 'a
