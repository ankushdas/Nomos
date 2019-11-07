module A = Ast
module PP = Pprint
val error : string -> 'a
val error_unknown_var : PP.A.str * string * 'a -> 'b
val error_unknown_var_right : PP.A.str * string * 'a -> 'b
val error_unknown_var_ctx : PP.A.str * string * 'a -> 'b
val error_undeclared : string -> 'a
val error_implicit : 'a -> 'b
val error_label_missing_alt : string -> 'a
val error_label_invalid :
  'a -> string * PP.A.stype * (PP.A.str * string * PP.A.mode) -> 'b
val error_label_mismatch : string * string -> 'a
val error_label_missing_branch : string -> 'a
val error_potstar : 'a
val error_mode_mismatch :
  (PP.A.str * string * PP.A.mode) * (PP.A.str * string * PP.A.mode) -> 'a
val error_mode_shared_comm : PP.A.str * string * PP.A.mode -> 'a
