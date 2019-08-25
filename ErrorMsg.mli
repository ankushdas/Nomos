(* Interface for ErrorMsg *)

val anyErrors : bool ref
val reset : unit -> unit
type error_cat = Lex | Parse | Type | Pragma
val error_msg :
  error_cat -> ((int * int) * (int * int) * string) option -> string -> unit
val warn :
  error_cat -> ((int * int) * (int * int) * string) option -> string -> unit
exception Error
val error :
  error_cat -> ((int * int) * (int * int) * string) option -> string -> 'a
