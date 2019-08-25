(* Interface for Mark *)

type ext = (int * int) * (int * int) * string
val show : (int * int) * (int * int) * string -> string
val show_source : (int * int) * (int * int) * string -> string
type 'a marked = 'a * ext option
val mark' : 'a * 'b -> 'a * 'b
val data : 'a * 'b -> 'a
val ext : 'a * 'b -> 'b
